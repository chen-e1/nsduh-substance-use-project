# ============================================================
# ROBUST hierarchical logistic regression - CURRENT USE outcomes
# Project: NSDUH hard/street drugs
#
# Research question, updated:
# Which socio-economic, mental-health and contextual factors are
# associated with current/recent use in the past 30 days among people
# with lifetime use history?
#
# Input expected in same folder:
#   nsduh_clean_with_indexes_geo_id.csv
#
# Outcomes used:
#   Y_HEROIN  = 1 if lifetime heroin use + heroin use in past 30 days;
#               0 if lifetime heroin use but no heroin use in past 30 days.
#   Y_COCAINE = same logic for cocaine.
#   Y_METH    = same logic for methamphetamine.
#
# Combined outcomes created safely with coalesce():
#   Y_HC  = current use of heroin OR cocaine among lifetime heroin/cocaine users
#   Y_HCM = current use of heroin OR cocaine OR meth among lifetime hard-drug users
#
# This script:
#   1. Reads the new indexed data file.
#   2. Checks required variables.
#   3. Builds combined outcomes and drug-profile variables.
#   4. Runs hierarchical logistic regression for three samples:
#        A. Heroin lifetime users
#        B. Heroin or cocaine lifetime users
#        C. Heroin, cocaine or meth lifetime users
#   5. Prints intermediate diagnostics.
#   6. Saves OR tables, model comparison tables, LR tests, pseudo-R2,
#      cross-validation, EPV checks and optional stepwise sensitivity results.
#
# Important interpretation:
#   This is NOT the old failed-to-stop model.
#   This is a current-use / recent-use model among lifetime users.
# ============================================================

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------
required_packages <- c("readr", "dplyr", "tibble")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them first, for example:\n",
      "install.packages(c(",
      paste0("'", missing_packages, "'", collapse = ", "),
      "))"
    )
  )
}

library(readr)
library(dplyr)
library(tibble)

has_MASS <- requireNamespace("MASS", quietly = TRUE)

# ------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------
input_file <- "data/nsduh_clean_with_indexes_geo_id.csv"
output_dir <- "outputs/hierarchical_logistic"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

run_stepwise_sensitivity <- TRUE
run_cross_validation <- TRUE
cv_k <- 5
cv_seed <- 123

# IMPORTANT:
# Run this script from the beginning with Source / source("current_use_hierarchical_logistic_ROBUST_v2_FIXED.R").
# Do not run only the validation/diagnostic blocks, because the model datasets are created inside the loop.

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------
section <- function(title) {
  cat("\n\n============================================================\n")
  cat(title, "\n")
  cat("============================================================\n")
}

subsection <- function(title) {
  cat("\n------------------------------------------------------------\n")
  cat(title, "\n")
  cat("------------------------------------------------------------\n")
}

safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

is_true_value <- function(x) {
  x %in% c(TRUE, 1, "1", "TRUE", "True", "true", "T", "t")
}

as_01 <- function(x) {
  dplyr::case_when(
    x %in% c(1, "1", TRUE, "TRUE", "True", "true", "T", "t") ~ 1,
    x %in% c(0, "0", 2, "2", FALSE, "FALSE", "False", "false", "F", "f") ~ 0,
    TRUE ~ NA_real_
  )
}

print_count_pct <- function(data, var, label = NULL) {
  if (is.null(label)) label <- var
  cat("\n", label, "\n", sep = "")

  out <- data %>%
    count(.data[[var]], name = "n") %>%
    mutate(percent = round(100 * n / sum(n), 1)) %>%
    arrange(desc(n)) %>%
    as_tibble()

  print(out, n = Inf)
  invisible(out)
}

print_cross_outcome <- function(data, var, outcome, label = NULL) {
  if (is.null(label)) label <- var
  cat("\n", label, " by outcome ", outcome, "\n", sep = "")

  out <- data %>%
    count(.data[[var]], .data[[outcome]], name = "n") %>%
    group_by(.data[[var]]) %>%
    mutate(percent_within_group = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    as_tibble()

  print(out, n = Inf)
  invisible(out)
}

make_or_table <- function(fit, model_name, analysis_name) {
  coef_matrix <- coef(summary(fit))
  ci <- suppressMessages(confint.default(fit))

  out <- tibble(
    analysis = analysis_name,
    model = model_name,
    term = rownames(coef_matrix),
    estimate_log_odds = coef_matrix[, "Estimate"],
    std_error = coef_matrix[, "Std. Error"],
    z_value = coef_matrix[, "z value"],
    p_value = coef_matrix[, "Pr(>|z|)"],
    conf_low_log_odds = ci[, 1],
    conf_high_log_odds = ci[, 2]
  ) %>%
    mutate(
      odds_ratio = exp(estimate_log_odds),
      conf_low_or = exp(conf_low_log_odds),
      conf_high_or = exp(conf_high_log_odds),
      odds_ratio_rounded = round(odds_ratio, 3),
      conf_low_or_rounded = round(conf_low_or, 3),
      conf_high_or_rounded = round(conf_high_or, 3),
      p_value_rounded = round(p_value, 4),
      significant_0_05 = if_else(!is.na(p_value) & p_value < 0.05, TRUE, FALSE)
    ) %>%
    select(
      analysis,
      model,
      term,
      odds_ratio,
      conf_low_or,
      conf_high_or,
      estimate_log_odds,
      std_error,
      z_value,
      p_value,
      odds_ratio_rounded,
      conf_low_or_rounded,
      conf_high_or_rounded,
      p_value_rounded,
      significant_0_05
    )

  out
}

make_fit_stats <- function(fit, model_name, analysis_name) {
  model_n <- nrow(stats::model.frame(fit))
  model_k <- length(stats::coef(fit))
  model_logLik <- as.numeric(stats::logLik(fit))
  model_AIC <- stats::AIC(fit)
  model_BIC_manual <- (-2 * model_logLik) + log(model_n) * model_k

  tibble(
    analysis = analysis_name,
    model = model_name,
    n = model_n,
    k_parameters = model_k,
    AIC = model_AIC,
    BIC_manual = model_BIC_manual,
    logLik = model_logLik,
    deviance = stats::deviance(fit),
    null_deviance = stats::deviance(update(fit, . ~ 1)),
    df_residual = stats::df.residual(fit)
  )
}

pseudo_r2 <- function(fit, analysis_name, model_name) {
  y <- model.response(model.frame(fit))
  p <- predict(fit, type = "response")

  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(update(fit, . ~ 1)))
  n <- nrow(model.frame(fit))
  k <- length(coef(fit))

  mcfadden <- 1 - (ll_full / ll_null)
  mcfadden_adj <- 1 - ((ll_full - k) / ll_null)
  cox_snell <- 1 - exp((2 / n) * (ll_null - ll_full))
  nagelkerke <- cox_snell / (1 - exp((2 / n) * ll_null))
  tjur <- mean(p[y == 1]) - mean(p[y == 0])
  brier <- mean((y - p)^2)

  tibble(
    analysis = analysis_name,
    model = model_name,
    n = n,
    k_parameters = k,
    logLik_full = ll_full,
    logLik_null = ll_null,
    McFadden_R2 = mcfadden,
    McFadden_adjusted_R2 = mcfadden_adj,
    Cox_Snell_R2 = cox_snell,
    Nagelkerke_R2 = nagelkerke,
    Tjur_R2 = tjur,
    Brier_score = brier
  )
}

auc_rank <- function(y, p) {
  y <- as.numeric(y)
  p <- as.numeric(p)

  pos <- p[y == 1]
  neg <- p[y == 0]

  if (length(pos) == 0 || length(neg) == 0) {
    return(NA_real_)
  }

  r <- rank(c(pos, neg))
  n_pos <- length(pos)
  n_neg <- length(neg)

  auc <- (sum(r[1:n_pos]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  return(auc)
}

make_stratified_folds <- function(y, k = 5, seed = 123) {
  set.seed(seed)
  folds <- rep(NA_integer_, length(y))

  for (class_value in unique(y)) {
    idx <- which(y == class_value)
    folds[idx] <- sample(rep(1:k, length.out = length(idx)))
  }

  folds
}

cv_logistic <- function(formula, data, outcome, k = 5, seed = 123, analysis_name, model_name) {
  folds <- make_stratified_folds(data[[outcome]], k = k, seed = seed)
  out <- vector("list", k)

  for (fold in 1:k) {
    train_data <- data[folds != fold, ]
    test_data <- data[folds == fold, ]

    fit <- tryCatch(
      glm(formula = formula, data = train_data, family = binomial(link = "logit")),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      out[[fold]] <- tibble(
        analysis = analysis_name,
        model = model_name,
        fold = fold,
        n_test = nrow(test_data),
        auc = NA_real_,
        brier = NA_real_,
        log_loss = NA_real_,
        accuracy = NA_real_,
        error_message = fit$message
      )
      next
    }

    pred_prob <- tryCatch(
      predict(fit, newdata = test_data, type = "response"),
      error = function(e) rep(NA_real_, nrow(test_data))
    )

    y_test <- test_data[[outcome]]
    valid <- !is.na(pred_prob) & !is.na(y_test)

    if (sum(valid) == 0) {
      out[[fold]] <- tibble(
        analysis = analysis_name,
        model = model_name,
        fold = fold,
        n_test = nrow(test_data),
        auc = NA_real_,
        brier = NA_real_,
        log_loss = NA_real_,
        accuracy = NA_real_,
        error_message = "No valid predictions"
      )
      next
    }

    pred_prob <- pred_prob[valid]
    y_test <- y_test[valid]

    pred_prob_clip <- pmin(pmax(pred_prob, 1e-15), 1 - 1e-15)

    log_loss <- -mean(
      y_test * log(pred_prob_clip) +
        (1 - y_test) * log(1 - pred_prob_clip)
    )

    brier <- mean((y_test - pred_prob)^2)
    auc <- auc_rank(y_test, pred_prob)
    pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
    accuracy <- mean(pred_class == y_test)

    out[[fold]] <- tibble(
      analysis = analysis_name,
      model = model_name,
      fold = fold,
      n_test = length(y_test),
      auc = auc,
      brier = brier,
      log_loss = log_loss,
      accuracy = accuracy,
      error_message = NA_character_
    )
  }

  bind_rows(out)
}

make_epv_table <- function(fit, data, outcome, analysis_name, model_name) {
  y <- data[[outcome]]
  n_total <- nrow(data)
  n_events <- sum(y == 1, na.rm = TRUE)
  n_nonevents <- sum(y == 0, na.rm = TRUE)
  k_params <- length(coef(fit))

  tibble(
    analysis = analysis_name,
    model = model_name,
    n_total = n_total,
    n_events = n_events,
    n_nonevents = n_nonevents,
    k_parameters = k_params,
    events_per_parameter = n_events / k_params,
    nonevents_per_parameter = n_nonevents / k_params
  )
}

fit_and_print <- function(model_name, model_formula, model_data, outcome, analysis_name) {
  section(paste("FITTING", analysis_name, "-", model_name))

  cat("\nFormula:\n")
  print(model_formula)

  cat("\nN used in this model data object:\n")
  print(nrow(model_data))

  cat("\nOutcome distribution in this model sample:\n")
  print(
    model_data %>%
      count(.data[[outcome]], name = "n") %>%
      mutate(percent = round(100 * n / sum(n), 1)) %>%
      as_tibble(),
    n = Inf
  )

  fit <- stats::glm(
    formula = model_formula,
    data = model_data,
    family = stats::binomial(link = "logit")
  )

  if (!inherits(fit, "glm")) {
    stop(paste("The fitted object for", model_name, "is not a glm object."))
  }

  subsection(paste(model_name, "- base R summary"))
  print(summary(fit))

  subsection(paste(model_name, "- odds ratios"))
  or_table <- make_or_table(fit, model_name, analysis_name)
  print(or_table, n = Inf)

  subsection(paste(model_name, "- fit statistics"))
  fit_stats <- make_fit_stats(fit, model_name, analysis_name)
  print(fit_stats, n = Inf)

  list(
    fit = fit,
    or_table = or_table,
    fit_stats = fit_stats
  )
}

# ------------------------------------------------------------
# 3. Read data
# ------------------------------------------------------------
section("READ DATA")

if (!file.exists(input_file)) {
  stop(paste0("Input file not found: ", input_file,
              "\nMake sure this R script is in the same folder as the CSV."))
}

df <- read_csv(input_file, show_col_types = FALSE)

cat("\nInput file:\n")
cat(input_file, "\n")

cat("\nRaw dimensions:\n")
print(dim(df))

cat("\nFirst 40 column names:\n")
print(head(names(df), 40))

# ------------------------------------------------------------
# 4. Required column check
# ------------------------------------------------------------
section("CHECK REQUIRED COLUMNS")

required_vars <- c(
  "Y_HEROIN",
  "Y_COCAINE",
  "Y_METH",
  "EDUHIGHCAT",
  "ECONOMIC_STATUS_INDEX",
  "EMPLOYMENT_INDEX",
  "ECONOMIC_ASSISTANCE_INDEX",
  "MENTAL_HEALTH_INDEX",
  "IRINSUR4",
  "CATAG3",
  "IRSEX",
  "SUTRTPY2",
  "BOOKED"
)

missing_vars <- setdiff(required_vars, names(df))

if (length(missing_vars) > 0) {
  stop(paste0("Missing required variables:\n", paste(missing_vars, collapse = ", ")))
} else {
  cat("\nAll required variables exist.\n")
}

# ------------------------------------------------------------
# 5. Create clean outcomes, profiles and model variables
# ------------------------------------------------------------
section("CREATE MODEL VARIABLES")

# NOTE:
# Y_HEROIN/Y_COCAINE/Y_METH are treated as:
#   1 = lifetime use + past-30-day use
#   0 = lifetime use + no past-30-day use
#   NA = no lifetime use / not relevant for that substance-specific outcome

analysis_df <- df %>%
  mutate(
    Y_HEROIN_MODEL = as_01(Y_HEROIN),
    Y_COCAINE_MODEL = as_01(Y_COCAINE),
    Y_METH_MODEL = as_01(Y_METH),

    ever_heroin_model = !is.na(Y_HEROIN_MODEL),
    ever_cocaine_model = !is.na(Y_COCAINE_MODEL),
    ever_meth_model = !is.na(Y_METH_MODEL),

    # Combined outcomes: coalesce avoids the FALSE | NA problem in R.
    Y_HC = if_else(
      ever_heroin_model | ever_cocaine_model,
      as.numeric(coalesce(Y_HEROIN_MODEL, 0) == 1 | coalesce(Y_COCAINE_MODEL, 0) == 1),
      NA_real_
    ),

    Y_HCM = if_else(
      ever_heroin_model | ever_cocaine_model | ever_meth_model,
      as.numeric(
        coalesce(Y_HEROIN_MODEL, 0) == 1 |
          coalesce(Y_COCAINE_MODEL, 0) == 1 |
          coalesce(Y_METH_MODEL, 0) == 1
      ),
      NA_real_
    ),

    drug_profile_hc = case_when(
      ever_heroin_model & !ever_cocaine_model ~ "Heroin only lifetime",
      !ever_heroin_model & ever_cocaine_model ~ "Cocaine only lifetime",
      ever_heroin_model & ever_cocaine_model ~ "Heroin + Cocaine lifetime",
      TRUE ~ NA_character_
    ),
    drug_profile_hc = factor(
      drug_profile_hc,
      levels = c("Cocaine only lifetime", "Heroin only lifetime", "Heroin + Cocaine lifetime")
    ),

    drug_profile_hcm = case_when(
      ever_heroin_model & !ever_cocaine_model & !ever_meth_model ~ "Heroin only lifetime",
      !ever_heroin_model & ever_cocaine_model & !ever_meth_model ~ "Cocaine only lifetime",
      !ever_heroin_model & !ever_cocaine_model & ever_meth_model ~ "Meth only lifetime",
      ever_heroin_model & ever_cocaine_model & !ever_meth_model ~ "Heroin + Cocaine lifetime",
      ever_heroin_model & !ever_cocaine_model & ever_meth_model ~ "Heroin + Meth lifetime",
      !ever_heroin_model & ever_cocaine_model & ever_meth_model ~ "Cocaine + Meth lifetime",
      ever_heroin_model & ever_cocaine_model & ever_meth_model ~ "All three lifetime",
      TRUE ~ NA_character_
    ),
    drug_profile_hcm = factor(
      drug_profile_hcm,
      levels = c(
        "Cocaine only lifetime",
        "Meth only lifetime",
        "Heroin only lifetime",
        "Cocaine + Meth lifetime",
        "Heroin + Cocaine lifetime",
        "Heroin + Meth lifetime",
        "All three lifetime"
      )
    ),

    drug_count_lifetime_hcm = as.numeric(ever_heroin_model) +
      as.numeric(ever_cocaine_model) +
      as.numeric(ever_meth_model),
    drug_count_lifetime_hcm_f = factor(
      drug_count_lifetime_hcm,
      levels = c(1, 2, 3),
      labels = c("One lifetime substance", "Two lifetime substances", "Three lifetime substances")
    ),

    EDUHIGHCAT_f = factor(EDUHIGHCAT),

    IRINSUR4_f = case_when(
      IRINSUR4 == 1 ~ "Has health insurance",
      IRINSUR4 == 2 ~ "No health insurance",
      TRUE ~ NA_character_
    ),
    IRINSUR4_f = factor(IRINSUR4_f, levels = c("Has health insurance", "No health insurance")),

    CATAG3_f = case_when(
      CATAG3 == 2 ~ "18-25",
      CATAG3 == 3 ~ "26-34",
      CATAG3 == 4 ~ "35-49",
      CATAG3 == 5 ~ "50+",
      TRUE ~ NA_character_
    ),
    CATAG3_f = factor(CATAG3_f, levels = c("18-25", "26-34", "35-49", "50+")),

    IRSEX_f = case_when(
      IRSEX == 1 ~ "Male",
      IRSEX == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    IRSEX_f = factor(IRSEX_f, levels = c("Male", "Female")),

    SUTRTPY2_f = case_when(
      SUTRTPY2 == 1 ~ "Received substance use treatment",
      SUTRTPY2 == 2 ~ "No substance use treatment",
      SUTRTPY2 == 0 ~ "No substance use treatment",
      TRUE ~ NA_character_
    ),
    SUTRTPY2_f = factor(
      SUTRTPY2_f,
      levels = c("No substance use treatment", "Received substance use treatment")
    ),

    BOOKED_clean_f = case_when(
      BOOKED %in% c(1, 3) ~ "Ever booked/arrested",
      BOOKED == 2 ~ "No booking/arrest record",
      TRUE ~ NA_character_
    ),
    BOOKED_clean_f = factor(
      BOOKED_clean_f,
      levels = c("No booking/arrest record", "Ever booked/arrested")
    ),

    ECONOMIC_STATUS_INDEX = as.numeric(ECONOMIC_STATUS_INDEX),
    EMPLOYMENT_INDEX = as.numeric(EMPLOYMENT_INDEX),
    ECONOMIC_ASSISTANCE_INDEX = as.numeric(ECONOMIC_ASSISTANCE_INDEX),
    MENTAL_HEALTH_INDEX = as.numeric(MENTAL_HEALTH_INDEX)
  )

# ------------------------------------------------------------
# 6. Initial diagnostics
# ------------------------------------------------------------
section("INITIAL OUTCOME AND SAMPLE DIAGNOSTICS")

cat("\nOutcome definitions:\n")
cat("Y=1: lifetime use + past-30-day use.\n")
cat("Y=0: lifetime use + no past-30-day use.\n")
cat("NA: no lifetime use for that substance / not in denominator.\n")

print_count_pct(analysis_df, "Y_HEROIN_MODEL", "Y_HEROIN_MODEL distribution")
print_count_pct(analysis_df, "Y_COCAINE_MODEL", "Y_COCAINE_MODEL distribution")
print_count_pct(analysis_df, "Y_METH_MODEL", "Y_METH_MODEL distribution")
print_count_pct(analysis_df, "Y_HC", "Y_HC combined heroin/cocaine distribution")
print_count_pct(analysis_df, "Y_HCM", "Y_HCM combined heroin/cocaine/meth distribution")

subsection("Drug profile distributions")
print_count_pct(analysis_df, "drug_profile_hc", "drug_profile_hc")
print_count_pct(analysis_df, "drug_profile_hcm", "drug_profile_hcm")
print_count_pct(analysis_df, "drug_count_lifetime_hcm_f", "drug_count_lifetime_hcm_f")

subsection("Outcome by drug profile")
print_cross_outcome(
  analysis_df %>% filter(!is.na(Y_HC), !is.na(drug_profile_hc)),
  "drug_profile_hc",
  "Y_HC",
  "drug_profile_hc"
)
print_cross_outcome(
  analysis_df %>% filter(!is.na(Y_HCM), !is.na(drug_profile_hcm)),
  "drug_profile_hcm",
  "Y_HCM",
  "drug_profile_hcm"
)

subsection("Predictor summaries")
cat("\nEDUHIGHCAT_f:\n")
print_count_pct(analysis_df, "EDUHIGHCAT_f", "Education")
cat("\nIRINSUR4_f:\n")
print_count_pct(analysis_df, "IRINSUR4_f", "Health insurance")
cat("\nCATAG3_f:\n")
print_count_pct(analysis_df, "CATAG3_f", "Age group")
cat("\nIRSEX_f:\n")
print_count_pct(analysis_df, "IRSEX_f", "Sex")
cat("\nSUTRTPY2_f:\n")
print_count_pct(analysis_df, "SUTRTPY2_f", "Substance use treatment")
cat("\nBOOKED_clean_f:\n")
print_count_pct(analysis_df, "BOOKED_clean_f", "Booked/arrested")

cat("\nIndex variable summaries:\n")
print(summary(analysis_df[, c(
  "ECONOMIC_STATUS_INDEX",
  "EMPLOYMENT_INDEX",
  "ECONOMIC_ASSISTANCE_INDEX",
  "MENTAL_HEALTH_INDEX"
)]))

# ------------------------------------------------------------
# 7. Analysis definitions
# ------------------------------------------------------------
section("DEFINE ANALYSES")

# Theoretical blocks:
#   Baseline: drug profile for combined models; intercept-only for heroin-only.
#   SES block: education, economic status, employment, economic assistance, insurance.
#   Demographic block: age and sex.
#   Mental-health block: mental-health index.
#   Context/severity block: substance-use treatment and booked/arrested.

analyses <- list(
  list(
    analysis_name = "A_heroin_lifetime_users",
    label = "Heroin lifetime users",
    data_filter_expr = quote(!is.na(Y_HEROIN_MODEL)),
    outcome = "Y_HEROIN_MODEL",
    profile_var = NULL
  ),
  list(
    analysis_name = "B_heroin_or_cocaine_lifetime_users",
    label = "Heroin or cocaine lifetime users",
    data_filter_expr = quote(!is.na(Y_HC)),
    outcome = "Y_HC",
    profile_var = "drug_profile_hc"
  ),
  list(
    analysis_name = "C_heroin_cocaine_meth_lifetime_users",
    label = "Heroin, cocaine or meth lifetime users",
    data_filter_expr = quote(!is.na(Y_HCM)),
    outcome = "Y_HCM",
    profile_var = "drug_profile_hcm"
  )
)

make_sequence_formulas <- function(outcome, profile_var = NULL) {
  base_terms <- if (is.null(profile_var)) {
    "1"
  } else {
    profile_var
  }

  m0 <- as.formula(paste(outcome, "~", base_terms))

  m1 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "EDUHIGHCAT_f",
      "ECONOMIC_STATUS_INDEX",
      "EMPLOYMENT_INDEX",
      "ECONOMIC_ASSISTANCE_INDEX",
      "IRINSUR4_f"
    ), collapse = " + ")
  ))

  m2 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "EDUHIGHCAT_f",
      "ECONOMIC_STATUS_INDEX",
      "EMPLOYMENT_INDEX",
      "ECONOMIC_ASSISTANCE_INDEX",
      "IRINSUR4_f",
      "CATAG3_f",
      "IRSEX_f"
    ), collapse = " + ")
  ))

  m3 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "EDUHIGHCAT_f",
      "ECONOMIC_STATUS_INDEX",
      "EMPLOYMENT_INDEX",
      "ECONOMIC_ASSISTANCE_INDEX",
      "IRINSUR4_f",
      "CATAG3_f",
      "IRSEX_f",
      "MENTAL_HEALTH_INDEX"
    ), collapse = " + ")
  ))

  m4 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "EDUHIGHCAT_f",
      "ECONOMIC_STATUS_INDEX",
      "EMPLOYMENT_INDEX",
      "ECONOMIC_ASSISTANCE_INDEX",
      "IRINSUR4_f",
      "CATAG3_f",
      "IRSEX_f",
      "MENTAL_HEALTH_INDEX",
      "SUTRTPY2_f",
      "BOOKED_clean_f"
    ), collapse = " + ")
  ))

  list(
    "Model 0 - baseline" = m0,
    "Model 1 - add SES indexes" = m1,
    "Model 2 - add demographics" = m2,
    "Model 3 - add mental health" = m3,
    "Model 4 - add treatment and booked" = m4
  )
}

make_sensitivity_formulas <- function(outcome, profile_var = NULL) {
  # Sensitivity order: baseline -> mental -> treatment/booked -> demographics -> SES last
  base_terms <- if (is.null(profile_var)) {
    "1"
  } else {
    profile_var
  }

  s0 <- as.formula(paste(outcome, "~", base_terms))

  s1 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "MENTAL_HEALTH_INDEX"
    ), collapse = " + ")
  ))

  s2 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "MENTAL_HEALTH_INDEX",
      "SUTRTPY2_f",
      "BOOKED_clean_f"
    ), collapse = " + ")
  ))

  s3 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "MENTAL_HEALTH_INDEX",
      "SUTRTPY2_f",
      "BOOKED_clean_f",
      "CATAG3_f",
      "IRSEX_f"
    ), collapse = " + ")
  ))

  s4 <- as.formula(paste(
    outcome, "~", paste(c(
      if (!is.null(profile_var)) profile_var else NULL,
      "MENTAL_HEALTH_INDEX",
      "SUTRTPY2_f",
      "BOOKED_clean_f",
      "CATAG3_f",
      "IRSEX_f",
      "EDUHIGHCAT_f",
      "ECONOMIC_STATUS_INDEX",
      "EMPLOYMENT_INDEX",
      "ECONOMIC_ASSISTANCE_INDEX",
      "IRINSUR4_f"
    ), collapse = " + ")
  ))

  list(
    "Sensitivity 0 - baseline" = s0,
    "Sensitivity 1 - add mental health" = s1,
    "Sensitivity 2 - add treatment and booked" = s2,
    "Sensitivity 3 - add demographics" = s3,
    "Sensitivity 4 - add SES last" = s4
  )
}

# ------------------------------------------------------------
# 8. Run analyses
# ------------------------------------------------------------
all_or_results <- list()
all_model_comparisons <- list()
all_lr_tests <- list()
all_model_data_summary <- list()
all_epv <- list()
all_pseudo_r2 <- list()
all_cv_results <- list()
all_cv_summaries <- list()
all_sensitivity_comparisons <- list()
all_sensitivity_lr <- list()
all_stepwise_or <- list()
all_stepwise_fit_stats <- list()
all_stepwise_selected <- list()

for (a in analyses) {
  analysis_name <- a$analysis_name
  analysis_label <- a$label
  outcome <- a$outcome
  profile_var <- a$profile_var

  section(paste("START ANALYSIS:", analysis_label))

  analysis_data_raw <- analysis_df %>%
    filter(eval(a$data_filter_expr))

  cat("\nRows before complete-case filtering for this analysis:\n")
  print(nrow(analysis_data_raw))

  cat("\nOutcome distribution before complete-case filtering:\n")
  print(
    analysis_data_raw %>%
      count(.data[[outcome]], name = "n") %>%
      mutate(percent = round(100 * n / sum(n), 1)) %>%
      as_tibble(),
    n = Inf
  )

  if (!is.null(profile_var)) {
    cat("\nDrug profile distribution before complete-case filtering:\n")
    print_count_pct(analysis_data_raw, profile_var, profile_var)
    print_cross_outcome(analysis_data_raw, profile_var, outcome, profile_var)
  }

  final_model_vars <- c(
    outcome,
    if (!is.null(profile_var)) profile_var else NULL,
    "EDUHIGHCAT_f",
    "ECONOMIC_STATUS_INDEX",
    "EMPLOYMENT_INDEX",
    "ECONOMIC_ASSISTANCE_INDEX",
    "IRINSUR4_f",
    "CATAG3_f",
    "IRSEX_f",
    "MENTAL_HEALTH_INDEX",
    "SUTRTPY2_f",
    "BOOKED_clean_f"
  )

  subsection("Missingness before complete-case filtering")
  missingness <- tibble(
    analysis = analysis_name,
    variable = final_model_vars,
    missing_n = sapply(analysis_data_raw[final_model_vars], function(x) sum(is.na(x))),
    missing_percent = round(100 * sapply(analysis_data_raw[final_model_vars], function(x) mean(is.na(x))), 1)
  )
  print(missingness, n = Inf)

  analysis_data <- analysis_data_raw %>%
    filter(if_all(all_of(final_model_vars), ~ !is.na(.)))

  # Compatibility alias:
  # Some diagnostic/helper code from the previous project used the name `model_data`.
  # In this current-use script the analysis-specific dataset is called `analysis_data`.
  # This alias prevents `object 'model_data' not found` if a downstream block expects that name.
  model_data <- analysis_data

  cat("\nRows after complete-case filtering:\n")
  print(nrow(analysis_data))

  cat("\nRows removed due to missingness:\n")
  print(nrow(analysis_data_raw) - nrow(analysis_data))

  cat("\nOutcome distribution after complete-case filtering:\n")
  print(
    analysis_data %>%
      count(.data[[outcome]], name = "n") %>%
      mutate(percent = round(100 * n / sum(n), 1)) %>%
      as_tibble(),
    n = Inf
  )

  if (!is.null(profile_var)) {
    cat("\nDrug profile after complete-case filtering:\n")
    print_count_pct(analysis_data, profile_var, profile_var)
    print_cross_outcome(analysis_data, profile_var, outcome, profile_var)
  }

  # Save model-ready data for this analysis.
  write_csv(
    analysis_data,
    file.path(output_dir, paste0(safe_name(analysis_name), "_model_data_complete_cases.csv"))
  )

  all_model_data_summary[[analysis_name]] <- tibble(
    analysis = analysis_name,
    label = analysis_label,
    outcome = outcome,
    rows_before_complete_case = nrow(analysis_data_raw),
    rows_after_complete_case = nrow(analysis_data),
    rows_removed = nrow(analysis_data_raw) - nrow(analysis_data),
    n_events = sum(analysis_data[[outcome]] == 1, na.rm = TRUE),
    n_nonevents = sum(analysis_data[[outcome]] == 0, na.rm = TRUE),
    event_rate_percent = round(100 * mean(analysis_data[[outcome]] == 1, na.rm = TRUE), 2)
  )

  # Main hierarchical sequence.
  formulas <- make_sequence_formulas(outcome, profile_var)
  results <- list()

  for (model_name in names(formulas)) {
    results[[model_name]] <- fit_and_print(
      model_name = model_name,
      model_formula = formulas[[model_name]],
      model_data = analysis_data,
      outcome = outcome,
      analysis_name = analysis_name
    )
  }

  or_results <- bind_rows(lapply(results, function(x) x$or_table))
  model_comparison <- bind_rows(lapply(results, function(x) x$fit_stats))

  subsection("Main model comparison")
  print(model_comparison, n = Inf)

  fits_only <- lapply(results, function(x) x$fit)
  lr_tests_base <- do.call(stats::anova, c(unname(fits_only), list(test = "Chisq")))
  subsection("Main likelihood-ratio tests")
  print(lr_tests_base)

  lr_tests <- as.data.frame(lr_tests_base) %>%
    rownames_to_column("model_step") %>%
    as_tibble() %>%
    mutate(analysis = analysis_name, .before = model_step)

  all_or_results[[analysis_name]] <- or_results
  all_model_comparisons[[analysis_name]] <- model_comparison
  all_lr_tests[[analysis_name]] <- lr_tests

  write_csv(or_results, file.path(output_dir, paste0(safe_name(analysis_name), "_or_results.csv")))
  write_csv(model_comparison, file.path(output_dir, paste0(safe_name(analysis_name), "_model_comparison.csv")))
  write_csv(lr_tests, file.path(output_dir, paste0(safe_name(analysis_name), "_lr_tests.csv")))

  # Final/full model diagnostics.
  final_model_name <- names(results)[length(results)]
  final_fit <- results[[final_model_name]]$fit
  final_formula <- formulas[[final_model_name]]

  subsection("Events per parameter - final full model")
  epv_table <- make_epv_table(final_fit, analysis_data, outcome, analysis_name, final_model_name)
  print(epv_table)
  all_epv[[analysis_name]] <- epv_table

  subsection("Pseudo R-squared - final full model")
  pseudo_table <- pseudo_r2(final_fit, analysis_name, final_model_name)
  print(pseudo_table)
  all_pseudo_r2[[analysis_name]] <- pseudo_table

  if (run_cross_validation) {
    subsection("5-fold cross-validation - final full model")
    cv_results <- cv_logistic(
      formula = final_formula,
      data = analysis_data,
      outcome = outcome,
      k = cv_k,
      seed = cv_seed,
      analysis_name = analysis_name,
      model_name = final_model_name
    )
    print(cv_results, n = Inf)

    cv_summary <- cv_results %>%
      summarise(
        analysis = first(analysis),
        model = first(model),
        mean_auc = mean(auc, na.rm = TRUE),
        sd_auc = sd(auc, na.rm = TRUE),
        mean_brier = mean(brier, na.rm = TRUE),
        sd_brier = sd(brier, na.rm = TRUE),
        mean_log_loss = mean(log_loss, na.rm = TRUE),
        sd_log_loss = sd(log_loss, na.rm = TRUE),
        mean_accuracy = mean(accuracy, na.rm = TRUE),
        sd_accuracy = sd(accuracy, na.rm = TRUE),
        n_folds_with_errors = sum(!is.na(error_message))
      )

    cat("\nCV summary:\n")
    print(cv_summary)

    all_cv_results[[analysis_name]] <- cv_results
    all_cv_summaries[[analysis_name]] <- cv_summary

    write_csv(cv_results, file.path(output_dir, paste0(safe_name(analysis_name), "_cv_5fold_results.csv")))
    write_csv(cv_summary, file.path(output_dir, paste0(safe_name(analysis_name), "_cv_5fold_summary.csv")))
  }

  # Sensitivity: SES last.
  section(paste("ORDER SENSITIVITY:", analysis_label))
  sens_formulas <- make_sensitivity_formulas(outcome, profile_var)
  sens_fits <- list()

  for (model_name in names(sens_formulas)) {
    cat("\nFitting sensitivity model:", model_name, "\n")
    print(sens_formulas[[model_name]])
    sens_fits[[model_name]] <- glm(
      formula = sens_formulas[[model_name]],
      data = analysis_data,
      family = binomial(link = "logit")
    )
  }

  sens_comparison <- bind_rows(
    lapply(names(sens_fits), function(nm) make_fit_stats(sens_fits[[nm]], nm, analysis_name))
  )

  subsection("Sensitivity model comparison")
  print(sens_comparison, n = Inf)

  sens_lr_base <- do.call(stats::anova, c(unname(sens_fits), list(test = "Chisq")))
  subsection("Sensitivity likelihood-ratio tests")
  print(sens_lr_base)

  sens_lr <- as.data.frame(sens_lr_base) %>%
    rownames_to_column("model_step") %>%
    as_tibble() %>%
    mutate(analysis = analysis_name, .before = model_step)

  all_sensitivity_comparisons[[analysis_name]] <- sens_comparison
  all_sensitivity_lr[[analysis_name]] <- sens_lr

  write_csv(sens_comparison, file.path(output_dir, paste0(safe_name(analysis_name), "_order_sensitivity_model_comparison.csv")))
  write_csv(sens_lr, file.path(output_dir, paste0(safe_name(analysis_name), "_order_sensitivity_lr_tests.csv")))

  # Optional stepwise sensitivity, not main model.
  if (run_stepwise_sensitivity) {
    section(paste("STEPWISE SENSITIVITY:", analysis_label))

    if (!has_MASS) {
      cat("\nPackage MASS is not installed. Skipping stepwise sensitivity.\n")
    } else {
      full_step_formula <- final_formula

      cat("\nFull formula used as starting point for stepwise sensitivity:\n")
      print(full_step_formula)

      full_step_fit <- glm(
        formula = full_step_formula,
        data = analysis_data,
        family = binomial(link = "logit")
      )

      step_fit <- MASS::stepAIC(
        full_step_fit,
        direction = "both",
        trace = TRUE
      )

      subsection("Stepwise final summary")
      print(summary(step_fit))

      step_or <- make_or_table(step_fit, "Stepwise sensitivity final", analysis_name)
      step_fit_stats <- make_fit_stats(step_fit, "Stepwise sensitivity final", analysis_name)
      selected_terms <- tibble(
        analysis = analysis_name,
        selected_term = names(coef(step_fit))
      )

      subsection("Stepwise OR table")
      print(step_or, n = Inf)

      subsection("Stepwise fit statistics")
      print(step_fit_stats, n = Inf)

      subsection("Stepwise selected terms")
      print(selected_terms, n = Inf)

      all_stepwise_or[[analysis_name]] <- step_or
      all_stepwise_fit_stats[[analysis_name]] <- step_fit_stats
      all_stepwise_selected[[analysis_name]] <- selected_terms

      write_csv(step_or, file.path(output_dir, paste0(safe_name(analysis_name), "_stepwise_or_results.csv")))
      write_csv(step_fit_stats, file.path(output_dir, paste0(safe_name(analysis_name), "_stepwise_fit_stats.csv")))
      write_csv(selected_terms, file.path(output_dir, paste0(safe_name(analysis_name), "_stepwise_selected_terms.csv")))
    }
  }
}

# ------------------------------------------------------------
# 9. Save combined outputs
# ------------------------------------------------------------
section("SAVE COMBINED OUTPUTS")

combined_model_data_summary <- bind_rows(all_model_data_summary)
combined_or_results <- bind_rows(all_or_results)
combined_model_comparisons <- bind_rows(all_model_comparisons)
combined_lr_tests <- bind_rows(all_lr_tests)
combined_epv <- bind_rows(all_epv)
combined_pseudo_r2 <- bind_rows(all_pseudo_r2)
combined_sens_comparisons <- bind_rows(all_sensitivity_comparisons)
combined_sens_lr <- bind_rows(all_sensitivity_lr)
combined_cv_results <- if (length(all_cv_results) > 0) bind_rows(all_cv_results) else tibble()
combined_cv_summaries <- if (length(all_cv_summaries) > 0) bind_rows(all_cv_summaries) else tibble()
combined_stepwise_or <- if (length(all_stepwise_or) > 0) bind_rows(all_stepwise_or) else tibble()
combined_stepwise_fit_stats <- if (length(all_stepwise_fit_stats) > 0) bind_rows(all_stepwise_fit_stats) else tibble()
combined_stepwise_selected <- if (length(all_stepwise_selected) > 0) bind_rows(all_stepwise_selected) else tibble()

write_csv(combined_model_data_summary, file.path(output_dir, "ALL_model_data_summary.csv"))
write_csv(combined_or_results, file.path(output_dir, "ALL_or_results.csv"))
write_csv(combined_model_comparisons, file.path(output_dir, "ALL_model_comparisons.csv"))
write_csv(combined_lr_tests, file.path(output_dir, "ALL_lr_tests.csv"))
write_csv(combined_epv, file.path(output_dir, "ALL_events_per_parameter.csv"))
write_csv(combined_pseudo_r2, file.path(output_dir, "ALL_pseudo_r2.csv"))
write_csv(combined_sens_comparisons, file.path(output_dir, "ALL_order_sensitivity_model_comparisons.csv"))
write_csv(combined_sens_lr, file.path(output_dir, "ALL_order_sensitivity_lr_tests.csv"))

if (nrow(combined_cv_results) > 0) {
  write_csv(combined_cv_results, file.path(output_dir, "ALL_cv_5fold_results.csv"))
  write_csv(combined_cv_summaries, file.path(output_dir, "ALL_cv_5fold_summaries.csv"))
}

if (nrow(combined_stepwise_or) > 0) {
  write_csv(combined_stepwise_or, file.path(output_dir, "ALL_stepwise_or_results.csv"))
  write_csv(combined_stepwise_fit_stats, file.path(output_dir, "ALL_stepwise_fit_stats.csv"))
  write_csv(combined_stepwise_selected, file.path(output_dir, "ALL_stepwise_selected_terms.csv"))
}

# Save R objects.
model_run_objects <- list(
  input_file = input_file,
  output_dir = output_dir,
  analyses = analyses,
  model_data_summary = combined_model_data_summary,
  or_results = combined_or_results,
  model_comparisons = combined_model_comparisons,
  lr_tests = combined_lr_tests,
  events_per_parameter = combined_epv,
  pseudo_r2 = combined_pseudo_r2,
  cv_results = combined_cv_results,
  cv_summaries = combined_cv_summaries,
  order_sensitivity_model_comparisons = combined_sens_comparisons,
  order_sensitivity_lr_tests = combined_sens_lr,
  stepwise_or_results = combined_stepwise_or,
  stepwise_fit_stats = combined_stepwise_fit_stats,
  stepwise_selected_terms = combined_stepwise_selected
)

saveRDS(model_run_objects, file.path(output_dir, "ALL_model_run_objects.rds"))

cat("\nSaved combined outputs to folder: ", output_dir, "\n", sep = "")
cat("\nMain files to inspect first:\n")
cat("\n1. ALL_model_data_summary.csv")
cat("\n2. ALL_or_results.csv")
cat("\n3. ALL_model_comparisons.csv")
cat("\n4. ALL_lr_tests.csv")
cat("\n5. ALL_order_sensitivity_model_comparisons.csv")
cat("\n6. ALL_order_sensitivity_lr_tests.csv")
cat("\n7. ALL_cv_5fold_summaries.csv")
cat("\n8. ALL_stepwise_selected_terms.csv, if stepwise ran\n")

# ------------------------------------------------------------
# 10. Final interpretation reminders
# ------------------------------------------------------------
section("INTERPRETATION REMINDERS")

cat("\n1. This is a CURRENT USE model, not the old failed-to-stop model.\n")
cat("2. Y=1 means lifetime use plus past-30-day use.\n")
cat("3. Y=0 means lifetime use but no past-30-day use.\n")
cat("4. Combined outcomes use coalesce() so that true 0 values are not lost because of NA.\n")
cat("5. Hierarchical models are the main theory-driven models.\n")
cat("6. Stepwise results, if used, should be treated as sensitivity/exploratory evidence, not the main model.\n")
cat("7. Treatment and booked/arrested variables should be interpreted as correlates/severity/context markers, not causal effects.\n")
cat("8. OR values are odds ratios, not probability ratios.\n")
cat("\nDone.\n")
