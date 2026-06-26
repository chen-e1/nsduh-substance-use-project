# =====================================================
# STEPWISE LOGISTIC REGRESSION ANALYSIS
# Research Question:
# Which socio-economic factors are associated with
# continued drug use among individuals with a history
# of drug use?
# =====================================================

library(dplyr)
library(MASS)

# =====================================================
# Variables included in every model
# =====================================================
# EDUHIGHCAT                = Education
# ECONOMIC_STATUS_INDEX     = Economic status
# EMPLOYMENT_INDEX          = Employment status
# ECONOMIC_ASSISTANCE_INDEX = Government/economic assistance
# MENTAL_HEALTH_INDEX       = Mental health burden
# IRINSUR4                  = Health insurance

predictors <- c(
  "EDUHIGHCAT",
  "ECONOMIC_STATUS_INDEX",
  "EMPLOYMENT_INDEX",
  "ECONOMIC_ASSISTANCE_INDEX",
  "MENTAL_HEALTH_INDEX",
  "IRINSUR4"
)

# =====================================================
# MODEL 1 - HEROIN USERS
# =====================================================
# Population:
# Everyone who has ever used heroin.
#
# Outcome:
# Y_HEROIN = 1 -> Used heroin in the last 30 days
# Y_HEROIN = 0 -> Used heroin in the past but NOT in
#                 the last 30 days
#
# Goal:
# Identify factors associated with continued heroin use.
# =====================================================

heroin_data <- data %>%
  filter(!is.na(Y_HEROIN)) %>%
  dplyr::select(
    Y_HEROIN,
    all_of(predictors)
  ) %>%
  na.omit()

cat("Model 1 sample size:", nrow(heroin_data), "\n")

full_model_heroin <- glm(
  Y_HEROIN ~ .,
  family = binomial,
  data = heroin_data
)

step_model_heroin <- stepAIC(
  full_model_heroin,
  direction = "both",
  trace = TRUE
)

summary(step_model_heroin)

# =====================================================
# MODEL 2 - HEROIN OR COCAINE USERS
# =====================================================
# Population:
# Everyone who has ever used heroin OR cocaine.
#
# Outcome:
# Y_MODEL = 1 -> Still uses heroin and/or cocaine
#                (during the last 30 days)
#
# Y_MODEL = 0 -> Used heroin/cocaine in the past
#                but currently uses neither
#
# Goal:
# Determine whether the same protective factors remain
# important when the sample is expanded.
# =====================================================

heroin_cocaine_data <- data %>%
  filter(
    !is.na(Y_HEROIN) |
      !is.na(Y_COCAINE)
  ) %>%
  mutate(
    Y_MODEL = ifelse(
      Y_HEROIN == 1 |
        Y_COCAINE == 1,
      1,
      0
    )
  ) %>%
  dplyr::select(
    Y_MODEL,
    all_of(predictors)
  ) %>%
  na.omit()

cat("Model 2 sample size:", nrow(heroin_cocaine_data), "\n")

full_model_hc <- glm(
  Y_MODEL ~ .,
  family = binomial,
  data = heroin_cocaine_data
)

step_model_hc <- stepAIC(
  full_model_hc,
  direction = "both",
  trace = TRUE
)

summary(step_model_hc)

# =====================================================
# MODEL 3 - HEROIN OR COCAINE OR METH USERS
# =====================================================
# Population:
# Everyone who has ever used heroin, cocaine or meth.
#
# Outcome:
# Y_MODEL = 1 -> Still uses at least one of the drugs
#                during the last 30 days
#
# Y_MODEL = 0 -> Used one of the drugs in the past
#                but currently uses none of them
#
# Goal:
# Examine whether the same predictors remain important
# when the broadest drug-user population is analyzed.
# =====================================================

all_drugs_data <- data %>%
  filter(
    !is.na(Y_HEROIN) |
      !is.na(Y_COCAINE) |
      !is.na(Y_METH)
  ) %>%
  mutate(
    Y_MODEL = ifelse(
      Y_HEROIN == 1 |
        Y_COCAINE == 1 |
        Y_METH == 1,
      1,
      0
    )
  ) %>%
  dplyr::select(
    Y_MODEL,
    all_of(predictors)
  ) %>%
  na.omit()

cat("Model 3 sample size:", nrow(all_drugs_data), "\n")

full_model_all <- glm(
  Y_MODEL ~ .,
  family = binomial,
  data = all_drugs_data
)

step_model_all <- stepAIC(
  full_model_all,
  direction = "both",
  trace = TRUE
)

summary(step_model_all)

# =====================================================
# VISUALIZATION OF STEPWISE REGRESSION RESULTS
# =====================================================
# Goal:
# Visualize the final findings from the three logistic
# regression models and compare which socioeconomic
# factors remained important as the sample expanded:
#
# Model 1: Heroin Users
# Model 2: Heroin + Cocaine Users
# Model 3: Heroin + Cocaine + Methamphetamine Users
# =====================================================

# =====================================================
# Figure 1
# Variables Retained in Each Stepwise Model
# =====================================================

library(tidyverse)

survival_df <- data.frame(
  Variable = c(
    "Education",
    "Economic Status",
    "Employment",
    "Government Assistance",
    "Mental Health",
    "Health Insurance"
  ),
  Heroin = c(1,1,1,0,1,0),
  Heroin_Cocaine = c(0,0,0,1,1,1),
  All_Drugs = c(0,1,0,1,1,1)
)

survival_long <- survival_df %>%
  pivot_longer(
    cols = -Variable,
    names_to = "Model",
    values_to = "Selected"
  )

ggplot(
  survival_long,
  aes(Model, Variable, fill = factor(Selected))
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = ifelse(Selected == 1, "✓", ""))
  ) +
  scale_fill_manual(
    values = c(
      "0" = "white",
      "1" = "darkgreen"
    )
  ) +
  labs(
    title = "Variables Retained by Stepwise Regression",
    subtitle = "Green cells indicate variables retained in the final model",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


# =====================================================
# Figure 2
# Statistical Significance Across Models
# =====================================================

library(tidyverse)

pvals <- data.frame(
  Variable = c(
    "Education",
    "Economic Status",
    "Employment",
    "Government Assistance",
    "Mental Health",
    "Health Insurance"
  ),
  
  Heroin = c(
    0.002895,
    0.042740,
    0.014184,
    NA,
    0.018767,
    NA
  ),
  
  Heroin_Cocaine = c(
    NA,
    NA,
    NA,
    0.015801,
    0.089918,
    0.000159
  ),
  
  All_Drugs = c(
    NA,
    0.00329,
    NA,
    0.06084,
    0.00125,
    0.00471
  )
)

pvals_long <- pvals %>%
  pivot_longer(
    cols = -Variable,
    names_to = "Model",
    values_to = "p_value"
  ) %>%
  mutate(
    significance = -log10(p_value)
  )

ggplot(
  pvals_long,
  aes(
    x = Model,
    y = Variable,
    fill = significance
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = ifelse(
        is.na(p_value),
        "",
        sprintf("%.3f", p_value)
      )
    ),
    size = 4
  ) +
  labs(
    title = "Statistical Significance of Predictors",
    subtitle = "Cells display p-values; darker colors indicate stronger significance",
    x = "",
    y = "",
    fill = "-log10(p)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

# =====================================================
# MODEL PERFORMANCE EVALUATION
# EXPANDING SAMPLE MODELS
#
# Metrics:
# 1. AUC
# 2. McFadden's Pseudo R²
# 3. Brier Score
# =====================================================

library(pROC)
library(pscl)

# =====================================================
# MODEL 1 - HEROIN
# =====================================================

prob_heroin <- predict(
  step_model_heroin,
  type = "response"
)

roc_heroin <- roc(
  heroin_data$Y_HEROIN,
  prob_heroin
)

auc_heroin <- auc(roc_heroin)

r2_heroin <- pR2(step_model_heroin)

brier_heroin <- mean(
  (prob_heroin - heroin_data$Y_HEROIN)^2
)

cat("\n============================\n")
cat("MODEL 1 - HEROIN\n")
cat("============================\n")
cat("AUC =", auc_heroin, "\n")
cat("McFadden R² =", r2_heroin["McFadden"], "\n")
cat("Brier Score =", brier_heroin, "\n")

plot(
  roc_heroin,
  main = "ROC Curve - Heroin"
)

# =====================================================
# MODEL 2 - HEROIN + COCAINE
# =====================================================

prob_hc <- predict(
  step_model_hc,
  type = "response"
)

roc_hc <- roc(
  heroin_cocaine_data$Y_MODEL,
  prob_hc
)

auc_hc <- auc(roc_hc)

r2_hc <- pR2(step_model_hc)

brier_hc <- mean(
  (prob_hc - heroin_cocaine_data$Y_MODEL)^2
)

cat("\n============================\n")
cat("MODEL 2 - HEROIN + COCAINE\n")
cat("============================\n")
cat("AUC =", auc_hc, "\n")
cat("McFadden R² =", r2_hc["McFadden"], "\n")
cat("Brier Score =", brier_hc, "\n")

plot(
  roc_hc,
  main = "ROC Curve - Heroin + Cocaine"
)

# =====================================================
# MODEL 3 - ALL DRUGS
# =====================================================

prob_all <- predict(
  step_model_all,
  type = "response"
)

roc_all <- roc(
  all_drugs_data$Y_MODEL,
  prob_all
)

auc_all <- auc(roc_all)

r2_all <- pR2(step_model_all)

brier_all <- mean(
  (prob_all - all_drugs_data$Y_MODEL)^2
)

cat("\n============================\n")
cat("MODEL 3 - ALL DRUGS\n")
cat("============================\n")
cat("AUC =", auc_all, "\n")
cat("McFadden R² =", r2_all["McFadden"], "\n")
cat("Brier Score =", brier_all, "\n")

plot(
  roc_all,
  main = "ROC Curve - All Drugs"
)

# =====================================================
# SUMMARY TABLE
# =====================================================

results_expanding <- data.frame(
  Model = c(
    "Heroin",
    "Heroin + Cocaine",
    "All Drugs"
  ),
  AUC = c(
    as.numeric(auc_heroin),
    as.numeric(auc_hc),
    as.numeric(auc_all)
  ),
  McFadden_R2 = c(
    as.numeric(r2_heroin["McFadden"]),
    as.numeric(r2_hc["McFadden"]),
    as.numeric(r2_all["McFadden"])
  ),
  Brier_Score = c(
    brier_heroin,
    brier_hc,
    brier_all
  )
)

cat("\n============================\n")
cat("MODEL COMPARISON\n")
cat("============================\n")

print(results_expanding)
