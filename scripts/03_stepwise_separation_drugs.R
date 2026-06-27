# =====================================================
# STEPWISE LOGISTIC REGRESSION
# HEROIN USERS
# =====================================================

library(dplyr)
library(MASS)

if (!exists("data")) {
  data <- read.csv("data/nsduh_clean_with_indexes_geo_id.csv")
}

predictors <- c(
  "EDUHIGHCAT",
  "ECONOMIC_STATUS_INDEX",
  "EMPLOYMENT_INDEX",
  "ECONOMIC_ASSISTANCE_INDEX",
  "MENTAL_HEALTH_INDEX",
  "IRINSUR4"
)

# -----------------------------------------------------
# HEROIN
# -----------------------------------------------------

heroin_data <- data %>%
  filter(!is.na(Y_HEROIN)) %>%
  dplyr::select(
    Y_HEROIN,
    all_of(predictors)
  ) %>%
  na.omit()

cat("Heroin sample size:", nrow(heroin_data), "\n")

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

cat("AIC =", AIC(step_model_heroin), "\n")

# =====================================================
# COCAINE USERS
# =====================================================

cocaine_data <- data %>%
  filter(!is.na(Y_COCAINE)) %>%
  dplyr::select(
    Y_COCAINE,
    all_of(predictors)
  ) %>%
  na.omit()

cat("Cocaine sample size:", nrow(cocaine_data), "\n")

full_model_cocaine <- glm(
  Y_COCAINE ~ .,
  family = binomial,
  data = cocaine_data
)

step_model_cocaine <- stepAIC(
  full_model_cocaine,
  direction = "both",
  trace = TRUE
)

summary(step_model_cocaine)

cat("AIC =", AIC(step_model_cocaine), "\n")

# =====================================================
# METH USERS
# =====================================================

meth_data <- data %>%
  filter(!is.na(Y_METH)) %>%
  dplyr::select(
    Y_METH,
    all_of(predictors)
  ) %>%
  na.omit()

cat("Meth sample size:", nrow(meth_data), "\n")

full_model_meth <- glm(
  Y_METH ~ .,
  family = binomial,
  data = meth_data
)

step_model_meth <- stepAIC(
  full_model_meth,
  direction = "both",
  trace = TRUE
)

summary(step_model_meth)

cat("AIC =", AIC(step_model_meth), "\n")


# =====================================================
# FIGURE 1
# VARIABLES RETAINED BY STEPWISE REGRESSION
# DRUG-SPECIFIC MODELS
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
  
  Cocaine = c(1,1,1,0,1,1),
  
  Meth = c(1,1,1,1,1,1)
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
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = ifelse(
        Selected == 1,
        "✓",
        ""
      )
    ),
    size = 8
  ) +
  scale_fill_manual(
    values = c(
      "0" = "white",
      "1" = "darkgreen"
    )
  ) +
  labs(
    title = "Variables Retained by Stepwise Regression",
    subtitle = "Drug-Specific Models",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "none"
  )

# =====================================================
# FIGURE 2
# PREDICTOR SIGNIFICANCE HEATMAP
# BASED ON P-VALUES FROM THE FINAL STEPWISE MODELS
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
  
  Cocaine = c(
    0.007391,
    0.010420,
    0.050169,
    NA,
    0.0000004,
    0.000159
  ),
  
  Meth = c(
    0.014647,
    0.0000004,
    0.002039,
    0.022454,
    0.0000013,
    0.004708
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
  geom_tile(
    color = "white"
  ) +
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
    subtitle = "Drug-Specific Models",
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
# Metrics:
# 1. AUC
# 2. McFadden's Pseudo R²
# 3. Brier Score
# =====================================================

library(pROC)
library(pscl)

# =====================================================
# HEROIN MODEL
# =====================================================

# Predicted probabilities
prob_heroin <- predict(
  step_model_heroin,
  type = "response"
)

# ROC-AUC
roc_heroin <- roc(
  heroin_data$Y_HEROIN,
  prob_heroin
)

auc_heroin <- auc(roc_heroin)

cat("\n============================\n")
cat("HEROIN MODEL\n")
cat("============================\n")
cat("AUC =", auc_heroin, "\n")

# McFadden R²
r2_heroin <- pR2(step_model_heroin)

cat("McFadden R² =",
    r2_heroin["McFadden"],
    "\n")

# Brier Score
brier_heroin <- mean(
  (prob_heroin - heroin_data$Y_HEROIN)^2
)

cat("Brier Score =",
    brier_heroin,
    "\n")

# ROC Plot
plot(
  roc_heroin,
  main = "ROC Curve - Heroin"
)

# =====================================================
# COCAINE MODEL
# =====================================================

prob_cocaine <- predict(
  step_model_cocaine,
  type = "response"
)

roc_cocaine <- roc(
  cocaine_data$Y_COCAINE,
  prob_cocaine
)

auc_cocaine <- auc(roc_cocaine)

cat("\n============================\n")
cat("COCAINE MODEL\n")
cat("============================\n")
cat("AUC =", auc_cocaine, "\n")

r2_cocaine <- pR2(step_model_cocaine)

cat("McFadden R² =",
    r2_cocaine["McFadden"],
    "\n")

brier_cocaine <- mean(
  (prob_cocaine - cocaine_data$Y_COCAINE)^2
)

cat("Brier Score =",
    brier_cocaine,
    "\n")

plot(
  roc_cocaine,
  main = "ROC Curve - Cocaine"
)

# =====================================================
# METH MODEL
# =====================================================

prob_meth <- predict(
  step_model_meth,
  type = "response"
)

roc_meth <- roc(
  meth_data$Y_METH,
  prob_meth
)

auc_meth <- auc(roc_meth)

cat("\n============================\n")
cat("METH MODEL\n")
cat("============================\n")
cat("AUC =", auc_meth, "\n")

r2_meth <- pR2(step_model_meth)

cat("McFadden R² =",
    r2_meth["McFadden"],
    "\n")

brier_meth <- mean(
  (prob_meth - meth_data$Y_METH)^2
)

cat("Brier Score =",
    brier_meth,
    "\n")

plot(
  roc_meth,
  main = "ROC Curve - Meth"
)

# =====================================================
# SUMMARY TABLE
# =====================================================

results <- data.frame(
  Model = c("Heroin", "Cocaine", "Meth"),
  
  AUC = c(
    as.numeric(auc_heroin),
    as.numeric(auc_cocaine),
    as.numeric(auc_meth)
  ),
  
  McFadden_R2 = c(
    as.numeric(r2_heroin["McFadden"]),
    as.numeric(r2_cocaine["McFadden"]),
    as.numeric(r2_meth["McFadden"])
  ),
  
  Brier_Score = c(
    brier_heroin,
    brier_cocaine,
    brier_meth
  )
)

cat("\n============================\n")
cat("MODEL COMPARISON\n")
cat("============================\n")

print(results)
