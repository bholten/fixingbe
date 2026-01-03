source("R/data.R")

library(dplyr)
library(ggplot2)
library(mgcv)
library(broom)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    average_hdi = (hardiness + dexterity + intellect) / 3,
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    avg_damage = (damage_low + damage_high) / 2,
    dps = avg_damage * speed * to_hit,
    average_ham = (health + action + mind) / 3,
    total_ham = health + action + mind
  )

no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=== EXPLORING NON-LINEAR RELATIONSHIPS ===\n\n")
cat("Dataset size:", nrow(no_armor_df), "unarmored creatures\n\n")

###############################################################################
# PART 1: LOG TRANSFORMATIONS
###############################################################################

cat("=== PART 1: LOGARITHMIC RELATIONSHIPS ===\n\n")

# Try log(level) as outcome
model.log_level <- lm(
  log(level) ~ health + action + mind +
               damage_low + damage_high + speed + to_hit +
               kinen + nonkinen,
  data = no_armor_df
)

# Try log of predictors
no_armor_df <- no_armor_df %>%
  mutate(
    log_health = log(health + 1),
    log_action = log(action + 1),
    log_mind = log(mind + 1),
    log_total_ham = log(total_ham + 1),
    log_avg_ham = log(average_ham + 1)
  )

model.log_predictors <- lm(
  level ~ log_health + log_action + log_mind +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = no_armor_df
)

# Try both log level and log predictors
model.log_both <- lm(
  log(level) ~ log_health + log_action + log_mind +
               damage_low + damage_high + speed + to_hit +
               kinen + nonkinen,
  data = no_armor_df
)

# Simple model: log(level) ~ log(total_ham)
model.simple_log <- lm(log(level) ~ log_total_ham, data = no_armor_df)

# Compare with baseline linear model
model.linear <- lm(
  level ~ health + action + mind +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = no_armor_df
)

cat("--- Model Comparison (R² on original scale) ---\n\n")

# For log(level) models, we need to back-transform and compute R² on original scale
no_armor_df$pred_linear <- predict(model.linear)
no_armor_df$pred_log_level <- exp(predict(model.log_level))
no_armor_df$pred_log_predictors <- predict(model.log_predictors)
no_armor_df$pred_log_both <- exp(predict(model.log_both))
no_armor_df$pred_simple_log <- exp(predict(model.simple_log))

calc_r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - ss_res / ss_tot
}

cat("Linear model (baseline):           R² =", round(calc_r2(no_armor_df$level, no_armor_df$pred_linear), 4), "\n")
cat("Log(level) outcome:                R² =", round(calc_r2(no_armor_df$level, no_armor_df$pred_log_level), 4), "\n")
cat("Log(HAM) predictors:               R² =", round(calc_r2(no_armor_df$level, no_armor_df$pred_log_predictors), 4), "\n")
cat("Log both (level and HAM):          R² =", round(calc_r2(no_armor_df$level, no_armor_df$pred_log_both), 4), "\n")
cat("Simple: log(level) ~ log(HAM):     R² =", round(calc_r2(no_armor_df$level, no_armor_df$pred_simple_log), 4), "\n")

cat("\n--- Log(level) Model Coefficients ---\n")
print(summary(model.log_level))

# Visualize log relationship
p1 <- ggplot(no_armor_df, aes(x = log_total_ham, y = log(level))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Log(Level) vs Log(Total HAM)",
    x = "log(health + action + mind)",
    y = "log(level)"
  ) +
  theme_minimal()
print(p1)

###############################################################################
# PART 2: ROUNDING ANALYSIS
###############################################################################

cat("\n\n=== PART 2: ROUNDING PATTERNS ===\n\n")

# Get residuals from linear model
no_armor_df$residual_linear <- no_armor_df$level - no_armor_df$pred_linear

# Check if residuals cluster around certain values
cat("--- Residual Distribution ---\n")
cat("Mean residual:", round(mean(no_armor_df$residual_linear), 4), "\n")
cat("SD residual:", round(sd(no_armor_df$residual_linear), 4), "\n")
cat("\nResidual quantiles:\n")
print(quantile(no_armor_df$residual_linear, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)))

# Check fractional parts of predictions
no_armor_df$pred_frac <- no_armor_df$pred_linear %% 1

cat("\n--- Fractional Part of Predictions ---\n")
cat("If game uses floor(), residuals should be 0 to 1\n")
cat("If game uses round(), residuals should be -0.5 to 0.5\n")
cat("If game uses ceiling(), residuals should be -1 to 0\n\n")

# Test floor hypothesis: level = floor(calculated)
# If true, then level <= calculated < level + 1
# So: 0 <= calculated - level < 1
# Meaning: -residual should be between 0 and 1 (since residual = level - pred)
# Actually: if pred ≈ calculated, and level = floor(calculated)
# Then level <= pred, so residual = level - pred <= 0

cat("Residuals consistent with floor() (between -1 and 0):",
    sum(no_armor_df$residual_linear >= -1 & no_armor_df$residual_linear <= 0), "/",
    nrow(no_armor_df), "\n")
cat("Residuals consistent with round() (between -0.5 and 0.5):",
    sum(abs(no_armor_df$residual_linear) <= 0.5), "/",
    nrow(no_armor_df), "\n")
cat("Residuals consistent with ceiling() (between 0 and 1):",
    sum(no_armor_df$residual_linear >= 0 & no_armor_df$residual_linear <= 1), "/",
    nrow(no_armor_df), "\n")

# Histogram of residuals
p2 <- ggplot(no_armor_df, aes(x = residual_linear)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = c(-0.5, 0.5), color = "red", linetype = "dashed") +
  labs(
    title = "Distribution of Residuals",
    subtitle = "Red lines at ±0.5 (round() boundaries)",
    x = "Residual (actual - predicted)",
    y = "Count"
  ) +
  theme_minimal()
print(p2)

###############################################################################
# PART 3: LEVEL-DEPENDENT EFFECTS
###############################################################################

cat("\n\n=== PART 3: LEVEL-DEPENDENT PATTERNS ===\n\n")

# Check if there's a pattern in residuals vs predicted level
no_armor_df$level_bucket <- cut(no_armor_df$pred_linear,
                                 breaks = c(0, 10, 15, 20, 25, 30, 35, 40, 50),
                                 labels = c("0-10", "10-15", "15-20", "20-25",
                                           "25-30", "30-35", "35-40", "40-50"))

residual_by_pred <- no_armor_df %>%
  group_by(level_bucket) %>%
  summarise(
    n = n(),
    mean_pred = mean(pred_linear),
    mean_actual = mean(level),
    mean_residual = mean(residual_linear),
    sd_residual = sd(residual_linear),
    .groups = "drop"
  )

cat("Residuals by predicted level bucket:\n")
print(residual_by_pred)

cat("\nIf there's a consistent bias pattern, the formula might use:\n")
cat("- A piecewise linear function (different slopes at different ranges)\n")
cat("- A ceiling/floor at certain thresholds\n")
cat("- A polynomial or log transformation\n")

p3 <- ggplot(no_armor_df, aes(x = pred_linear, y = residual_linear)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Predicted Level",
    subtitle = "Red line shows local trend (should be flat if model is correct)",
    x = "Predicted Level",
    y = "Residual"
  ) +
  theme_minimal()
print(p3)

###############################################################################
# PART 4: POLYNOMIAL MODELS
###############################################################################

cat("\n\n=== PART 4: POLYNOMIAL / SQRT TRANSFORMATIONS ===\n\n")

# Try sqrt of HAM
no_armor_df <- no_armor_df %>%
  mutate(
    sqrt_health = sqrt(health),
    sqrt_action = sqrt(action),
    sqrt_mind = sqrt(mind),
    sqrt_total_ham = sqrt(total_ham)
  )

model.sqrt_ham <- lm(
  level ~ sqrt_health + sqrt_action + sqrt_mind +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = no_armor_df
)

# Try quadratic terms
model.quad <- lm(
  level ~ health + I(health^2) + action + I(action^2) + mind + I(mind^2) +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = no_armor_df
)

no_armor_df$pred_sqrt <- predict(model.sqrt_ham)
no_armor_df$pred_quad <- predict(model.quad)

cat("Linear model:                      R² =", round(summary(model.linear)$r.squared, 4), "\n")
cat("Sqrt(HAM) predictors:              R² =", round(summary(model.sqrt_ham)$r.squared, 4), "\n")
cat("Quadratic (HAM + HAM²):            R² =", round(summary(model.quad)$r.squared, 4), "\n")

cat("\n--- Sqrt Model Coefficients ---\n")
print(summary(model.sqrt_ham))

###############################################################################
# PART 5: COMPARISON VISUALIZATION
###############################################################################

cat("\n\n=== PART 5: MODEL COMPARISON ===\n")

comparison_long <- no_armor_df %>%
  select(level, pred_linear, pred_log_level, pred_sqrt) %>%
  tidyr::pivot_longer(
    cols = starts_with("pred_"),
    names_to = "model",
    values_to = "predicted"
  ) %>%
  mutate(model = case_when(
    model == "pred_linear" ~ "Linear",
    model == "pred_log_level" ~ "Log(level)",
    model == "pred_sqrt" ~ "Sqrt(HAM)"
  ))

p4 <- ggplot(comparison_long, aes(x = predicted, y = level)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~model) +
  labs(
    title = "Model Comparison: Predicted vs Actual Level",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p4)

###############################################################################
# PART 6: RESIDUAL PATTERNS BY LEVEL BUCKET
###############################################################################

cat("\n\n=== PART 6: CHECKING FOR INTEGER BOUNDARIES ===\n\n")

# Check if residuals change at integer boundaries (10, 15, 20, etc.)
no_armor_df$actual_bucket <- cut(no_armor_df$level,
                                  breaks = c(0, 10, 15, 20, 25, 30, 40, 50),
                                  include.lowest = TRUE)

cat("Mean residual by actual level bucket:\n")
no_armor_df %>%
  group_by(actual_bucket) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    mean_residual = mean(residual_linear),
    .groups = "drop"
  ) %>%
  print()

cat("\n--- Key Question: Does the game round level to nearest 5? ---\n")
cat("Level distribution modulo 5:\n")
table(no_armor_df$level %% 5)

cat("\n\nLevel distribution modulo 10:\n")
table(no_armor_df$level %% 10)
