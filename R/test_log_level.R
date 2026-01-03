source("R/data.R")

library(dplyr)
library(ggplot2)
library(lmtest)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    avg_damage = (damage_low + damage_high) / 2
  )

no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=== TESTING LOG(LEVEL) MODEL ===\n\n")

###############################################################################
# Log(level) model
###############################################################################

model.log <- lm(
  log(level) ~ health + action + mind +
               damage_low + damage_high + speed + to_hit +
               kinen + nonkinen,
  data = no_armor_df
)

# Get predictions on original scale
no_armor_df$pred_log <- exp(predict(model.log))
no_armor_df$residual_log <- no_armor_df$level - no_armor_df$pred_log

# Compare with linear model
model.linear <- lm(
  level ~ health + action + mind +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = no_armor_df
)
no_armor_df$pred_linear <- predict(model.linear)
no_armor_df$residual_linear <- no_armor_df$level - no_armor_df$pred_linear

###############################################################################
# Residual comparison by predicted level
###############################################################################

cat("=== RESIDUAL COMPARISON BY PREDICTED LEVEL ===\n\n")

no_armor_df$pred_bucket <- cut(no_armor_df$pred_linear,
                                breaks = c(0, 10, 15, 20, 25, 30, 40, 50),
                                include.lowest = TRUE)

comparison <- no_armor_df %>%
  group_by(pred_bucket) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    linear_residual = mean(residual_linear),
    log_residual = mean(residual_log),
    linear_sd = sd(residual_linear),
    log_sd = sd(residual_log),
    .groups = "drop"
  )

cat("Linear vs Log(level) model - Mean residuals by bucket:\n")
print(comparison)

###############################################################################
# Diagnostic tests
###############################################################################

cat("\n\n=== DIAGNOSTIC TESTS ===\n\n")

cat("--- Linear Model ---\n")
cat("R² (original scale):", round(summary(model.linear)$r.squared, 4), "\n")
cat("Residual SE:", round(summary(model.linear)$sigma, 4), "\n")
cat("BP test p-value:", round(bptest(model.linear)$p.value, 6), "\n")

cat("\n--- Log(level) Model ---\n")
# For log model, compute R² on original scale
ss_res <- sum((no_armor_df$level - no_armor_df$pred_log)^2)
ss_tot <- sum((no_armor_df$level - mean(no_armor_df$level))^2)
r2_log <- 1 - ss_res / ss_tot
cat("R² (original scale):", round(r2_log, 4), "\n")
cat("Residual SE (original):", round(sd(no_armor_df$residual_log), 4), "\n")
cat("BP test p-value (log scale):", round(bptest(model.log)$p.value, 6), "\n")

cat("\n--- Shapiro-Wilk Test (normality) ---\n")
cat("Linear model:", shapiro.test(no_armor_df$residual_linear)$p.value, "\n")
cat("Log model (on log scale):", shapiro.test(resid(model.log))$p.value, "\n")

###############################################################################
# Visualizations
###############################################################################

cat("\n\n=== VISUALIZATIONS ===\n")

# Residuals vs predicted for both models
p1 <- ggplot(no_armor_df, aes(x = pred_linear, y = residual_linear)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Linear Model: Residuals vs Predicted",
       subtitle = "Bathtub pattern visible",
       x = "Predicted Level", y = "Residual") +
  theme_minimal()
print(p1)

p2 <- ggplot(no_armor_df, aes(x = pred_log, y = residual_log)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Log(Level) Model: Residuals vs Predicted",
       subtitle = "Is the bathtub pattern reduced?",
       x = "Predicted Level", y = "Residual") +
  theme_minimal()
print(p2)

# Predicted vs actual for both
p3 <- ggplot(no_armor_df, aes(x = pred_linear, y = level)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Linear Model: Predicted vs Actual",
       x = "Predicted Level", y = "Actual Level") +
  theme_minimal()
print(p3)

p4 <- ggplot(no_armor_df, aes(x = pred_log, y = level)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Log(Level) Model: Predicted vs Actual",
       x = "Predicted Level", y = "Actual Level") +
  theme_minimal()
print(p4)

###############################################################################
# Coefficient interpretation
###############################################################################

cat("\n\n=== LOG MODEL COEFFICIENTS ===\n\n")

cat("In the log(level) model, coefficients represent PERCENTAGE effects:\n")
cat("A 1-unit increase in X multiplies level by exp(coef)\n\n")

coefs <- coef(model.log)
for (name in names(coefs)) {
  val <- coefs[name]
  pct <- (exp(val) - 1) * 100
  if (name == "(Intercept)") {
    cat(sprintf("Base level (exp of intercept): %.2f\n", exp(val)))
  } else if (abs(val) < 0.01) {
    # For small coefficients, show effect per 100 or 1000 units
    pct_100 <- (exp(val * 100) - 1) * 100
    pct_1000 <- (exp(val * 1000) - 1) * 100
    cat(sprintf("%s: %.6f → +100 units = %.2f%% level, +1000 units = %.2f%% level\n",
                name, val, pct_100, pct_1000))
  } else {
    cat(sprintf("%s: %.6f → +1 unit = %.2f%% level change\n", name, val, pct))
  }
}

###############################################################################
# Summary
###############################################################################

cat("\n\n=== SUMMARY ===\n\n")

cat("If log(level) eliminates the bathtub pattern, this suggests:\n")
cat("  level = exp(base + sum of weighted stats)\n")
cat("  i.e., stats have MULTIPLICATIVE effects on level\n\n")
cat("If bathtub pattern persists, we may need piecewise models or\n")
cat("there's genuine complexity in the unarmored formula.\n")
