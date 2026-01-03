source("R/data.R")

library(dplyr)
library(ggplot2)
library(lmtest)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    avg_damage = (damage_low + damage_high) / 2,
    damage_spread = damage_high - damage_low
  )

no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=== TESTING CLEANER MODELS ===\n\n")
cat("Full unarmored dataset:", nrow(no_armor_df), "creatures\n")

###############################################################################
# Apply post-patch damage filter (from damage_analysis.Rmd)
###############################################################################

post_patch_df <- no_armor_df %>%
  filter((damage_spread > 10 & power >= 380) | power < 380)

cat("After post-patch damage filter:", nrow(post_patch_df), "creatures\n\n")

###############################################################################
# Model comparison on post-patch data
###############################################################################

cat("=== MODEL COMPARISON (Post-Patch Data) ===\n\n")

# Model 1: Original with damage_low + damage_high (problematic)
model.orig <- lm(
  log(level) ~ health + action + mind +
               damage_low + damage_high + speed + to_hit +
               kinen + nonkinen,
  data = post_patch_df
)

# Model 2: Use avg_damage instead
model.avg_dmg <- lm(
  log(level) ~ health + action + mind +
               avg_damage + speed + to_hit +
               kinen + nonkinen,
  data = post_patch_df
)

# Model 3: Use power instead of damage
model.power <- lm(
  log(level) ~ health + action + mind +
               power + speed + to_hit +
               kinen + nonkinen,
  data = post_patch_df
)

# Model 4: Raw attributes only (like armored model style)
model.raw <- lm(
  log(level) ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + courage +
               kinen + nonkinen,
  data = post_patch_df
)

# Compare R² on original scale
calc_r2_exp <- function(model, data) {
  pred <- exp(predict(model, newdata = data))
  actual <- data$level
  1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
}

cat("Log(level) models - R² on original scale:\n")
cat("  Original (dmg_low + dmg_high):  ", round(calc_r2_exp(model.orig, post_patch_df), 4), "\n")
cat("  With avg_damage:                 ", round(calc_r2_exp(model.avg_dmg, post_patch_df), 4), "\n")
cat("  With power:                      ", round(calc_r2_exp(model.power, post_patch_df), 4), "\n")
cat("  Raw attributes only:             ", round(calc_r2_exp(model.raw, post_patch_df), 4), "\n")

###############################################################################
# Best model coefficients
###############################################################################

cat("\n\n=== BEST MODEL: LOG(LEVEL) ~ RAW ATTRIBUTES ===\n\n")
print(summary(model.raw))

cat("\n--- Coefficient Interpretation ---\n")
cat("Base level:", round(exp(coef(model.raw)["(Intercept)"]), 2), "\n\n")

coefs <- coef(model.raw)[-1]  # exclude intercept
for (name in names(coefs)) {
  val <- coefs[name]
  if (abs(val) < 0.01) {
    pct_100 <- (exp(val * 100) - 1) * 100
    cat(sprintf("%s: +100 units = %+.1f%% level\n", name, pct_100))
  } else {
    pct_1 <- (exp(val) - 1) * 100
    cat(sprintf("%s: +1 unit = %+.1f%% level\n", name, pct_1))
  }
}

###############################################################################
# Residual analysis
###############################################################################

cat("\n\n=== RESIDUAL ANALYSIS ===\n\n")

post_patch_df$pred_raw <- exp(predict(model.raw, newdata = post_patch_df))
post_patch_df$residual_raw <- post_patch_df$level - post_patch_df$pred_raw

# By predicted level bucket
post_patch_df$pred_bucket <- cut(post_patch_df$pred_raw,
                                  breaks = c(0, 10, 15, 20, 25, 30, 40, 50),
                                  include.lowest = TRUE)

residual_summary <- post_patch_df %>%
  group_by(pred_bucket) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    mean_residual = mean(residual_raw),
    sd_residual = sd(residual_raw),
    .groups = "drop"
  )

cat("Residuals by predicted level bucket:\n")
print(residual_summary)

cat("\n--- Diagnostic Tests ---\n")
cat("BP test p-value:", round(bptest(model.raw)$p.value, 4), "\n")
cat("Shapiro-Wilk p-value:", round(shapiro.test(resid(model.raw))$p.value, 6), "\n")

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

p1 <- ggplot(post_patch_df, aes(x = pred_raw, y = level)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Log(Level) Model with Raw Attributes (Post-Patch)",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p1)

p2 <- ggplot(post_patch_df, aes(x = pred_raw, y = residual_raw)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Predicted (Post-Patch)",
    x = "Predicted Level",
    y = "Residual"
  ) +
  theme_minimal()
print(p2)

###############################################################################
# Compare with full dataset
###############################################################################

cat("\n\n=== COMPARISON: FULL VS POST-PATCH ===\n\n")

model.raw.full <- lm(
  log(level) ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + courage +
               kinen + nonkinen,
  data = no_armor_df
)

cat("Raw attributes model R² (original scale):\n")
cat("  Full dataset:      ", round(calc_r2_exp(model.raw.full, no_armor_df), 4), "\n")
cat("  Post-patch only:   ", round(calc_r2_exp(model.raw, post_patch_df), 4), "\n")
