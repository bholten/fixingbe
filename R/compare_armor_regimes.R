source("R/data.R")

library(dplyr)
library(ggplot2)
library(lmtest)
library(tidyr)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    avg_damage = (damage_low + damage_high) / 2
  )

# Split by armor status
armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=== COMPARING ARMORED VS UNARMORED FORMULAS ===\n\n")
cat("Armored creatures:", nrow(armor_df), "\n")
cat("Unarmored creatures:", nrow(no_armor_df), "\n\n")

###############################################################################
# Log(level) models with raw attributes
###############################################################################

# Unarmored model
model.unarmored <- lm(
  log(level) ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + courage +
               kinen + nonkinen,
  data = no_armor_df
)

# Armored model
model.armored <- lm(
  log(level) ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + courage +
               kinen + nonkinen,
  data = armor_df
)

###############################################################################
# Side-by-side coefficient comparison
###############################################################################

cat("=== COEFFICIENT COMPARISON ===\n\n")

# Extract coefficients
coef_unarmored <- coef(model.unarmored)
coef_armored <- coef(model.armored)
se_unarmored <- summary(model.unarmored)$coefficients[, "Std. Error"]
se_armored <- summary(model.armored)$coefficients[, "Std. Error"]

# Build comparison table
comparison <- data.frame(
  term = names(coef_unarmored),
  unarmored_coef = coef_unarmored,
  unarmored_se = se_unarmored,
  armored_coef = coef_armored[names(coef_unarmored)],
  armored_se = se_armored[names(coef_unarmored)]
) %>%
  mutate(
    unarmored_pct = ifelse(term == "(Intercept)",
                           exp(unarmored_coef),
                           (exp(unarmored_coef * 100) - 1) * 100),
    armored_pct = ifelse(term == "(Intercept)",
                         exp(armored_coef),
                         (exp(armored_coef * 100) - 1) * 100),
    sign_flip = sign(unarmored_coef) != sign(armored_coef)
  )

cat("Coefficients (raw values):\n")
cat(sprintf("%-12s %12s %12s %10s\n", "Term", "Unarmored", "Armored", "Sign Flip?"))
cat(paste(rep("-", 50), collapse = ""), "\n")
for (i in 1:nrow(comparison)) {
  row <- comparison[i, ]
  flip <- ifelse(row$sign_flip & row$term != "(Intercept)", "  <-- YES", "")
  cat(sprintf("%-12s %12.6f %12.6f %s\n",
              row$term, row$unarmored_coef, row$armored_coef, flip))
}

cat("\n\nPercentage effect per 100 units (or base level for intercept):\n")
cat(sprintf("%-12s %12s %12s %10s\n", "Term", "Unarmored", "Armored", "Sign Flip?"))
cat(paste(rep("-", 50), collapse = ""), "\n")
for (i in 1:nrow(comparison)) {
  row <- comparison[i, ]
  flip <- ifelse(row$sign_flip & row$term != "(Intercept)", "  <-- YES", "")
  if (row$term == "(Intercept)") {
    cat(sprintf("%-12s %11.2f %11.2f %s\n",
                "Base Level", row$unarmored_pct, row$armored_pct, ""))
  } else {
    cat(sprintf("%-12s %+11.1f%% %+11.1f%% %s\n",
                row$term, row$unarmored_pct, row$armored_pct, flip))
  }
}

###############################################################################
# Model summaries
###############################################################################

cat("\n\n=== UNARMORED MODEL SUMMARY ===\n\n")
print(summary(model.unarmored))

cat("\n\n=== ARMORED MODEL SUMMARY ===\n\n")
print(summary(model.armored))

###############################################################################
# R² comparison
###############################################################################

cat("\n\n=== MODEL FIT COMPARISON ===\n\n")

calc_r2_exp <- function(model, data) {
  pred <- exp(predict(model, newdata = data))
  actual <- data$level
  1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
}

cat("Log(level) model with raw attributes:\n")
cat("  Unarmored R² (original scale):", round(calc_r2_exp(model.unarmored, no_armor_df), 4), "\n")
cat("  Armored R² (original scale):  ", round(calc_r2_exp(model.armored, armor_df), 4), "\n")

cat("\n  Unarmored R² (log scale):     ", round(summary(model.unarmored)$r.squared, 4), "\n")
cat("  Armored R² (log scale):       ", round(summary(model.armored)$r.squared, 4), "\n")

###############################################################################
# Residual analysis
###############################################################################

cat("\n\n=== RESIDUAL ANALYSIS ===\n\n")

no_armor_df$pred <- exp(predict(model.unarmored))
no_armor_df$residual <- no_armor_df$level - no_armor_df$pred

armor_df$pred <- exp(predict(model.armored))
armor_df$residual <- armor_df$level - armor_df$pred

cat("Unarmored residuals:\n")
cat("  Mean:", round(mean(no_armor_df$residual), 3), "\n")
cat("  SD:", round(sd(no_armor_df$residual), 3), "\n")
cat("  BP test p-value:", round(bptest(model.unarmored)$p.value, 6), "\n")

cat("\nArmored residuals:\n")
cat("  Mean:", round(mean(armor_df$residual), 3), "\n")
cat("  SD:", round(sd(armor_df$residual), 3), "\n")
cat("  BP test p-value:", round(bptest(model.armored)$p.value, 6), "\n")

# Residuals by level bucket
cat("\n--- Unarmored residuals by predicted level ---\n")
no_armor_df %>%
  mutate(pred_bucket = cut(pred, breaks = c(0, 10, 15, 20, 25, 30, 40, 50), include.lowest = TRUE)) %>%
  group_by(pred_bucket) %>%
  summarise(n = n(), mean_res = mean(residual), sd_res = sd(residual), .groups = "drop") %>%
  print()

cat("\n--- Armored residuals by predicted level ---\n")
armor_df %>%
  mutate(pred_bucket = cut(pred, breaks = c(30, 35, 40, 45, 50, 55, 60), include.lowest = TRUE)) %>%
  group_by(pred_bucket) %>%
  summarise(n = n(), mean_res = mean(residual), sd_res = sd(residual), .groups = "drop") %>%
  print()

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATIONS ===\n")

# Combine for plotting
no_armor_df$type <- "Unarmored"
armor_df$type <- "Armored"
combined <- bind_rows(
  no_armor_df %>% select(level, pred, residual, type),
  armor_df %>% select(level, pred, residual, type)
)

p1 <- ggplot(combined, aes(x = pred, y = level, color = type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Armored" = "blue", "Unarmored" = "red")) +
  labs(
    title = "Log(Level) Model: Predicted vs Actual",
    subtitle = "Separate models for armored and unarmored",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p1)

p2 <- ggplot(combined, aes(x = pred, y = residual, color = type)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("Armored" = "blue", "Unarmored" = "red")) +
  labs(
    title = "Residuals vs Predicted Level",
    x = "Predicted Level",
    y = "Residual"
  ) +
  theme_minimal()
print(p2)

###############################################################################
# Key findings summary
###############################################################################

cat("\n\n=== KEY FINDINGS ===\n\n")

cat("SIGN FLIPS between armored and unarmored:\n")
flips <- comparison %>% filter(sign_flip & term != "(Intercept)")
if (nrow(flips) > 0) {
  for (i in 1:nrow(flips)) {
    cat(sprintf("  - %s: %.1f%% (unarmored) vs %.1f%% (armored)\n",
                flips$term[i], flips$unarmored_pct[i], flips$armored_pct[i]))
  }
} else {
  cat("  None found\n")
}

cat("\nLARGEST COEFFICIENT DIFFERENCES:\n")
comparison_noicept <- comparison %>% filter(term != "(Intercept)")
comparison_noicept$diff <- abs(comparison_noicept$unarmored_pct - comparison_noicept$armored_pct)
top_diffs <- comparison_noicept %>% arrange(desc(diff)) %>% head(5)
for (i in 1:nrow(top_diffs)) {
  cat(sprintf("  - %s: %.1f%% difference (%.1f%% vs %.1f%%)\n",
              top_diffs$term[i], top_diffs$diff[i],
              top_diffs$unarmored_pct[i], top_diffs$armored_pct[i]))
}
