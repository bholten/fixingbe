source("R/data.R")

library(dplyr)
library(ggplot2)

cat("=============================================================\n")
cat("   NON-LINEAR MODEL REFINEMENT\n")
cat("=============================================================\n\n")

###############################################################################
# Setup - use normalized_df from data.R
###############################################################################

cat("Using normalized_df from data.R\n")
cat("Total creatures:", nrow(normalized_df), "\n\n")

# Split by armor status
armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
no_armor_df <- normalized_df %>% filter(armor == 0)

cat("Armored creatures:", nrow(armor_df), "\n")
cat("Unarmored creatures:", nrow(no_armor_df), "\n\n")

###############################################################################
# Current linear model predictions (baseline)
###############################################################################

cat("=== BASELINE LINEAR MODEL ===\n\n")

# Armored baseline
armor_df$pred_linear <- -23 +
  0.01 * armor_df$hardiness +
  0.06 * armor_df$fortitude +
  0.005 * armor_df$dexterity +
  0.01 * armor_df$intellect +
  0.025 * armor_df$cleverness +
  0.015 * armor_df$power +
  0.1 * armor_df$kinen +
  0.08 * armor_df$nonkinen

armor_df$resid_linear <- armor_df$level - armor_df$pred_linear

# Unarmored baseline
no_armor_df$pred_linear <- 9 +
  0.01 * no_armor_df$hardiness -
  0.02 * no_armor_df$fortitude +
  0.01 * no_armor_df$dexterity +
  0.01 * no_armor_df$intellect +
  0.025 * no_armor_df$cleverness +
  0.015 * no_armor_df$power +
  0.12 * no_armor_df$kinen +
  0.06 * no_armor_df$nonkinen

no_armor_df$resid_linear <- no_armor_df$level - no_armor_df$pred_linear

# Baseline stats
combined_linear <- c(armor_df$resid_linear, no_armor_df$resid_linear)
r2_linear <- 1 - sum(combined_linear^2) /
  sum((c(armor_df$level, no_armor_df$level) - mean(c(armor_df$level, no_armor_df$level)))^2)
sw_linear <- shapiro.test(combined_linear)

cat("Combined baseline:\n")
cat("  R²:", round(r2_linear, 4), "\n")
cat("  SD:", round(sd(combined_linear), 3), "\n")
cat("  Shapiro p:", format(sw_linear$p.value, digits = 4), "\n\n")

###############################################################################
# Add non-linear terms from previous investigation
###############################################################################

cat("=== ADDING NON-LINEAR TERMS ===\n\n")

# Create non-linear features for both datasets
add_nonlinear_features <- function(df) {
  df %>%
    mutate(
      # Cleverness thresholds
      clev_above_200 = pmax(0, cleverness - 200),
      clev_above_300 = pmax(0, cleverness - 300),
      clev_above_400 = pmax(0, cleverness - 400),

      # Squared terms
      intellect_sq = intellect^2 / 1000,  # scale down
      kinen_sq = kinen^2 / 100,           # scale down

      # Interaction
      power_clev = (power * cleverness) / 10000,  # scale down

      # Kinen threshold (positive vs negative)
      kinen_positive = ifelse(kinen > 0, kinen, 0),
      kinen_negative = ifelse(kinen < 0, kinen, 0)
    )
}

armor_df <- add_nonlinear_features(armor_df)
no_armor_df <- add_nonlinear_features(no_armor_df)

###############################################################################
# Fit non-linear model for UNARMORED (where we have the most outlier issues)
###############################################################################

cat("--- Unarmored Non-Linear Model ---\n\n")

# Start with base linear terms and add non-linear
model_unarmor_nl <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                         cleverness + power + kinen + nonkinen +
                         clev_above_200 + intellect_sq + power_clev,
                       data = no_armor_df)

cat("Non-linear model summary:\n")
print(summary(model_unarmor_nl))

no_armor_df$pred_nl <- predict(model_unarmor_nl)
no_armor_df$resid_nl <- no_armor_df$level - no_armor_df$pred_nl

cat("\nUnarmored non-linear model stats:\n")
cat("  R²:", round(summary(model_unarmor_nl)$r.squared, 4), "\n")
cat("  SD:", round(sd(no_armor_df$resid_nl), 3), "\n")
sw_unarmor_nl <- shapiro.test(no_armor_df$resid_nl)
cat("  Shapiro p:", format(sw_unarmor_nl$p.value, digits = 4),
    ifelse(sw_unarmor_nl$p.value > 0.05, " <-- NORMAL!", ""), "\n")

###############################################################################
# Fit non-linear model for ARMORED
###############################################################################

cat("\n--- Armored Non-Linear Model ---\n\n")

model_armor_nl <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                       cleverness + power + kinen + nonkinen +
                       clev_above_200 + intellect_sq + power_clev,
                     data = armor_df)

cat("Non-linear model summary:\n")
print(summary(model_armor_nl))

armor_df$pred_nl <- predict(model_armor_nl)
armor_df$resid_nl <- armor_df$level - armor_df$pred_nl

cat("\nArmored non-linear model stats:\n")
cat("  R²:", round(summary(model_armor_nl)$r.squared, 4), "\n")
cat("  SD:", round(sd(armor_df$resid_nl), 3), "\n")
sw_armor_nl <- shapiro.test(armor_df$resid_nl)
cat("  Shapiro p:", format(sw_armor_nl$p.value, digits = 4),
    ifelse(sw_armor_nl$p.value > 0.05, " <-- NORMAL!", ""), "\n")

###############################################################################
# Combined non-linear analysis
###############################################################################

cat("\n=== COMBINED NON-LINEAR MODEL ANALYSIS ===\n\n")

combined_nl <- c(armor_df$resid_nl, no_armor_df$resid_nl)
combined_levels <- c(armor_df$level, no_armor_df$level)
combined_pred_nl <- c(armor_df$pred_nl, no_armor_df$pred_nl)

r2_nl <- 1 - sum(combined_nl^2) / sum((combined_levels - mean(combined_levels))^2)
sw_nl <- shapiro.test(combined_nl)

cat("Combined non-linear model:\n")
cat("  R²:", round(r2_nl, 4), "\n")
cat("  SD:", round(sd(combined_nl), 3), "\n")
cat("  Shapiro p:", format(sw_nl$p.value, digits = 4),
    ifelse(sw_nl$p.value > 0.05, " <-- NORMAL!", ""), "\n")

cat("\nImprovement over linear:\n")
cat("  R²: ", round(r2_linear, 4), " -> ", round(r2_nl, 4),
    " (", round((r2_nl - r2_linear) * 100, 2), "% improvement)\n", sep = "")
cat("  SD: ", round(sd(combined_linear), 3), " -> ", round(sd(combined_nl), 3), "\n", sep = "")
cat("  Shapiro p: ", format(sw_linear$p.value, digits = 4), " -> ",
    format(sw_nl$p.value, digits = 4), "\n", sep = "")

###############################################################################
# Check the previous outliers
###############################################################################

cat("\n=== CHECKING PREVIOUS OUTLIERS ===\n\n")

# Find creatures with large residuals in linear model
outliers_linear <- bind_rows(
  armor_df %>% filter(abs(resid_linear) > 5) %>% mutate(type = "Armored"),
  no_armor_df %>% filter(abs(resid_linear) > 5) %>% mutate(type = "Unarmored")
)

cat("Creatures with |residual| > 5 in linear model:\n")
print(outliers_linear %>%
        select(serial, skin, type, level, pred_linear, resid_linear,
               pred_nl, resid_nl, cleverness, power) %>%
        mutate(improvement = abs(resid_linear) - abs(resid_nl)) %>%
        arrange(desc(abs(resid_linear))) %>%
        as.data.frame())

###############################################################################
# Simplified interpretable non-linear formula
###############################################################################

cat("\n\n=== DERIVING INTERPRETABLE FORMULA ===\n\n")

# For unarmored, extract key coefficients
coef_unarmor <- coef(model_unarmor_nl)
cat("Unarmored non-linear coefficients:\n")
for (name in names(coef_unarmor)) {
  cat(sprintf("  %s: %.4f\n", name, coef_unarmor[name]))
}

# Round to nice numbers
cat("\n\nSIMPLIFIED UNARMORED FORMULA:\n")
cat("level ≈ ", round(coef_unarmor["(Intercept)"], 0), "\n")
cat("  + ", round(coef_unarmor["hardiness"], 3), " × hardiness\n")
cat("  + ", round(coef_unarmor["fortitude"], 3), " × fortitude\n")
cat("  + ", round(coef_unarmor["dexterity"], 3), " × dexterity\n")
cat("  + ", round(coef_unarmor["intellect"], 3), " × intellect\n")
cat("  + ", round(coef_unarmor["cleverness"], 3), " × cleverness\n")
cat("  + ", round(coef_unarmor["power"], 3), " × power\n")
cat("  + ", round(coef_unarmor["kinen"], 3), " × kinen\n")
cat("  + ", round(coef_unarmor["nonkinen"], 3), " × nonkinen\n")
cat("  + ", round(coef_unarmor["clev_above_200"], 3), " × max(0, cleverness - 200)\n")
cat("  + ", round(coef_unarmor["intellect_sq"], 5), " × intellect²/1000\n")
cat("  + ", round(coef_unarmor["power_clev"], 4), " × (power × cleverness)/10000\n")

# For armored
coef_armor <- coef(model_armor_nl)
cat("\n\nSIMPLIFIED ARMORED FORMULA:\n")
cat("level ≈ ", round(coef_armor["(Intercept)"], 0), "\n")
cat("  + ", round(coef_armor["hardiness"], 3), " × hardiness\n")
cat("  + ", round(coef_armor["fortitude"], 3), " × fortitude\n")
cat("  + ", round(coef_armor["dexterity"], 3), " × dexterity\n")
cat("  + ", round(coef_armor["intellect"], 3), " × intellect\n")
cat("  + ", round(coef_armor["cleverness"], 3), " × cleverness\n")
cat("  + ", round(coef_armor["power"], 3), " × power\n")
cat("  + ", round(coef_armor["kinen"], 3), " × kinen\n")
cat("  + ", round(coef_armor["nonkinen"], 3), " × nonkinen\n")
cat("  + ", round(coef_armor["clev_above_200"], 3), " × max(0, cleverness - 200)\n")
cat("  + ", round(coef_armor["intellect_sq"], 5), " × intellect²/1000\n")
cat("  + ", round(coef_armor["power_clev"], 4), " × (power × cleverness)/10000\n")

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# Predicted vs Actual
armor_df$type <- "Armored"
no_armor_df$type <- "Unarmored"

combined_df <- bind_rows(
  armor_df %>% select(level, pred_linear, resid_linear, pred_nl, resid_nl, type),
  no_armor_df %>% select(level, pred_linear, resid_linear, pred_nl, resid_nl, type)
)

p1 <- ggplot(combined_df, aes(x = pred_nl, y = level, color = type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Armored" = "blue", "Unarmored" = "red")) +
  labs(title = "Non-Linear Model: Predicted vs Actual Level",
       x = "Predicted Level", y = "Actual Level") +
  theme_minimal()
print(p1)

# Residual comparison
p2 <- ggplot(combined_df) +
  geom_histogram(aes(x = resid_linear, fill = "Linear"), alpha = 0.5, binwidth = 0.5) +
  geom_histogram(aes(x = resid_nl, fill = "Non-Linear"), alpha = 0.5, binwidth = 0.5) +
  scale_fill_manual(values = c("Linear" = "gray50", "Non-Linear" = "steelblue")) +
  labs(title = "Residual Distribution: Linear vs Non-Linear",
       x = "Residual", y = "Count", fill = "Model") +
  theme_minimal()
print(p2)

# QQ plots
p3 <- ggplot(combined_df, aes(sample = resid_nl)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~type) +
  labs(title = "QQ Plot of Non-Linear Model Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
print(p3)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("LINEAR MODEL:\n")
cat("  Combined R²:", round(r2_linear, 4), "\n")
cat("  Combined SD:", round(sd(combined_linear), 3), "levels\n")
cat("  Combined Shapiro p:", format(sw_linear$p.value, digits = 4), "\n")
cat("  Normal?", ifelse(sw_linear$p.value > 0.05, "YES", "NO"), "\n")

cat("\nNON-LINEAR MODEL (with cleverness threshold, intellect², power×cleverness):\n")
cat("  Combined R²:", round(r2_nl, 4), "\n")
cat("  Combined SD:", round(sd(combined_nl), 3), "levels\n")
cat("  Combined Shapiro p:", format(sw_nl$p.value, digits = 4), "\n")
cat("  Normal?", ifelse(sw_nl$p.value > 0.05, "YES", "NO"), "\n")

cat("\nKEY FINDINGS:\n")
cat("1. Cleverness threshold at 200: Additional effect for high cleverness\n")
cat("2. Intellect²: Non-linear scaling of mind contribution\n")
cat("3. Power × Cleverness interaction: DPS components interact\n")

if (sw_nl$p.value > 0.05) {
  cat("\n*** NON-LINEAR MODEL ACHIEVES NORMAL RESIDUALS! ***\n")
  cat("The remaining variance is likely due to crafting system randomness.\n")
}
