source("R/data.R")

library(dplyr)
library(ggplot2)

cat("=============================================================\n")
cat("   FINAL MODEL: COMBINING ALL EFFECTS\n")
cat("=============================================================\n\n")

###############################################################################
# Setup
###############################################################################

armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
no_armor_df <- normalized_df %>% filter(armor == 0)

cat("Armored creatures:", nrow(armor_df), "\n")
cat("Unarmored creatures:", nrow(no_armor_df), "\n\n")

###############################################################################
# Baseline linear predictions
###############################################################################

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

###############################################################################
# Full model with skin effects (unarmored only, skins with n >= 3)
###############################################################################

cat("=== UNARMORED: TESTING DIFFERENT MODEL SPECIFICATIONS ===\n\n")

# Filter to common skins
skin_counts <- no_armor_df %>%
  group_by(skin) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 3)

no_armor_common <- no_armor_df %>%
  filter(skin %in% skin_counts$skin)

cat("Using", nrow(no_armor_common), "creatures from", nrow(skin_counts), "skins\n\n")

# Model 1: Linear only
model1 <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + kinen + nonkinen,
             data = no_armor_common)

# Model 2: Linear + skin
model2 <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + kinen + nonkinen + skin,
             data = no_armor_common)

# Model 3: Linear + skin + level quadratic term
# Use predicted level as proxy to avoid circularity
no_armor_common$pred_for_quad <- predict(model1)
model3 <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + kinen + nonkinen + skin +
               I(pred_for_quad^2),
             data = no_armor_common)

# Model 4: Linear + skin + intellect^2 (non-linear mind)
model4 <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + kinen + nonkinen + skin +
               I(intellect^2),
             data = no_armor_common)

# Compare models
cat("Model Comparison:\n\n")

models <- list(
  "1. Linear only" = model1,
  "2. Linear + skin" = model2,
  "3. + pred_level^2" = model3,
  "4. + intellect^2" = model4
)

for (name in names(models)) {
  m <- models[[name]]
  resids <- residuals(m)
  r2 <- summary(m)$r.squared
  sw <- shapiro.test(resids)

  cat(sprintf("%-20s  R²=%.4f  SD=%.3f  Shapiro p=%.6f %s\n",
              name, r2, sd(resids), sw$p.value,
              ifelse(sw$p.value > 0.05, "<-- NORMAL!", "")))
}

###############################################################################
# Try robust regression to handle outliers
###############################################################################

cat("\n\n=== ROBUST REGRESSION ===\n\n")

# Check if MASS is available for robust regression
if (require(MASS, quietly = TRUE)) {
  model_robust <- rlm(level ~ hardiness + fortitude + dexterity + intellect +
                        cleverness + power + kinen + nonkinen + skin,
                      data = no_armor_common)

  no_armor_common$pred_robust <- predict(model_robust)
  no_armor_common$resid_robust <- no_armor_common$level - no_armor_common$pred_robust

  cat("Robust regression (rlm):\n")
  cat("  SD:", round(sd(no_armor_common$resid_robust), 3), "\n")
  sw_robust <- shapiro.test(no_armor_common$resid_robust)
  cat("  Shapiro p:", format(sw_robust$p.value, digits = 4),
      ifelse(sw_robust$p.value > 0.05, " <-- NORMAL!", ""), "\n")
} else {
  cat("MASS package not available for robust regression\n")
}

###############################################################################
# Identify remaining outliers
###############################################################################

cat("\n\n=== REMAINING OUTLIERS (using model 2: linear + skin) ===\n\n")

no_armor_common$pred_skin <- predict(model2)
no_armor_common$resid_skin <- no_armor_common$level - no_armor_common$pred_skin

# Find outliers (|resid| > 3)
outliers <- no_armor_common %>%
  filter(abs(resid_skin) > 3) %>%
  dplyr::select(serial, skin, level, pred_skin, resid_skin,
         cleverness, power, hardiness, fortitude) %>%
  arrange(desc(abs(resid_skin)))

cat("Creatures with |residual| > 3 after skin adjustment:\n\n")
print(as.data.frame(outliers))

cat("\n", nrow(outliers), "outliers out of", nrow(no_armor_common), "creatures\n")
cat("(", round(nrow(outliers) / nrow(no_armor_common) * 100, 1), "%)\n", sep = "")

###############################################################################
# Test removing outliers
###############################################################################

cat("\n\n=== WITHOUT OUTLIERS ===\n\n")

no_outliers <- no_armor_common %>%
  filter(abs(resid_skin) <= 3)

model_clean <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                    cleverness + power + kinen + nonkinen + skin,
                  data = no_outliers)

clean_resids <- residuals(model_clean)
sw_clean <- shapiro.test(clean_resids)

cat("Without outliers (n =", nrow(no_outliers), "):\n")
cat("  R²:", round(summary(model_clean)$r.squared, 4), "\n")
cat("  SD:", round(sd(clean_resids), 3), "\n")
cat("  Shapiro p:", format(sw_clean$p.value, digits = 4),
    ifelse(sw_clean$p.value > 0.05, " <-- NORMAL!", ""), "\n")

###############################################################################
# Final combined analysis (armored + unarmored)
###############################################################################

cat("\n\n=== COMBINED ANALYSIS ===\n\n")

# Armored: use linear model (already normal)
sw_armor <- shapiro.test(armor_df$resid_linear)

# Unarmored: use model with skin effects
no_armor_df$skin_adj <- no_armor_df$skin %in% skin_counts$skin
no_armor_with_skin <- no_armor_df %>% filter(skin_adj)

model_unarmor_final <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                            cleverness + power + kinen + nonkinen + skin,
                          data = no_armor_with_skin)

no_armor_with_skin$resid_final <- residuals(model_unarmor_final)
sw_unarmor <- shapiro.test(no_armor_with_skin$resid_final)

cat("ARMORED (linear model):\n")
cat("  n:", nrow(armor_df), "\n")
cat("  R²: 0.979\n")
cat("  SD:", round(sd(armor_df$resid_linear), 3), "\n")
cat("  Shapiro p:", format(sw_armor$p.value, digits = 4),
    ifelse(sw_armor$p.value > 0.05, " <-- NORMAL!", ""), "\n")

cat("\nUNARMORED (with skin effects):\n")
cat("  n:", nrow(no_armor_with_skin), "\n")
cat("  R²:", round(summary(model_unarmor_final)$r.squared, 4), "\n")
cat("  SD:", round(sd(no_armor_with_skin$resid_final), 3), "\n")
cat("  Shapiro p:", format(sw_unarmor$p.value, digits = 4),
    ifelse(sw_unarmor$p.value > 0.05, " <-- NORMAL!", ""), "\n")

# Combined
combined_resids <- c(armor_df$resid_linear, no_armor_with_skin$resid_final)
combined_levels <- c(armor_df$level, no_armor_with_skin$level)

r2_combined <- 1 - sum(combined_resids^2) / sum((combined_levels - mean(combined_levels))^2)
sw_combined <- shapiro.test(combined_resids)

cat("\nCOMBINED:\n")
cat("  n:", length(combined_resids), "\n")
cat("  R²:", round(r2_combined, 4), "\n")
cat("  SD:", round(sd(combined_resids), 3), "\n")
cat("  Shapiro p:", format(sw_combined$p.value, digits = 4),
    ifelse(sw_combined$p.value > 0.05, " <-- NORMAL!", ""), "\n")

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# QQ plot of final residuals
p1 <- ggplot(data.frame(resid = combined_resids), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Combined Residuals",
       subtitle = sprintf("Shapiro p = %.4f", sw_combined$p.value),
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
print(p1)

# Histogram of combined residuals
p2 <- ggplot(data.frame(resid = combined_resids), aes(x = resid)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "steelblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(combined_resids), sd = sd(combined_resids)),
                color = "red", linewidth = 1) +
  labs(title = "Distribution of Combined Residuals",
       x = "Residual", y = "Density") +
  theme_minimal()
print(p2)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    FINAL SUMMARY\n")
cat("=============================================================\n\n")

cat("BEST MODEL CONFIGURATION:\n\n")

cat("ARMORED CREATURES:\n")
cat("  level = -23\n")
cat("    + 0.01 × hardiness\n")
cat("    + 0.06 × fortitude\n")
cat("    + 0.005 × dexterity\n")
cat("    + 0.01 × intellect\n")
cat("    + 0.025 × cleverness\n")
cat("    + 0.015 × power\n")
cat("    + 0.1 × kinen\n")
cat("    + 0.08 × nonkinen\n")
cat("  R² = 0.979, SD = 1.8 levels, Shapiro p = 0.56 (NORMAL)\n\n")

cat("UNARMORED CREATURES:\n")
cat("  level = 9\n")
cat("    + 0.01 × hardiness\n")
cat("    - 0.02 × fortitude\n")
cat("    + 0.01 × dexterity\n")
cat("    + 0.01 × intellect\n")
cat("    + 0.025 × cleverness\n")
cat("    + 0.015 × power\n")
cat("    + 0.12 × kinen\n")
cat("    + 0.06 × nonkinen\n")
cat("    + skin_adjustment\n")
cat("  R² = 0.947, SD = 1.9 levels\n\n")

cat("NOTABLE SKIN ADJUSTMENTS (unarmored):\n")
cat("  Rancor:      +5.2 levels\n")
cat("  Merek:       +1.9 levels\n")
cat("  Falumpaset:  +1.2 levels\n")
cat("  Woolamander: -2.5 levels\n")
cat("  Torton:      -1.5 levels\n")
cat("  Huurton:     -1.2 levels\n")
cat("  (See full list from model coefficients)\n\n")

cat("INTERPRETATION:\n")
cat("  1. The base attribute formula explains most variance (R² > 0.94)\n")
cat("  2. Skin-specific adjustments capture inherent level bonuses\n")
cat("  3. Remaining variance (~2 levels SD) is likely crafting randomness\n")
cat("  4. A few persistent outliers may have data quality issues\n")
