source("R/data.R")

library(dplyr)
library(ggplot2)

cat("=============================================================\n")
cat("       TESTING SKIN-SPECIFIC BASE RESIST HYPOTHESIS\n")
cat("=============================================================\n\n")

###############################################################################
# Load raw creature data with special/effective resists
###############################################################################

creatures_raw <- read_csv("data/clean/furrycat/creatures.csv", show_col_types = FALSE)
templates <- read_csv("data/clean/furrycat/templates.csv", show_col_types = FALSE)

# Join to get all the data we need
full_df <- creatures_raw %>%
  left_join(templates %>% select(serial, fortitude, hardiness, dexterity,
                                  intellect, cleverness, power),
            by = c("template_id" = "serial")) %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    # Special resist averages
    kinen_special = (kinetic.special + energy.special) / 2,
    nonkinen_special = (blast.special + heat.special + cold.special +
                        electricity.special + acid.special + stun.special) / 6
  )

# Focus on unarmored
unarmored_df <- full_df %>% filter(armor == 0)

cat("Unarmored creatures:", nrow(unarmored_df), "\n\n")

###############################################################################
# Step 1: Calculate skin-specific base resists
###############################################################################

cat("=== STEP 1: CALCULATE SKIN-SPECIFIC BASE RESISTS ===\n\n")

# For each skin, calculate the average deviation of kinen from fortitude/10
skin_base_kinen <- unarmored_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_kinen = mean(kinen),
    mean_fort = mean(fortitude),
    expected_kinen = mean(fortitude) / 10,
    base_kinen = mean(kinen - fortitude / 10),  # This is the "base" resist
    sd_base = sd(kinen - fortitude / 10),
    .groups = "drop"
  ) %>%
  arrange(base_kinen)

cat("Skin-specific base kinetic/energy resist (deviation from fort/10):\n\n")
print(skin_base_kinen, n = 40)

# Same for nonkinen
skin_base_nonkinen <- unarmored_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_nonkinen = mean(nonkinen),
    mean_fort = mean(fortitude),
    expected_nonkinen = mean(fortitude) / 10,
    base_nonkinen = mean(nonkinen - fortitude / 10),
    .groups = "drop"
  ) %>%
  arrange(base_nonkinen)

cat("\n\nSkin-specific base non-kinen resist (deviation from fort/10):\n\n")
print(skin_base_nonkinen, n = 40)

###############################################################################
# Step 2: Create adjusted resist variables
###############################################################################

cat("\n\n=== STEP 2: CREATE ADJUSTED RESIST VARIABLES ===\n\n")

# Join skin base resists back to the data
unarmored_df <- unarmored_df %>%
  left_join(skin_base_kinen %>% select(skin, base_kinen), by = "skin") %>%
  left_join(skin_base_nonkinen %>% select(skin, base_nonkinen), by = "skin") %>%
  mutate(
    # Adjusted resist = actual resist - skin base
    # This isolates the "fortitude contribution" to resist
    kinen_adjusted = kinen - base_kinen,
    nonkinen_adjusted = nonkinen - base_nonkinen,

    # Or we could think of it as: what the resist "should be" based on fort/10
    kinen_from_fort = fortitude / 10,
    nonkinen_from_fort = fortitude / 10  # Same contribution from fortitude
  )

cat("Created adjusted resist variables:\n")
cat("  kinen_adjusted = kinen - skin_base_kinen\n")
cat("  nonkinen_adjusted = nonkinen - skin_base_nonkinen\n")
cat("  kinen_from_fort = fortitude / 10\n\n")

###############################################################################
# Step 3: Compare level models
###############################################################################

cat("=== STEP 3: COMPARE LEVEL MODELS ===\n\n")

# Model 1: Original (using raw resists)
model_original <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen,
  data = unarmored_df
)

# Model 2: Using adjusted resists (removing skin base)
model_adjusted <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen_adjusted + nonkinen_adjusted,
  data = unarmored_df
)

# Model 3: Using fortitude-derived resists only
model_fort_only <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen_from_fort,
  data = unarmored_df
)

# Model 4: Original + skin as a factor (to see if skin explains variance)
model_with_skin <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen + skin,
  data = unarmored_df
)

# Compare R²
cat("Model Comparison (R²):\n\n")
cat("  Model 1 (original resists):      ", round(summary(model_original)$r.squared, 4), "\n")
cat("  Model 2 (adjusted resists):      ", round(summary(model_adjusted)$r.squared, 4), "\n")
cat("  Model 3 (fort-derived only):     ", round(summary(model_fort_only)$r.squared, 4), "\n")
cat("  Model 4 (original + skin):       ", round(summary(model_with_skin)$r.squared, 4), "\n")

# Calculate residuals and test normality
unarmored_df$resid_original <- residuals(model_original)
unarmored_df$resid_adjusted <- residuals(model_adjusted)

cat("\n\nResidual Analysis:\n\n")
cat("Model 1 (original):\n")
cat("  Mean:", round(mean(unarmored_df$resid_original), 3), "\n")
cat("  SD:", round(sd(unarmored_df$resid_original), 3), "\n")
cat("  Shapiro p:", format(shapiro.test(unarmored_df$resid_original)$p.value, digits = 4), "\n")

cat("\nModel 2 (adjusted):\n")
cat("  Mean:", round(mean(unarmored_df$resid_adjusted), 3), "\n")
cat("  SD:", round(sd(unarmored_df$resid_adjusted), 3), "\n")
cat("  Shapiro p:", format(shapiro.test(unarmored_df$resid_adjusted)$p.value, digits = 4), "\n")

###############################################################################
# Step 4: Check if skin effects are significant
###############################################################################

cat("\n\n=== STEP 4: ARE SKIN EFFECTS SIGNIFICANT? ===\n\n")

# ANOVA comparing models
anova_result <- anova(model_original, model_with_skin)
cat("ANOVA: Model 1 vs Model 4 (with skin):\n")
print(anova_result)

cat("\n\nInterpretation: If p < 0.05, adding skin significantly improves the model,\n")
cat("which would support the hypothesis that skin-specific resists matter.\n")

###############################################################################
# Step 5: Test the CLEAN formula with skin adjustment
###############################################################################

cat("\n\n=== STEP 5: TEST CLEAN FORMULA WITH SKIN ADJUSTMENT ===\n\n")

# Apply original clean formula
unarmored_df$pred_clean <- 9 +
  0.01 * unarmored_df$hardiness -
  0.02 * unarmored_df$fortitude +
  0.01 * unarmored_df$dexterity +
  0.01 * unarmored_df$intellect +
  0.025 * unarmored_df$cleverness +
  0.015 * unarmored_df$power +
  0.12 * unarmored_df$kinen +
  0.06 * unarmored_df$nonkinen

unarmored_df$resid_clean <- unarmored_df$level - unarmored_df$pred_clean

# Apply formula with adjusted resists
unarmored_df$pred_adjusted <- 9 +
  0.01 * unarmored_df$hardiness -
  0.02 * unarmored_df$fortitude +
  0.01 * unarmored_df$dexterity +
  0.01 * unarmored_df$intellect +
  0.025 * unarmored_df$cleverness +
  0.015 * unarmored_df$power +
  0.12 * unarmored_df$kinen_adjusted +
  0.06 * unarmored_df$nonkinen_adjusted

unarmored_df$resid_adjusted_clean <- unarmored_df$level - unarmored_df$pred_adjusted

cat("Clean formula with original resists:\n")
cat("  R²:", round(1 - sum(unarmored_df$resid_clean^2) /
                   sum((unarmored_df$level - mean(unarmored_df$level))^2), 4), "\n")
cat("  Mean residual:", round(mean(unarmored_df$resid_clean), 3), "\n")
cat("  SD residual:", round(sd(unarmored_df$resid_clean), 3), "\n")
cat("  Shapiro p:", format(shapiro.test(unarmored_df$resid_clean)$p.value, digits = 4), "\n")

cat("\nClean formula with adjusted resists:\n")
cat("  R²:", round(1 - sum(unarmored_df$resid_adjusted_clean^2) /
                   sum((unarmored_df$level - mean(unarmored_df$level))^2), 4), "\n")
cat("  Mean residual:", round(mean(unarmored_df$resid_adjusted_clean), 3), "\n")
cat("  SD residual:", round(sd(unarmored_df$resid_adjusted_clean), 3), "\n")
cat("  Shapiro p:", format(shapiro.test(unarmored_df$resid_adjusted_clean)$p.value, digits = 4), "\n")

###############################################################################
# Step 6: Check residuals by skin
###############################################################################

cat("\n\n=== STEP 6: RESIDUALS BY SKIN ===\n\n")

skin_resid_comparison <- unarmored_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    resid_original = mean(resid_clean),
    resid_adjusted = mean(resid_adjusted_clean),
    improvement = abs(resid_original) - abs(resid_adjusted),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  arrange(desc(improvement))

cat("Skin-level improvement from using adjusted resists:\n\n")
print(skin_resid_comparison, n = 30)

###############################################################################
# Step 7: Alternative hypothesis - maybe base resist IS used in level calc
###############################################################################

cat("\n\n=== STEP 7: ALTERNATIVE - BASE RESIST IN LEVEL FORMULA? ===\n\n")

cat("What if the level formula uses BOTH the base resist AND fortitude contribution?\n\n")

# Model with base resist as separate term
model_with_base <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + base_kinen + base_nonkinen +
          kinen_from_fort,
  data = unarmored_df
)

cat("Model with base_kinen and base_nonkinen as separate terms:\n")
cat("  R²:", round(summary(model_with_base)$r.squared, 4), "\n\n")
cat("Coefficients:\n")
print(round(coef(model_with_base), 4))

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# Plot residuals before and after adjustment
p1 <- ggplot(unarmored_df, aes(x = pred_clean, y = resid_clean)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", color = "darkred") +
  labs(title = "Residuals: Original Clean Formula",
       x = "Predicted Level", y = "Residual") +
  theme_minimal()
print(p1)

p2 <- ggplot(unarmored_df, aes(x = pred_adjusted, y = resid_adjusted_clean)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", color = "darkblue") +
  labs(title = "Residuals: Adjusted Resist Formula",
       x = "Predicted Level", y = "Residual") +
  theme_minimal()
print(p2)

# Compare skin base resist vs original residual
p3 <- ggplot(unarmored_df, aes(x = base_kinen, y = resid_clean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Skin Base Resist vs Level Residual",
       subtitle = "If correlated, base resist affects level",
       x = "Skin Base Kinen Resist",
       y = "Original Formula Residual") +
  theme_minimal()
print(p3)

cor_base_resid <- cor(unarmored_df$base_kinen, unarmored_df$resid_clean)
cat("\nCorrelation between skin base resist and level residual:", round(cor_base_resid, 3), "\n")

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("HYPOTHESIS: Skin-specific base resists affect creature level\n\n")

r2_orig <- summary(model_original)$r.squared
r2_skin <- summary(model_with_skin)$r.squared

if (r2_skin > r2_orig + 0.01) {
  cat("SUPPORTED: Adding skin improves R² from", round(r2_orig, 4),
      "to", round(r2_skin, 4), "\n")
} else {
  cat("NOT STRONGLY SUPPORTED: Adding skin only improves R² from",
      round(r2_orig, 4), "to", round(r2_skin, 4), "\n")
}

cat("\nCorrelation between base_kinen and level residual:", round(cor_base_resid, 3), "\n")
if (abs(cor_base_resid) > 0.2) {
  cat("  → Base resist IS correlated with level prediction error\n")
  cat("  → Suggests base resist may affect level calculation\n")
} else {
  cat("  → Base resist is NOT strongly correlated with prediction error\n")
  cat("  → Level formula may already account for total resist correctly\n")
}
