source("R/data.R")

library(mgcv)
library(ggplot2)
library(dplyr)
library(broom)
library(lmtest)

# Feature engineering (from creature_level.R)
normalized_df <- normalized_df %>%
  mutate(
    average_hdi = (hardiness + dexterity + intellect) / 3,
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    # Derived combat stats
    avg_damage = (damage_low + damage_high) / 2,
    dps = avg_damage * speed * to_hit,
    average_ham = (health + action + mind) / 3
  )

# Split by armor status
armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=== DATASET SIZES ===\n")
cat("Armored creatures:", nrow(armor_df), "\n")
cat("Unarmored creatures:", nrow(no_armor_df), "\n\n")

###############################################################################
# PART 1: ARMORED CREATURES - Compare raw attributes vs final stats
###############################################################################

cat("=== PART 1: ARMORED CREATURES ===\n\n")

# Model A: Raw attributes (baseline from creature_level_analysis.Rmd)
model.armor.raw <- lm(
  level ~ average_hdi + fortitude + cleverness + power + kinen + nonkinen,
  data = armor_df
)

# Model B: Final stats (HAM + damage components + resists)
model.armor.final <- lm(
  level ~ health + action + mind +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = armor_df
)

# Model C: Final stats simplified (average HAM + DPS + resists)
model.armor.final.simple <- lm(
  level ~ average_ham + dps + kinen + nonkinen,
  data = armor_df
)

# Model D: Final stats with fortitude (maybe armor rating matters separately?)
model.armor.final.fort <- lm(
  level ~ health + action + mind +
          damage_low + damage_high + speed + to_hit +
          fortitude + kinen + nonkinen,
  data = armor_df
)

cat("--- Model Comparison (Armored) ---\n\n")

cat("Model A (Raw Attributes):\n")
cat("  R² =", summary(model.armor.raw)$r.squared, "\n")
cat("  Adj R² =", summary(model.armor.raw)$adj.r.squared, "\n")
cat("  Residual SE =", summary(model.armor.raw)$sigma, "\n\n")

cat("Model B (Final Stats - Full):\n")
cat("  R² =", summary(model.armor.final)$r.squared, "\n")
cat("  Adj R² =", summary(model.armor.final)$adj.r.squared, "\n")
cat("  Residual SE =", summary(model.armor.final)$sigma, "\n\n")

cat("Model C (Final Stats - Simple: avg_ham + dps):\n")
cat("  R² =", summary(model.armor.final.simple)$r.squared, "\n")
cat("  Adj R² =", summary(model.armor.final.simple)$adj.r.squared, "\n")
cat("  Residual SE =", summary(model.armor.final.simple)$sigma, "\n\n")

cat("Model D (Final Stats + Fortitude):\n")
cat("  R² =", summary(model.armor.final.fort)$r.squared, "\n")
cat("  Adj R² =", summary(model.armor.final.fort)$adj.r.squared, "\n")
cat("  Residual SE =", summary(model.armor.final.fort)$sigma, "\n\n")

# Print coefficients for the best-looking model
cat("--- Coefficients (Model D - Final Stats + Fortitude) ---\n")
print(summary(model.armor.final.fort))

# Diagnostic tests for Model D
cat("\n--- Diagnostics for Model D ---\n")
cat("Shapiro-Wilk (normality of residuals):\n")
print(shapiro.test(resid(model.armor.final.fort)))
cat("\nBreusch-Pagan (homoscedasticity):\n")
print(bptest(model.armor.final.fort))

# Visualization
p1 <- ggplot(armor_df, aes(x = predict(model.armor.final.fort), y = level)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Armored: Final Stats + Fortitude Model",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p1)


###############################################################################
# PART 2: UNARMORED CREATURES - The harder case
###############################################################################

cat("\n\n=== PART 2: UNARMORED CREATURES ===\n\n")

# Model A: Raw attributes (baseline)
model.noarmor.raw <- lm(
  level ~ average_hdi + fortitude + cleverness + power + kinen + nonkinen,
  data = no_armor_df
)

# Model B: Final stats
model.noarmor.final <- lm(
  level ~ health + action + mind +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = no_armor_df
)

# Model C: Final stats simplified
model.noarmor.final.simple <- lm(
  level ~ average_ham + dps + kinen + nonkinen,
  data = no_armor_df
)

cat("--- Model Comparison (Unarmored) ---\n\n")

cat("Model A (Raw Attributes):\n")
cat("  R² =", summary(model.noarmor.raw)$r.squared, "\n")
cat("  Adj R² =", summary(model.noarmor.raw)$adj.r.squared, "\n")
cat("  Residual SE =", summary(model.noarmor.raw)$sigma, "\n")
cat("  BP test p-value =", bptest(model.noarmor.raw)$p.value, "(heteroscedastic if < 0.05)\n\n")

cat("Model B (Final Stats - Full):\n")
cat("  R² =", summary(model.noarmor.final)$r.squared, "\n")
cat("  Adj R² =", summary(model.noarmor.final)$adj.r.squared, "\n")
cat("  Residual SE =", summary(model.noarmor.final)$sigma, "\n")
cat("  BP test p-value =", bptest(model.noarmor.final)$p.value, "(heteroscedastic if < 0.05)\n\n")

cat("Model C (Final Stats - Simple):\n")
cat("  R² =", summary(model.noarmor.final.simple)$r.squared, "\n")
cat("  Adj R² =", summary(model.noarmor.final.simple)$adj.r.squared, "\n")
cat("  Residual SE =", summary(model.noarmor.final.simple)$sigma, "\n")
cat("  BP test p-value =", bptest(model.noarmor.final.simple)$p.value, "(heteroscedastic if < 0.05)\n\n")

# Full summary of the final stats model
cat("--- Coefficients (Model B - Final Stats) ---\n")
print(summary(model.noarmor.final))

# Visualization: compare raw vs final stats
p2 <- ggplot(no_armor_df, aes(x = predict(model.noarmor.raw), y = level)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Unarmored: Raw Attributes Model",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p2)

p3 <- ggplot(no_armor_df, aes(x = predict(model.noarmor.final), y = level)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Unarmored: Final Stats Model",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p3)


###############################################################################
# PART 3: GAM exploration - look for non-linearities in final stats
###############################################################################

cat("\n\n=== PART 3: GAM EXPLORATION (Final Stats) ===\n\n")

# GAM on armored with final stats
model.gam.armor.final <- gam(
  level ~ s(health) + s(action) + s(mind) +
          s(avg_damage) + s(speed) + s(to_hit) +
          s(kinen) + s(nonkinen),
  data = armor_df
)
cat("--- GAM Summary (Armored - Final Stats) ---\n")
summary(model.gam.armor.final)

# GAM on unarmored with final stats
model.gam.noarmor.final <- gam(
  level ~ s(health) + s(action) + s(mind) +
          s(avg_damage) + s(speed) + s(to_hit) +
          s(kinen) + s(nonkinen),
  data = no_armor_df
)
cat("\n--- GAM Summary (Unarmored - Final Stats) ---\n")
summary(model.gam.noarmor.final)

# Plot the smooth terms to see if relationships are linear
cat("\nPlotting GAM smooth terms for unarmored creatures...\n")
cat("(Look for edf close to 1.0 = linear relationship)\n")
plot(model.gam.noarmor.final, pages = 1, main = "Unarmored: GAM Smooth Terms (Final Stats)")


###############################################################################
# PART 4: Check if coefficients look "round" (game-dev friendly)
###############################################################################

cat("\n\n=== PART 4: COEFFICIENT ANALYSIS ===\n\n")

cat("Looking for 'round' coefficients that a game dev might use...\n\n")

# For armored creatures, what multipliers make sense?
coef_armor <- coef(model.armor.final.fort)
cat("Armored model coefficients:\n")
for (name in names(coef_armor)) {
  val <- coef_armor[name]
  # Check if it's close to simple fractions
  cat(sprintf("  %s: %.6f", name, val))
  # Test some common denominators
  for (denom in c(100, 200, 500, 1000)) {
    rounded <- round(val * denom) / denom
    if (abs(val - rounded) < 0.001) {
      cat(sprintf(" ≈ %d/%d", round(val * denom), denom))
      break
    }
  }
  cat("\n")
}

cat("\n\nUnarmored model coefficients:\n")
coef_noarmor <- coef(model.noarmor.final)
for (name in names(coef_noarmor)) {
  val <- coef_noarmor[name]
  cat(sprintf("  %s: %.6f\n", name, val))
}


###############################################################################
# PART 5: Residual analysis by level range
###############################################################################

cat("\n\n=== PART 5: RESIDUALS BY LEVEL RANGE ===\n\n")

no_armor_df <- no_armor_df %>%
  mutate(
    predicted_final = predict(model.noarmor.final, newdata = no_armor_df),
    residual_final = level - predicted_final,
    level_bucket = cut(level, breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
                       labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "30+"))
  )

residual_summary <- no_armor_df %>%
  group_by(level_bucket) %>%
  summarise(
    n = n(),
    mean_residual = mean(residual_final),
    sd_residual = sd(residual_final),
    .groups = "drop"
  )

cat("Residual summary by level range (unarmored, final stats model):\n")
print(residual_summary)

cat("\nIf SD increases with level, that suggests multiplicative error (percentage-based).\n")
cat("If SD is constant, that suggests additive error (fixed rounding).\n")
