source("R/data.R")

library(dplyr)
library(ggplot2)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6
  )

armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=============================================================\n")
cat("       TESTING CLEAN COEFFICIENTS\n")
cat("=============================================================\n\n")

# Helper function to test a formula
test_formula <- function(df, intercept, hardiness, fortitude, dexterity, intellect,
                         cleverness, power, kinen, nonkinen, name) {
  pred <- intercept +
    hardiness * df$hardiness +
    fortitude * df$fortitude +
    dexterity * df$dexterity +
    intellect * df$intellect +
    cleverness * df$cleverness +
    power * df$power +
    kinen * df$kinen +
    nonkinen * df$nonkinen

  residual <- df$level - pred

  ss_res <- sum(residual^2)
  ss_tot <- sum((df$level - mean(df$level))^2)
  r2 <- 1 - ss_res / ss_tot

  cat(sprintf("%-30s R²=%.4f  SD=%.2f  Mean=%.2f  Max=%.2f\n",
              name, r2, sd(residual), mean(residual), max(abs(residual))))

  return(list(r2 = r2, sd = sd(residual), pred = pred, residual = residual))
}

###############################################################################
# ARMORED FORMULA TESTS
###############################################################################

cat("=== ARMORED FORMULAS ===\n\n")

cat("Regression coefficients (baseline):\n")
cat("  intercept=-23, har=0.009, for=0.057, dex=0.005, int=0.012,\n")
cat("  cle=0.024, pow=0.016, kin=0.1, non=0.08\n\n")

# Baseline (regression-derived, rounded)
armor_baseline <- test_formula(armor_df,
  intercept = -23,
  hardiness = 0.009,
  fortitude = 0.057,
  dexterity = 0.005,
  intellect = 0.012,
  cleverness = 0.024,
  power = 0.016,
  kinen = 0.1,
  nonkinen = 0.08,
  "Regression (rounded)")

# Test 1: Simple fractions - fortitude as 1/18
armor_test1 <- test_formula(armor_df,
  intercept = -23,
  hardiness = 0.01,      # 1/100
  fortitude = 0.055,     # ~1/18
  dexterity = 0.005,     # 1/200
  intellect = 0.01,      # 1/100
  cleverness = 0.025,    # 1/40
  power = 0.015,         # 3/200
  kinen = 0.1,           # 1/10
  nonkinen = 0.08,       # 2/25
  "Clean v1 (1/100, 1/18, etc.)")

# Test 2: Even simpler - all multiples of 0.005
armor_test2 <- test_formula(armor_df,
  intercept = -23,
  hardiness = 0.01,      # 1/100
  fortitude = 0.06,      # 3/50
  dexterity = 0.005,     # 1/200
  intellect = 0.01,      # 1/100
  cleverness = 0.025,    # 1/40
  power = 0.015,         # 3/200
  kinen = 0.1,           # 1/10
  nonkinen = 0.08,       # 2/25
  "Clean v2 (fort=0.06)")

# Test 3: Try grouping HAM stats together
armor_test3 <- test_formula(armor_df,
  intercept = -25,
  hardiness = 0.01,      # same weight
  fortitude = 0.06,      # 3/50
  dexterity = 0.01,      # same weight
  intellect = 0.01,      # same weight
  cleverness = 0.02,     # 1/50
  power = 0.02,          # 1/50
  kinen = 0.1,           # 1/10
  nonkinen = 0.08,       # 2/25
  "Clean v3 (HDI same weight)")

# Test 4: Very simple - 1/100 for most stats
armor_test4 <- test_formula(armor_df,
  intercept = -20,
  hardiness = 0.01,
  fortitude = 0.05,      # 1/20
  dexterity = 0.01,
  intellect = 0.01,
  cleverness = 0.02,
  power = 0.02,
  kinen = 0.1,
  nonkinen = 0.1,        # same as kinen
  "Clean v4 (very simple)")

cat("\nBest clean armored formula appears to be v2 or v3\n")

###############################################################################
# UNARMORED FORMULA TESTS
###############################################################################

cat("\n\n=== UNARMORED FORMULAS ===\n\n")

cat("Regression coefficients (baseline):\n")
cat("  intercept=9, har=0.012, for=-0.019, dex=0.007, int=0.011,\n")
cat("  cle=0.024, pow=0.013, kin=0.12, non=0.06\n\n")

# Baseline (regression-derived, rounded)
noarmor_baseline <- test_formula(no_armor_df,
  intercept = 9,
  hardiness = 0.012,
  fortitude = -0.019,
  dexterity = 0.007,
  intellect = 0.011,
  cleverness = 0.024,
  power = 0.013,
  kinen = 0.12,
  nonkinen = 0.06,
  "Regression (rounded)")

# Test 1: Clean fractions
noarmor_test1 <- test_formula(no_armor_df,
  intercept = 9,
  hardiness = 0.01,      # 1/100
  fortitude = -0.02,     # -1/50
  dexterity = 0.01,      # 1/100
  intellect = 0.01,      # 1/100
  cleverness = 0.025,    # 1/40
  power = 0.015,         # 3/200
  kinen = 0.12,          # 3/25
  nonkinen = 0.06,       # 3/50
  "Clean v1 (for=-1/50)")

# Test 2: HDI same weight
noarmor_test2 <- test_formula(no_armor_df,
  intercept = 8,
  hardiness = 0.01,
  fortitude = -0.02,
  dexterity = 0.01,
  intellect = 0.01,
  cleverness = 0.025,
  power = 0.015,
  kinen = 0.12,
  nonkinen = 0.06,
  "Clean v2 (HDI same, int=8)")

# Test 3: Even simpler
noarmor_test3 <- test_formula(no_armor_df,
  intercept = 10,
  hardiness = 0.01,
  fortitude = -0.02,
  dexterity = 0.01,
  intellect = 0.01,
  cleverness = 0.02,
  power = 0.01,
  kinen = 0.1,
  nonkinen = 0.05,
  "Clean v3 (very simple)")

# Test 4: Different intercept
noarmor_test4 <- test_formula(no_armor_df,
  intercept = 10,
  hardiness = 0.01,
  fortitude = -0.02,
  dexterity = 0.01,
  intellect = 0.01,
  cleverness = 0.025,
  power = 0.015,
  kinen = 0.12,
  nonkinen = 0.06,
  "Clean v4 (int=10, HDI same)")

cat("\nBest clean unarmored formula appears to be v1 or v4\n")

###############################################################################
# FINAL RECOMMENDED FORMULAS
###############################################################################

cat("\n\n=============================================================\n")
cat("              RECOMMENDED CLEAN FORMULAS\n")
cat("=============================================================\n\n")

cat("ARMORED (fortitude >= 500):\n")
cat("---------------------------\n")
cat("level = -23\n")
cat("      + 0.01   * hardiness      // 1/100\n")
cat("      + 0.06   * fortitude      // 3/50\n")
cat("      + 0.005  * dexterity      // 1/200\n")
cat("      + 0.01   * intellect      // 1/100\n")
cat("      + 0.025  * cleverness     // 1/40\n")
cat("      + 0.015  * power          // 3/200\n")
cat("      + 0.1    * kinen          // 1/10\n")
cat("      + 0.08   * nonkinen       // 2/25\n")

cat("\nValidation:\n")
armor_final <- test_formula(armor_df,
  intercept = -23,
  hardiness = 0.01,
  fortitude = 0.06,
  dexterity = 0.005,
  intellect = 0.01,
  cleverness = 0.025,
  power = 0.015,
  kinen = 0.1,
  nonkinen = 0.08,
  "FINAL ARMORED")

cat("\n\nUNARMORED (fortitude < 500):\n")
cat("----------------------------\n")
cat("level = 9\n")
cat("      + 0.01   * hardiness      // 1/100\n")
cat("      - 0.02   * fortitude      // -1/50 (NEGATIVE!)\n")
cat("      + 0.01   * dexterity      // 1/100\n")
cat("      + 0.01   * intellect      // 1/100\n")
cat("      + 0.025  * cleverness     // 1/40\n")
cat("      + 0.015  * power          // 3/200\n")
cat("      + 0.12   * kinen          // 3/25\n")
cat("      + 0.06   * nonkinen       // 3/50\n")

cat("\nValidation:\n")
noarmor_final <- test_formula(no_armor_df,
  intercept = 9,
  hardiness = 0.01,
  fortitude = -0.02,
  dexterity = 0.01,
  intellect = 0.01,
  cleverness = 0.025,
  power = 0.015,
  kinen = 0.12,
  nonkinen = 0.06,
  "FINAL UNARMORED")

###############################################################################
# COMPARISON TABLE
###############################################################################

cat("\n\n=============================================================\n")
cat("              COEFFICIENT COMPARISON TABLE\n")
cat("=============================================================\n\n")

cat(sprintf("%-12s %12s %12s %12s %12s\n",
            "Term", "Armor(reg)", "Armor(clean)", "Unarm(reg)", "Unarm(clean)"))
cat(paste(rep("-", 62), collapse = ""), "\n")
cat(sprintf("%-12s %12d %12d %12d %12d\n", "intercept", -23, -23, 9, 9))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "hardiness", 0.009, 0.01, 0.012, 0.01))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "fortitude", 0.057, 0.06, -0.019, -0.02))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "dexterity", 0.005, 0.005, 0.007, 0.01))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "intellect", 0.012, 0.01, 0.011, 0.01))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "cleverness", 0.024, 0.025, 0.024, 0.025))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "power", 0.016, 0.015, 0.013, 0.015))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "kinen", 0.1, 0.1, 0.12, 0.12))
cat(sprintf("%-12s %12.3f %12.3f %12.3f %12.3f\n", "nonkinen", 0.08, 0.08, 0.06, 0.06))

cat("\n\nR² Comparison:\n")
cat(sprintf("  Armored regression:   %.4f\n", armor_baseline$r2))
cat(sprintf("  Armored clean:        %.4f (Δ = %.4f)\n", armor_final$r2, armor_final$r2 - armor_baseline$r2))
cat(sprintf("  Unarmored regression: %.4f\n", noarmor_baseline$r2))
cat(sprintf("  Unarmored clean:      %.4f (Δ = %.4f)\n", noarmor_final$r2, noarmor_final$r2 - noarmor_baseline$r2))

###############################################################################
# FINAL PSEUDOCODE
###############################################################################

cat("\n\n=============================================================\n")
cat("              FINAL IMPLEMENTATION\n")
cat("=============================================================\n\n")

cat('
int calculateCreatureLevel(Creature* c) {
    // Compute resist averages
    float kinen = (c->kinetic + c->energy) / 2.0f;
    float nonkinen = (c->blast + c->heat + c->cold +
                      c->electricity + c->acid + c->stun) / 6.0f;

    float level;

    if (c->fortitude >= 500) {
        // ARMORED FORMULA
        level = -23.0f
              + 0.01f   * c->hardiness
              + 0.06f   * c->fortitude
              + 0.005f  * c->dexterity
              + 0.01f   * c->intellect
              + 0.025f  * c->cleverness
              + 0.015f  * c->power
              + 0.1f    * kinen
              + 0.08f   * nonkinen;
    } else {
        // UNARMORED FORMULA (note: fortitude is SUBTRACTED)
        level = 9.0f
              + 0.01f   * c->hardiness
              - 0.02f   * c->fortitude    // NEGATIVE!
              + 0.01f   * c->dexterity
              + 0.01f   * c->intellect
              + 0.025f  * c->cleverness
              + 0.015f  * c->power
              + 0.12f   * kinen
              + 0.06f   * nonkinen;
    }

    // Clamp and round
    if (level < 1.0f) level = 1.0f;
    if (level > 75.0f) level = 75.0f;
    return (int)(level + 0.5f);  // round to nearest
}
')

###############################################################################
# VISUALIZATION
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# Add predictions with clean formula
armor_df$pred_clean <- -23 +
  0.01 * armor_df$hardiness +
  0.06 * armor_df$fortitude +
  0.005 * armor_df$dexterity +
  0.01 * armor_df$intellect +
  0.025 * armor_df$cleverness +
  0.015 * armor_df$power +
  0.1 * armor_df$kinen +
  0.08 * armor_df$nonkinen

no_armor_df$pred_clean <- 9 +
  0.01 * no_armor_df$hardiness -
  0.02 * no_armor_df$fortitude +
  0.01 * no_armor_df$dexterity +
  0.01 * no_armor_df$intellect +
  0.025 * no_armor_df$cleverness +
  0.015 * no_armor_df$power +
  0.12 * no_armor_df$kinen +
  0.06 * no_armor_df$nonkinen

armor_df$type <- "Armored"
no_armor_df$type <- "Unarmored"

combined <- bind_rows(
  armor_df %>% select(level, pred_clean, type),
  no_armor_df %>% select(level, pred_clean, type)
)

p <- ggplot(combined, aes(x = pred_clean, y = level, color = type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Armored" = "blue", "Unarmored" = "red")) +
  labs(
    title = "Clean Formulas: Predicted vs Actual Level",
    subtitle = "Using game-dev friendly coefficients",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p)
