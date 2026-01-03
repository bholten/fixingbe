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
cat("       RESIDUAL DISTRIBUTION ANALYSIS\n")
cat("=============================================================\n\n")

cat("Sample sizes:\n")
cat("  Armored:", nrow(armor_df), "\n")
cat("  Unarmored:", nrow(no_armor_df), "\n\n")

###############################################################################
# Calculate residuals from clean formulas
###############################################################################

# Armored clean formula
armor_df$pred <- -23 +
  0.01 * armor_df$hardiness +
  0.06 * armor_df$fortitude +
  0.005 * armor_df$dexterity +
  0.01 * armor_df$intellect +
  0.025 * armor_df$cleverness +
  0.015 * armor_df$power +
  0.1 * armor_df$kinen +
  0.08 * armor_df$nonkinen

armor_df$residual <- armor_df$level - armor_df$pred

# Unarmored clean formula
no_armor_df$pred <- 9 +
  0.01 * no_armor_df$hardiness -
  0.02 * no_armor_df$fortitude +
  0.01 * no_armor_df$dexterity +
  0.01 * no_armor_df$intellect +
  0.025 * no_armor_df$cleverness +
  0.015 * no_armor_df$power +
  0.12 * no_armor_df$kinen +
  0.06 * no_armor_df$nonkinen

no_armor_df$residual <- no_armor_df$level - no_armor_df$pred

###############################################################################
# Basic residual statistics
###############################################################################

cat("=== RESIDUAL SUMMARY STATISTICS ===\n\n")

cat("ARMORED:\n")
cat("  Mean:     ", round(mean(armor_df$residual), 3), "\n")
cat("  Median:   ", round(median(armor_df$residual), 3), "\n")
cat("  SD:       ", round(sd(armor_df$residual), 3), "\n")
cat("  Min:      ", round(min(armor_df$residual), 3), "\n")
cat("  Max:      ", round(max(armor_df$residual), 3), "\n")
cat("  IQR:      ", round(IQR(armor_df$residual), 3), "\n")
# Manual skewness and kurtosis calculations
calc_skewness <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^3) / (n * s^3)
}

calc_kurtosis <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^4) / (n * s^4)
}

cat("  Skewness: ", round(calc_skewness(armor_df$residual), 3), "\n")
cat("  Kurtosis: ", round(calc_kurtosis(armor_df$residual), 3), "(normal = 3)\n")

cat("\nUNARMORED:\n")
cat("  Mean:     ", round(mean(no_armor_df$residual), 3), "\n")
cat("  Median:   ", round(median(no_armor_df$residual), 3), "\n")
cat("  SD:       ", round(sd(no_armor_df$residual), 3), "\n")
cat("  Min:      ", round(min(no_armor_df$residual), 3), "\n")
cat("  Max:      ", round(max(no_armor_df$residual), 3), "\n")
cat("  IQR:      ", round(IQR(no_armor_df$residual), 3), "\n")
cat("  Skewness: ", round(calc_skewness(no_armor_df$residual), 3), "\n")
cat("  Kurtosis: ", round(calc_kurtosis(no_armor_df$residual), 3), "(normal = 3)\n")

###############################################################################
# Normality tests
###############################################################################

cat("\n\n=== NORMALITY TESTS ===\n\n")

# Shapiro-Wilk test (best for small-medium samples)
cat("Shapiro-Wilk Test (H0: data is normally distributed):\n\n")

sw_armor <- shapiro.test(armor_df$residual)
cat("  ARMORED:\n")
cat("    W statistic:", round(sw_armor$statistic, 4), "\n")
cat("    p-value:    ", format(sw_armor$p.value, digits = 4), "\n")
cat("    Conclusion: ", ifelse(sw_armor$p.value > 0.05,
                               "NORMAL (fail to reject H0)",
                               "NOT normal (reject H0)"), "\n")

sw_noarmor <- shapiro.test(no_armor_df$residual)
cat("\n  UNARMORED:\n")
cat("    W statistic:", round(sw_noarmor$statistic, 4), "\n")
cat("    p-value:    ", format(sw_noarmor$p.value, digits = 4), "\n")
cat("    Conclusion: ", ifelse(sw_noarmor$p.value > 0.05,
                               "NORMAL (fail to reject H0)",
                               "NOT normal (reject H0)"), "\n")

# Kolmogorov-Smirnov test (alternative)
cat("\n\nKolmogorov-Smirnov Test (comparing to normal distribution):\n\n")

ks_armor <- ks.test(armor_df$residual, "pnorm",
                    mean = mean(armor_df$residual),
                    sd = sd(armor_df$residual))
cat("  ARMORED:\n")
cat("    D statistic:", round(ks_armor$statistic, 4), "\n")
cat("    p-value:    ", format(ks_armor$p.value, digits = 4), "\n")

ks_noarmor <- ks.test(no_armor_df$residual, "pnorm",
                      mean = mean(no_armor_df$residual),
                      sd = sd(no_armor_df$residual))
cat("\n  UNARMORED:\n")
cat("    D statistic:", round(ks_noarmor$statistic, 4), "\n")
cat("    p-value:    ", format(ks_noarmor$p.value, digits = 4), "\n")

###############################################################################
# Residual distribution by integer levels
###############################################################################

cat("\n\n=== RESIDUAL DISTRIBUTION BY ROUNDING ===\n\n")

cat("If the game rounds to nearest integer, residuals should cluster\n")
cat("around discrete values. Let's check the distribution:\n\n")

# Round residuals to nearest 0.5
armor_df$residual_rounded <- round(armor_df$residual * 2) / 2
no_armor_df$residual_rounded <- round(no_armor_df$residual * 2) / 2

cat("ARMORED - Residual frequency (rounded to 0.5):\n")
armor_resid_table <- table(armor_df$residual_rounded)
print(armor_resid_table)

cat("\nUNARMORED - Residual frequency (rounded to 0.5):\n")
noarmor_resid_table <- table(no_armor_df$residual_rounded)
print(noarmor_resid_table)

###############################################################################
# Check for heteroscedasticity
###############################################################################

cat("\n\n=== HETEROSCEDASTICITY CHECK ===\n\n")

cat("Residual SD by predicted level bucket:\n\n")

cat("ARMORED:\n")
armor_df %>%
  mutate(pred_bucket = cut(pred, breaks = c(30, 40, 45, 50, 55, 60, 70),
                           include.lowest = TRUE)) %>%
  group_by(pred_bucket) %>%
  summarise(
    n = n(),
    mean_resid = round(mean(residual), 2),
    sd_resid = round(sd(residual), 2),
    .groups = "drop"
  ) %>%
  print()

cat("\nUNARMORED:\n")
no_armor_df %>%
  mutate(pred_bucket = cut(pred, breaks = c(0, 10, 15, 20, 25, 30, 50),
                           include.lowest = TRUE)) %>%
  group_by(pred_bucket) %>%
  summarise(
    n = n(),
    mean_resid = round(mean(residual), 2),
    sd_resid = round(sd(residual), 2),
    .groups = "drop"
  ) %>%
  print()

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATIONS ===\n")

# Histograms with normal curve overlay
p1 <- ggplot(armor_df, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5,
                 fill = "blue", alpha = 0.6, color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(armor_df$residual),
                           sd = sd(armor_df$residual)),
                color = "red", linewidth = 1) +
  labs(
    title = "Armored Residual Distribution",
    subtitle = sprintf("Mean=%.2f, SD=%.2f, Shapiro p=%.4f",
                       mean(armor_df$residual), sd(armor_df$residual),
                       sw_armor$p.value),
    x = "Residual (Actual - Predicted Level)",
    y = "Density"
  ) +
  theme_minimal()
print(p1)

p2 <- ggplot(no_armor_df, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5,
                 fill = "red", alpha = 0.6, color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(no_armor_df$residual),
                           sd = sd(no_armor_df$residual)),
                color = "darkred", linewidth = 1) +
  labs(
    title = "Unarmored Residual Distribution",
    subtitle = sprintf("Mean=%.2f, SD=%.2f, Shapiro p=%.4f",
                       mean(no_armor_df$residual), sd(no_armor_df$residual),
                       sw_noarmor$p.value),
    x = "Residual (Actual - Predicted Level)",
    y = "Density"
  ) +
  theme_minimal()
print(p2)

# Q-Q plots
p3 <- ggplot(armor_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "Armored Residuals - Q-Q Plot",
    subtitle = "Points should follow line if normally distributed",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()
print(p3)

p4 <- ggplot(no_armor_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "Unarmored Residuals - Q-Q Plot",
    subtitle = "Points should follow line if normally distributed",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()
print(p4)

# Residuals vs Predicted (check for patterns)
p5 <- ggplot(armor_df, aes(x = pred, y = residual)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Armored: Residuals vs Predicted Level",
    subtitle = "Flat loess line suggests no systematic pattern",
    x = "Predicted Level",
    y = "Residual"
  ) +
  theme_minimal()
print(p5)

p6 <- ggplot(no_armor_df, aes(x = pred, y = residual)) +
  geom_point(alpha = 0.6, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  labs(
    title = "Unarmored: Residuals vs Predicted Level",
    subtitle = "Flat loess line suggests no systematic pattern",
    x = "Predicted Level",
    y = "Residual"
  ) +
  theme_minimal()
print(p6)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("ARMORED CREATURES:\n")
cat("  - Residuals have mean", round(mean(armor_df$residual), 2),
    "and SD", round(sd(armor_df$residual), 2), "\n")
cat("  - Shapiro-Wilk p-value:", format(sw_armor$p.value, digits = 4), "\n")
if (sw_armor$p.value > 0.05) {
  cat("  - CONCLUSION: Residuals ARE normally distributed!\n")
  cat("    This strongly suggests the remaining error is due to\n")
  cat("    randomness in the crafting system itself.\n")
} else {
  cat("  - CONCLUSION: Residuals are NOT normally distributed.\n")
  cat("    There may be additional structure we haven't captured.\n")
}

cat("\nUNARMORED CREATURES:\n")
cat("  - Residuals have mean", round(mean(no_armor_df$residual), 2),
    "and SD", round(sd(no_armor_df$residual), 2), "\n")
cat("  - Shapiro-Wilk p-value:", format(sw_noarmor$p.value, digits = 4), "\n")
if (sw_noarmor$p.value > 0.05) {
  cat("  - CONCLUSION: Residuals ARE normally distributed!\n")
  cat("    This strongly suggests the remaining error is due to\n")
  cat("    randomness in the crafting system itself.\n")
} else {
  cat("  - CONCLUSION: Residuals are NOT normally distributed.\n")
  cat("    There may be additional structure we haven't captured,\n")
  cat("    OR the non-normality could be due to rounding effects\n")
  cat("    at low levels where integer rounding is proportionally larger.\n")
}
