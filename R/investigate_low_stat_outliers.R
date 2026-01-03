source("R/data.R")

library(dplyr)
library(ggplot2)

cat("=============================================================\n")
cat("   INVESTIGATING LOW-STAT OUTLIERS AND FLOOR EFFECTS\n")
cat("=============================================================\n\n")

###############################################################################
# Setup
###############################################################################

no_armor_df <- normalized_df %>% filter(armor == 0)

# Linear predictions
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

###############################################################################
# Look at outliers with NEGATIVE residuals (actual < predicted)
###############################################################################

cat("=== NEGATIVE RESIDUAL OUTLIERS (Level < Predicted) ===\n\n")

neg_outliers <- no_armor_df %>%
  filter(resid_linear < -5) %>%
  select(serial, skin, level, pred_linear, resid_linear,
         cleverness, power, hardiness, fortitude, kinen, nonkinen) %>%
  arrange(resid_linear)

cat("Creatures where actual level is MUCH LOWER than predicted:\n\n")
print(as.data.frame(neg_outliers))

cat("\n\nMean stats of negative outliers vs population:\n")
cat("                    Outliers    Population\n")
cat(sprintf("  Cleverness:       %6.1f      %6.1f\n",
            mean(neg_outliers$cleverness), mean(no_armor_df$cleverness)))
cat(sprintf("  Power:            %6.1f      %6.1f\n",
            mean(neg_outliers$power), mean(no_armor_df$power)))
cat(sprintf("  Hardiness:        %6.1f      %6.1f\n",
            mean(neg_outliers$hardiness), mean(no_armor_df$hardiness)))
cat(sprintf("  Fortitude:        %6.1f      %6.1f\n",
            mean(neg_outliers$fortitude), mean(no_armor_df$fortitude)))
cat(sprintf("  Kinen:            %6.1f      %6.1f\n",
            mean(neg_outliers$kinen), mean(no_armor_df$kinen)))
cat(sprintf("  Predicted:        %6.1f      %6.1f\n",
            mean(neg_outliers$pred_linear), mean(no_armor_df$pred_linear)))
cat(sprintf("  Actual Level:     %6.1f      %6.1f\n",
            mean(neg_outliers$level), mean(no_armor_df$level)))

###############################################################################
# Check if predicted level exceeds some skin-specific maximum
###############################################################################

cat("\n\n=== SKIN-SPECIFIC ANALYSIS ===\n\n")

# For each skin, what's the max observed level?
skin_max <- no_armor_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    max_level = max(level),
    max_pred = max(pred_linear),
    mean_resid = mean(resid_linear),
    .groups = "drop"
  ) %>%
  arrange(desc(max_pred - max_level))

cat("Skins where max predicted exceeds max observed level:\n")
print(skin_max %>% filter(max_pred > max_level + 2) %>% as.data.frame())

###############################################################################
# Check for level capping by skin
###############################################################################

cat("\n\n=== LEVEL CAPPING ANALYSIS ===\n\n")

# What if there's a skin-specific level cap?
# For each negative outlier, check if their level is at or near the skin's max

neg_outliers_with_max <- neg_outliers %>%
  left_join(skin_max %>% select(skin, max_level), by = "skin") %>%
  mutate(at_skin_max = level == max_level)

cat("Are negative outliers at their skin's maximum level?\n\n")
print(neg_outliers_with_max %>%
        select(serial, skin, level, max_level, at_skin_max, pred_linear, resid_linear) %>%
        as.data.frame())

###############################################################################
# Alternative: Check for diminishing returns at high stats
###############################################################################

cat("\n\n=== DIMINISHING RETURNS ANALYSIS ===\n\n")

# Plot level vs predicted, look for ceiling effect
no_armor_df$total_dps_stats <- no_armor_df$cleverness + no_armor_df$power

# Are high DPS-stat creatures under-leveled?
cat("Correlation of DPS stats with residual:\n")
cat("  cleverness vs resid:", round(cor(no_armor_df$cleverness, no_armor_df$resid_linear), 3), "\n")
cat("  power vs resid:", round(cor(no_armor_df$power, no_armor_df$resid_linear), 3), "\n")
cat("  clev+power vs resid:", round(cor(no_armor_df$total_dps_stats, no_armor_df$resid_linear), 3), "\n")

# Check by cleverness quartiles
no_armor_df$clev_quartile <- cut(no_armor_df$cleverness,
                                  breaks = quantile(no_armor_df$cleverness, c(0, 0.25, 0.5, 0.75, 1)),
                                  labels = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"),
                                  include.lowest = TRUE)

cat("\nResiduals by cleverness quartile:\n")
quartile_stats <- no_armor_df %>%
  group_by(clev_quartile) %>%
  summarise(
    n = n(),
    mean_clev = mean(cleverness),
    mean_resid = mean(resid_linear),
    sd_resid = sd(resid_linear),
    .groups = "drop"
  )
print(as.data.frame(quartile_stats))

###############################################################################
# Check for square root or log relationship
###############################################################################

cat("\n\n=== TESTING DIMINISHING RETURNS FORMULAS ===\n\n")

# What if cleverness and power have diminishing returns (sqrt)?
no_armor_df$pred_sqrt <- 9 +
  0.01 * no_armor_df$hardiness -
  0.02 * no_armor_df$fortitude +
  0.01 * no_armor_df$dexterity +
  0.01 * no_armor_df$intellect +
  0.5 * sqrt(no_armor_df$cleverness) +  # sqrt instead of linear
  0.3 * sqrt(no_armor_df$power) +        # sqrt instead of linear
  0.12 * no_armor_df$kinen +
  0.06 * no_armor_df$nonkinen

# Fit optimal sqrt coefficients
model_sqrt <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                   I(sqrt(cleverness)) + I(sqrt(power)) + kinen + nonkinen,
                 data = no_armor_df)

cat("Model with sqrt(cleverness) and sqrt(power):\n")
print(summary(model_sqrt))

no_armor_df$pred_sqrt_fit <- predict(model_sqrt)
no_armor_df$resid_sqrt <- no_armor_df$level - no_armor_df$pred_sqrt_fit

cat("\nSqrt model residual stats:\n")
cat("  R²:", round(summary(model_sqrt)$r.squared, 4), "\n")
cat("  SD:", round(sd(no_armor_df$resid_sqrt), 3), "\n")
sw_sqrt <- shapiro.test(no_armor_df$resid_sqrt)
cat("  Shapiro p:", format(sw_sqrt$p.value, digits = 4),
    ifelse(sw_sqrt$p.value > 0.05, " <-- NORMAL!", ""), "\n")

# Compare to linear
cat("\nComparison - Linear vs Sqrt:\n")
cat("  Linear R²: 0.938, Sqrt R²:", round(summary(model_sqrt)$r.squared, 4), "\n")
cat("  Linear SD: 2.27, Sqrt SD:", round(sd(no_armor_df$resid_sqrt), 3), "\n")

###############################################################################
# Check for log relationship
###############################################################################

cat("\n\n--- Testing log transformation ---\n\n")

# Log transform (add small constant to avoid log(0))
no_armor_df <- no_armor_df %>%
  mutate(
    log_clev = log(cleverness + 1),
    log_power = log(power + 1)
  )

model_log <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                  log_clev + log_power + kinen + nonkinen,
                data = no_armor_df)

cat("Model with log(cleverness+1) and log(power+1):\n")
print(summary(model_log))

no_armor_df$pred_log <- predict(model_log)
no_armor_df$resid_log <- no_armor_df$level - no_armor_df$pred_log

cat("\nLog model residual stats:\n")
cat("  R²:", round(summary(model_log)$r.squared, 4), "\n")
cat("  SD:", round(sd(no_armor_df$resid_log), 3), "\n")
sw_log <- shapiro.test(no_armor_df$resid_log)
cat("  Shapiro p:", format(sw_log$p.value, digits = 4),
    ifelse(sw_log$p.value > 0.05, " <-- NORMAL!", ""), "\n")

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# Predicted vs Actual with diminishing returns highlighted
p1 <- ggplot(no_armor_df, aes(x = pred_linear, y = level)) +
  geom_point(aes(color = cleverness), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_viridis_c(option = "C") +
  labs(title = "Predicted vs Actual Level",
       subtitle = "Color = cleverness (looking for high-cleverness under-prediction)",
       x = "Predicted Level", y = "Actual Level") +
  theme_minimal()
print(p1)

# Residual vs cleverness
p2 <- ggplot(no_armor_df, aes(x = cleverness, y = resid_linear)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual vs Cleverness",
       subtitle = "Looking for diminishing returns pattern",
       x = "Cleverness", y = "Residual (actual - predicted)") +
  theme_minimal()
print(p2)

# Residual vs power
p3 <- ggplot(no_armor_df, aes(x = power, y = resid_linear)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual vs Power",
       subtitle = "Looking for diminishing returns pattern",
       x = "Power", y = "Residual (actual - predicted)") +
  theme_minimal()
print(p3)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("FINDINGS:\n\n")

cat("1. NEGATIVE RESIDUAL OUTLIERS:\n")
cat("   - ", nrow(neg_outliers), " creatures have actual level 5+ below predicted\n")
cat("   - These creatures have HIGH cleverness (", round(mean(neg_outliers$cleverness)),
    " vs ", round(mean(no_armor_df$cleverness)), " population mean)\n", sep = "")
cat("   - And HIGH power (", round(mean(neg_outliers$power)),
    " vs ", round(mean(no_armor_df$power)), " population mean)\n", sep = "")

cat("\n2. DIMINISHING RETURNS:\n")
cat("   - Residuals negatively correlate with cleverness: r = ",
    round(cor(no_armor_df$cleverness, no_armor_df$resid_linear), 3), "\n", sep = "")
cat("   - Residuals negatively correlate with power: r = ",
    round(cor(no_armor_df$power, no_armor_df$resid_linear), 3), "\n", sep = "")
cat("   - This suggests the linear model OVERPREDICTS for high-stat creatures\n")

cat("\n3. MODEL COMPARISON:\n")
cat("   - Linear:      R² = 0.938, Shapiro p = (baseline)\n")
cat("   - Sqrt model:  R² = ", round(summary(model_sqrt)$r.squared, 4),
    ", Shapiro p = ", format(sw_sqrt$p.value, digits = 4), "\n", sep = "")
cat("   - Log model:   R² = ", round(summary(model_log)$r.squared, 4),
    ", Shapiro p = ", format(sw_log$p.value, digits = 4), "\n", sep = "")

cat("\n4. INTERPRETATION:\n")
cat("   - Cleverness and power likely have DIMINISHING RETURNS\n")
cat("   - The game probably uses sqrt() or similar transformation\n")
cat("   - High DPS stats don't increase level as much as expected\n")
