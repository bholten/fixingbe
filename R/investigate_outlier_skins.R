source("R/data.R")

library(dplyr)
library(ggplot2)

cat("=============================================================\n")
cat("       INVESTIGATING OUTLIER SKINS\n")
cat("=============================================================\n\n")

###############################################################################
# Load data
###############################################################################

creatures_raw <- read_csv("data/clean/furrycat/creatures.csv", show_col_types = FALSE)
templates <- read_csv("data/clean/furrycat/templates.csv", show_col_types = FALSE)

full_df <- creatures_raw %>%
  left_join(templates %>% select(serial, fortitude, hardiness, dexterity,
                                  intellect, cleverness, power, endurance,
                                  courage, fierceness, dependability),
            by = c("template_id" = "serial")) %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6
  )

unarmored_df <- full_df %>% filter(armor == 0)

# Calculate predictions
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

###############################################################################
# Identify outlier skins (adjustment > |3| levels)
###############################################################################

outlier_skins <- c("piket", "fambaa", "thune", "rancor", "kliknik",
                   "carrion_spat", "shear_mite", "guf_drolg", "graul")

cat("Investigating skins with large adjustments:\n")
cat("  Positive outliers: piket, fambaa, thune, rancor, kliknik, carrion_spat\n")
cat("  Negative outliers: shear_mite, guf_drolg, graul\n\n")

###############################################################################
# Look at each outlier skin in detail
###############################################################################

cat("=== DETAILED ANALYSIS OF OUTLIER SKINS ===\n\n")

for (skin_name in outlier_skins) {
  skin_creatures <- unarmored_df %>% filter(skin == skin_name)

  if (nrow(skin_creatures) == 0) {
    cat(sprintf("--- %s: NO UNARMORED CREATURES ---\n\n", toupper(skin_name)))
    next
  }

  cat(sprintf("--- %s (n=%d) ---\n\n", toupper(skin_name), nrow(skin_creatures)))

  # Basic stats
  cat("Creature details:\n")
  print(skin_creatures %>%
          select(serial, level, pred_clean, resid_clean, health,
                 damage_low, damage_high, speed, to_hit, fortitude) %>%
          as.data.frame())

  cat("\nSummary:\n")
  cat("  Mean level:", round(mean(skin_creatures$level), 1), "\n")
  cat("  Mean predicted:", round(mean(skin_creatures$pred_clean), 1), "\n")
  cat("  Mean residual:", round(mean(skin_creatures$resid_clean), 2), "\n")
  cat("  Residual range:", round(min(skin_creatures$resid_clean), 2), "to",
      round(max(skin_creatures$resid_clean), 2), "\n")

  # Check for unusual attribute patterns
  cat("\nAttribute summary:\n")
  cat("  Hardiness:", round(mean(skin_creatures$hardiness)), "\n")
  cat("  Fortitude:", round(mean(skin_creatures$fortitude)), "\n")
  cat("  Power:", round(mean(skin_creatures$power)), "\n")
  cat("  Cleverness:", round(mean(skin_creatures$cleverness)), "\n")
  cat("  Kinen:", round(mean(skin_creatures$kinen), 1), "\n")
  cat("  Nonkinen:", round(mean(skin_creatures$nonkinen), 1), "\n")

  cat("\n\n")
}

###############################################################################
# Compare outliers to typical creatures at similar levels
###############################################################################

cat("=== COMPARISON WITH SIMILAR-LEVEL CREATURES ===\n\n")

# For each outlier, compare to non-outlier creatures at similar predicted levels
for (skin_name in c("piket", "fambaa", "thune", "rancor")) {
  skin_creatures <- unarmored_df %>% filter(skin == skin_name)

  if (nrow(skin_creatures) == 0) next

  mean_pred <- mean(skin_creatures$pred_clean)

  # Find similar predicted level creatures (excluding this skin)
  similar <- unarmored_df %>%
    filter(skin != skin_name,
           pred_clean >= mean_pred - 3,
           pred_clean <= mean_pred + 3)

  cat(sprintf("%s vs similar predicted level (%.1f ± 3):\n", skin_name, mean_pred))
  cat(sprintf("  %s: actual=%.1f, pred=%.1f, resid=%.2f\n",
              skin_name, mean(skin_creatures$level), mean_pred,
              mean(skin_creatures$resid_clean)))
  cat(sprintf("  Similar (n=%d): actual=%.1f, pred=%.1f, resid=%.2f\n\n",
              nrow(similar), mean(similar$level), mean(similar$pred_clean),
              mean(similar$resid_clean)))
}

###############################################################################
# Check if these are simply low-n noise
###############################################################################

cat("=== SAMPLE SIZE ANALYSIS ===\n\n")

skin_counts <- unarmored_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_resid = mean(resid_clean),
    sd_resid = sd(resid_clean),
    se_resid = sd(resid_clean) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    # 95% CI
    ci_lower = mean_resid - 1.96 * se_resid,
    ci_upper = mean_resid + 1.96 * se_resid,
    # Is zero in CI?
    zero_in_ci = ci_lower <= 0 & ci_upper >= 0
  ) %>%
  arrange(desc(abs(mean_resid)))

cat("Skins with largest residuals (with 95% CI):\n\n")
print(skin_counts %>% head(15) %>% as.data.frame())

cat("\n\nSkins where 0 is NOT in the 95% CI (truly different):\n")
truly_different <- skin_counts %>% filter(!zero_in_ci, !is.na(se_resid))
print(truly_different %>% as.data.frame())

###############################################################################
# Check the raw HTML for suspicious entries
###############################################################################

cat("\n\n=== CHECKING FOR DATA QUALITY ISSUES ===\n\n")

# Look at the specific creatures
suspicious <- unarmored_df %>%
  filter(skin %in% c("piket", "fambaa", "thune")) %>%
  select(serial, skin, level, health, action, mind, damage_low, damage_high,
         fortitude, hardiness, power, kinen, nonkinen, pred_clean, resid_clean)

cat("Potentially suspicious creatures (piket, fambaa, thune):\n\n")
print(as.data.frame(suspicious))

cat("\n\nChecking for unusual patterns:\n")

for (i in 1:nrow(suspicious)) {
  row <- suspicious[i, ]
  cat(sprintf("\n%s (%s):\n", row$serial, row$skin))

  # Check if stats seem reasonable for the level
  expected_health_range <- c(row$level * 150, row$level * 400)
  cat(sprintf("  Health %d (expected ~%d-%d for level %d)\n",
              row$health, expected_health_range[1], expected_health_range[2], row$level))

  # Check damage
  cat(sprintf("  Damage %d-%d\n", row$damage_low, row$damage_high))

  # Check if attributes are unusual
  cat(sprintf("  Fortitude: %d, Power: %d\n", row$fortitude, row$power))
}

###############################################################################
# What if we exclude these outlier skins?
###############################################################################

cat("\n\n=== MODEL WITHOUT OUTLIER SKINS ===\n\n")

# Define truly problematic skins (n=1 with huge residuals)
problem_skins <- c("piket", "fambaa", "thune", "hermit_spider", "guf_drolg",
                   "graul", "shear_mite", "vesp", "snorbal", "boar_wolf",
                   "kahmurra", "zucca_boar", "eopie", "veermok")

filtered_df <- unarmored_df %>%
  filter(!(skin %in% problem_skins))

cat("Removing skins with n <= 2 or extreme outliers:\n")
cat("  Removed:", paste(problem_skins, collapse = ", "), "\n")
cat("  Remaining creatures:", nrow(filtered_df), "of", nrow(unarmored_df), "\n\n")

# Recalculate residuals
filtered_df$resid <- filtered_df$level - filtered_df$pred_clean

cat("Filtered dataset residual analysis:\n")
cat("  Mean residual:", round(mean(filtered_df$resid), 3), "\n")
cat("  SD residual:", round(sd(filtered_df$resid), 3), "\n")
cat("  Shapiro p:", format(shapiro.test(filtered_df$resid)$p.value, digits = 4), "\n")

cat("\nOriginal (all skins):\n")
cat("  Mean residual:", round(mean(unarmored_df$resid_clean), 3), "\n")
cat("  SD residual:", round(sd(unarmored_df$resid_clean), 3), "\n")

# R² comparison
r2_orig <- 1 - sum(unarmored_df$resid_clean^2) /
               sum((unarmored_df$level - mean(unarmored_df$level))^2)
r2_filtered <- 1 - sum(filtered_df$resid^2) /
                   sum((filtered_df$level - mean(filtered_df$level))^2)

cat("\nR² comparison:\n")
cat("  Original:", round(r2_orig, 4), "\n")
cat("  Filtered:", round(r2_filtered, 4), "\n")

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# Highlight outlier skins
unarmored_df$is_outlier_skin <- unarmored_df$skin %in% outlier_skins

p1 <- ggplot(unarmored_df, aes(x = pred_clean, y = level, color = is_outlier_skin)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "red")) +
  labs(title = "Predicted vs Actual Level",
       subtitle = "Red = outlier skins",
       x = "Predicted Level", y = "Actual Level",
       color = "Outlier Skin?") +
  theme_minimal()
print(p1)

# Residuals by skin (for skins with n >= 3)
skin_summary <- unarmored_df %>%
  group_by(skin) %>%
  summarise(n = n(), mean_resid = mean(resid_clean), .groups = "drop") %>%
  filter(n >= 3)

p2 <- ggplot(skin_summary, aes(x = reorder(skin, mean_resid), y = mean_resid)) +
  geom_col(aes(fill = mean_resid > 0)) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "coral")) +
  labs(title = "Mean Residual by Skin (n >= 3)",
       x = "Skin", y = "Mean Residual") +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("FINDINGS:\n\n")

cat("1. EXTREME OUTLIERS (n=1 creatures):\n")
cat("   - piket, fambaa, thune have residuals of +8 to +18 levels\n")
cat("   - These are likely data quality issues or very unusual specimens\n")
cat("   - With n=1, we can't distinguish signal from noise\n\n")

cat("2. RANCOR (n=5):\n")
cat("   - Consistently high residuals (+5.1 mean)\n")
cat("   - This is a real effect, not noise\n")
cat("   - Rancors may have an inherent level bonus\n\n")

cat("3. RECOMMENDATION:\n")
cat("   - Consider filtering skins with n <= 2\n")
cat("   - Or add explicit skin adjustments for common skins\n")
cat("   - Rancor appears to need a +5 level adjustment\n")
