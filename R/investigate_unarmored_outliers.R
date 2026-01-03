source("R/data.R")

library(dplyr)
library(ggplot2)
library(tidyr)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6
  )

no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=============================================================\n")
cat("       INVESTIGATING UNARMORED OUTLIERS\n")
cat("=============================================================\n\n")

###############################################################################
# Calculate residuals from clean formula
###############################################################################

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
# Identify outliers
###############################################################################

cat("=== OUTLIER IDENTIFICATION ===\n\n")

# Define outliers as |residual| > 2*SD
sd_resid <- sd(no_armor_df$residual)
mean_resid <- mean(no_armor_df$residual)

no_armor_df$is_outlier <- abs(no_armor_df$residual - mean_resid) > 2 * sd_resid

cat("Residual statistics:\n")
cat("  Mean:", round(mean_resid, 2), "\n")
cat("  SD:  ", round(sd_resid, 2), "\n")
cat("  Outlier threshold: |residual -", round(mean_resid, 2), "| >", round(2 * sd_resid, 2), "\n")
cat("  Outliers:", sum(no_armor_df$is_outlier), "of", nrow(no_armor_df),
    sprintf("(%.1f%%)\n\n", 100 * sum(no_armor_df$is_outlier) / nrow(no_armor_df)))

###############################################################################
# Examine positive outliers (actual level >> predicted)
###############################################################################

cat("=== POSITIVE OUTLIERS (actual >> predicted) ===\n\n")

positive_outliers <- no_armor_df %>%
  filter(residual > mean_resid + 2 * sd_resid) %>%
  arrange(desc(residual)) %>%
  select(serial, skin, level, pred, residual, fortitude, kinen, nonkinen, hardiness, power)

cat("Count:", nrow(positive_outliers), "\n\n")

if (nrow(positive_outliers) > 0) {
  cat("Top positive outliers (level much higher than predicted):\n")
  print(positive_outliers %>% head(15), n = 15)

  cat("\n\nSkin distribution of positive outliers:\n")
  print(table(positive_outliers$skin))
}

###############################################################################
# Examine negative outliers (actual level << predicted)
###############################################################################

cat("\n\n=== NEGATIVE OUTLIERS (actual << predicted) ===\n\n")

negative_outliers <- no_armor_df %>%
  filter(residual < mean_resid - 2 * sd_resid) %>%
  arrange(residual) %>%
  select(serial, skin, level, pred, residual, fortitude, kinen, nonkinen, hardiness, power)

cat("Count:", nrow(negative_outliers), "\n\n")

if (nrow(negative_outliers) > 0) {
  cat("Top negative outliers (level much lower than predicted):\n")
  print(negative_outliers %>% head(15), n = 15)

  cat("\n\nSkin distribution of negative outliers:\n")
  print(table(negative_outliers$skin))
}

###############################################################################
# Analyze the low-level overprediction pattern
###############################################################################

cat("\n\n=== LOW LEVEL ANALYSIS (pred < 15) ===\n\n")

low_level <- no_armor_df %>% filter(pred < 15)

cat("Creatures with predicted level < 15:", nrow(low_level), "\n")
cat("Mean residual:", round(mean(low_level$residual), 2), "\n\n")

cat("Breakdown by actual level:\n")
low_level %>%
  group_by(level) %>%
  summarise(
    n = n(),
    mean_pred = round(mean(pred), 1),
    mean_resid = round(mean(residual), 2),
    .groups = "drop"
  ) %>%
  arrange(level) %>%
  print(n = 30)

cat("\n\nSkin distribution of low-level creatures:\n")
print(table(low_level$skin))

###############################################################################
# Check for skin-specific patterns
###############################################################################

cat("\n\n=== RESIDUALS BY SKIN TYPE ===\n\n")

skin_summary <- no_armor_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_level = round(mean(level), 1),
    mean_pred = round(mean(pred), 1),
    mean_resid = round(mean(residual), 2),
    sd_resid = round(sd(residual), 2),
    .groups = "drop"
  ) %>%
  arrange(mean_resid)

cat("Skins with NEGATIVE mean residual (we're overpredicting):\n")
print(skin_summary %>% filter(mean_resid < -1 & n >= 3), n = 30)

cat("\n\nSkins with POSITIVE mean residual (we're underpredicting):\n")
print(skin_summary %>% filter(mean_resid > 1 & n >= 3), n = 30)

###############################################################################
# Check for fortitude-based patterns
###############################################################################

cat("\n\n=== RESIDUALS BY FORTITUDE RANGE ===\n\n")

no_armor_df %>%
  mutate(fort_bucket = cut(fortitude,
                           breaks = c(0, 100, 200, 300, 400, 500),
                           include.lowest = TRUE)) %>%
  group_by(fort_bucket) %>%
  summarise(
    n = n(),
    mean_level = round(mean(level), 1),
    mean_resid = round(mean(residual), 2),
    sd_resid = round(sd(residual), 2),
    .groups = "drop"
  ) %>%
  print()

###############################################################################
# Check for kinen (kinetic/energy resist) patterns
###############################################################################

cat("\n\n=== RESIDUALS BY KINEN (Kinetic/Energy Resist) ===\n\n")

no_armor_df %>%
  mutate(kinen_bucket = cut(kinen,
                            breaks = c(-100, -50, 0, 20, 40, 60),
                            include.lowest = TRUE)) %>%
  group_by(kinen_bucket) %>%
  summarise(
    n = n(),
    mean_level = round(mean(level), 1),
    mean_resid = round(mean(residual), 2),
    sd_resid = round(sd(residual), 2),
    .groups = "drop"
  ) %>%
  print()

###############################################################################
# Look for possible additional skin minimums
###############################################################################

cat("\n\n=== POSSIBLE ADDITIONAL SKIN MINIMUMS ===\n\n")

cat("These are skins where the minimum level in our data matches\n")
cat("a creature with very high stats (suggesting a skin minimum):\n\n")

skin_mins <- no_armor_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    min_level = min(level),
    max_level = max(level),
    n_at_min = sum(level == min(level)),
    .groups = "drop"
  ) %>%
  filter(n >= 3, n_at_min >= 2) %>%
  arrange(min_level)

for (i in 1:nrow(skin_mins)) {
  sk <- skin_mins$skin[i]
  min_lvl <- skin_mins$min_level[i]

  min_creatures <- no_armor_df %>%
    filter(skin == sk, level == min_lvl)

  if (nrow(min_creatures) >= 2) {
    # Check if these min-level creatures have high predicted levels
    if (mean(min_creatures$pred) > min_lvl + 3) {
      cat(sprintf("%s: %d creatures at level %d (pred avg: %.1f)\n",
                  sk, nrow(min_creatures), min_lvl, mean(min_creatures$pred)))
    }
  }
}

###############################################################################
# Examine creatures at predicted level 10-15 with negative residuals
###############################################################################

cat("\n\n=== MID-LEVEL UNDERPREDICTION (pred 15-25, level 10-20) ===\n\n")

mid_under <- no_armor_df %>%
  filter(pred >= 15, pred <= 25, residual < -2) %>%
  arrange(residual) %>%
  select(serial, skin, level, pred, residual, fortitude, kinen, nonkinen)

cat("Count:", nrow(mid_under), "\n\n")
print(mid_under %>% head(20), n = 20)

cat("\n\nSkin distribution:\n")
print(table(mid_under$skin))

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATIONS ===\n")

# Residuals colored by skin (top skins only)
top_skins <- no_armor_df %>%
  count(skin) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  pull(skin)

p1 <- no_armor_df %>%
  mutate(skin_group = ifelse(skin %in% top_skins, skin, "Other")) %>%
  ggplot(aes(x = pred, y = residual, color = skin_group)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(-2*sd_resid, 2*sd_resid) + mean_resid,
             linetype = "dotted", color = "red") +
  labs(
    title = "Unarmored Residuals by Skin Type",
    x = "Predicted Level",
    y = "Residual",
    color = "Skin"
  ) +
  theme_minimal()
print(p1)

# Residuals vs fortitude
p2 <- ggplot(no_armor_df, aes(x = fortitude, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fortitude",
    subtitle = "Looking for non-linear fortitude effects",
    x = "Fortitude",
    y = "Residual"
  ) +
  theme_minimal()
print(p2)

# Residuals vs kinen
p3 <- ggplot(no_armor_df, aes(x = kinen, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Kinetic/Energy Resist",
    subtitle = "Looking for non-linear resist effects",
    x = "Kinen (avg kinetic + energy)",
    y = "Residual"
  ) +
  theme_minimal()
print(p3)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("Key patterns found:\n\n")

cat("1. OUTLIER COUNT:", sum(no_armor_df$is_outlier), "of", nrow(no_armor_df), "creatures\n\n")

cat("2. SKIN-SPECIFIC EFFECTS:\n")
problem_skins <- skin_summary %>% filter(abs(mean_resid) > 2, n >= 3)
if (nrow(problem_skins) > 0) {
  for (i in 1:nrow(problem_skins)) {
    cat(sprintf("   - %s: mean residual %.2f (n=%d)\n",
                problem_skins$skin[i], problem_skins$mean_resid[i], problem_skins$n[i]))
  }
}

cat("\n3. LEVEL-DEPENDENT BIAS:\n")
cat("   - Low predicted (<15): mean residual ~positive (underpredicting level)\n")
cat("   - Mid predicted (15-25): mean residual ~negative (overpredicting level)\n")
