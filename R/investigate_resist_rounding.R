source("R/data.R")

library(dplyr)
library(ggplot2)

# Load raw creature data with special/effective
creatures_raw <- read_csv("data/clean/furrycat/creatures.csv", show_col_types = FALSE)

cat("=============================================================\n")
cat("       INVESTIGATING RESIST ROUNDING\n")
cat("=============================================================\n\n")

###############################################################################
# Join creatures with templates to get fortitude
###############################################################################

templates <- read_csv("data/clean/furrycat/templates.csv", show_col_types = FALSE)

df <- creatures_raw %>%
  left_join(templates %>% select(serial, fortitude),
            by = c("template_id" = "serial"))

cat("Dataset: ", nrow(df), "creatures with fortitude data\n\n")

###############################################################################
# Focus on creatures with special kinetic resist
###############################################################################

special_df <- df %>%
  filter(kinetic.special > 0) %>%
  mutate(
    # Expected if special = fortitude / 10
    expected_fort10 = fortitude / 10,
    diff_from_fort10 = kinetic.special - expected_fort10,

    # What if it's rounded?
    expected_rounded = round(fortitude / 10),
    diff_from_rounded = kinetic.special - expected_rounded,

    # What about floor or ceiling?
    expected_floor = floor(fortitude / 10),
    expected_ceil = ceiling(fortitude / 10),
    diff_from_floor = kinetic.special - expected_floor,
    diff_from_ceil = kinetic.special - expected_ceil
  )

cat("=== ROUNDING HYPOTHESIS TEST ===\n\n")

cat("If kinetic.special = round(fortitude/10):\n")
cat("  Mean difference:", round(mean(special_df$diff_from_rounded), 3), "\n")
cat("  SD difference:", round(sd(special_df$diff_from_rounded), 3), "\n")
cat("  Exact matches:", sum(special_df$diff_from_rounded == 0), "of", nrow(special_df), "\n")

cat("\nIf kinetic.special = floor(fortitude/10):\n")
cat("  Mean difference:", round(mean(special_df$diff_from_floor), 3), "\n")
cat("  SD difference:", round(sd(special_df$diff_from_floor), 3), "\n")
cat("  Exact matches:", sum(special_df$diff_from_floor == 0), "of", nrow(special_df), "\n")

cat("\nIf kinetic.special = ceiling(fortitude/10):\n")
cat("  Mean difference:", round(mean(special_df$diff_from_ceil), 3), "\n")
cat("  SD difference:", round(sd(special_df$diff_from_ceil), 3), "\n")
cat("  Exact matches:", sum(special_df$diff_from_ceil == 0), "of", nrow(special_df), "\n")

###############################################################################
# Distribution of differences
###############################################################################

cat("\n\n=== DISTRIBUTION OF DIFFERENCES FROM round(fort/10) ===\n\n")

diff_table <- table(round(special_df$diff_from_rounded))
print(diff_table)

cat("\n\nMost common differences:\n")
sort(diff_table, decreasing = TRUE) %>% head(10) %>% print()

###############################################################################
# Look for patterns in the deviation
###############################################################################

cat("\n\n=== PATTERNS IN DEVIATION ===\n\n")

# Does the difference correlate with anything?
cat("Correlation of difference with:\n")
cat("  fortitude:", round(cor(special_df$diff_from_rounded, special_df$fortitude), 3), "\n")
cat("  level:", round(cor(special_df$diff_from_rounded, special_df$level), 3), "\n")
cat("  kinetic (final):", round(cor(special_df$diff_from_rounded, special_df$kinetic), 3), "\n")

###############################################################################
# Check if other resists show same pattern
###############################################################################

cat("\n\n=== CHECKING OTHER RESIST TYPES ===\n\n")

# For each resist type, check deviation from round(fort/10)
resist_types <- c("energy", "blast", "heat", "cold", "electricity", "acid", "stun")

for (rt in resist_types) {
  special_col <- paste0(rt, ".special")

  has_special <- df %>%
    filter(.data[[special_col]] > 0, !is.na(fortitude)) %>%
    mutate(
      expected = round(fortitude / 10),
      diff = .data[[special_col]] - expected
    )

  if (nrow(has_special) > 10) {
    cat(sprintf("%s: n=%d, mean_diff=%.1f, sd=%.1f, exact_matches=%d\n",
                rt, nrow(has_special), mean(has_special$diff),
                sd(has_special$diff), sum(has_special$diff == 0)))
  }
}

###############################################################################
# Key insight: maybe special resist has a BASE + fortitude/10?
###############################################################################

cat("\n\n=== ALTERNATIVE HYPOTHESIS: BASE + FORTITUDE/10 ===\n\n")

# Maybe special resist = base_resist + fortitude/10
# Let's estimate the base by looking at creatures with low fortitude

low_fort <- special_df %>% filter(fortitude < 100)
cat("Low fortitude creatures (fort < 100):\n")
cat("  Count:", nrow(low_fort), "\n")
if (nrow(low_fort) > 0) {
  cat("  Mean kinetic.special:", round(mean(low_fort$kinetic.special), 1), "\n")
  cat("  Mean fortitude:", round(mean(low_fort$fortitude), 1), "\n")
  cat("  Mean expected (fort/10):", round(mean(low_fort$expected_fort10), 1), "\n")
  cat("  Mean difference:", round(mean(low_fort$diff_from_fort10), 1), "\n")
}

high_fort <- special_df %>% filter(fortitude > 400)
cat("\nHigh fortitude creatures (fort > 400):\n")
cat("  Count:", nrow(high_fort), "\n")
if (nrow(high_fort) > 0) {
  cat("  Mean kinetic.special:", round(mean(high_fort$kinetic.special), 1), "\n")
  cat("  Mean fortitude:", round(mean(high_fort$fortitude), 1), "\n")
  cat("  Mean expected (fort/10):", round(mean(high_fort$expected_fort10), 1), "\n")
  cat("  Mean difference:", round(mean(high_fort$diff_from_fort10), 1), "\n")
}

###############################################################################
# Linear regression: kinetic.special ~ fortitude
###############################################################################

cat("\n\n=== REGRESSION: kinetic.special ~ fortitude ===\n\n")

model <- lm(kinetic.special ~ fortitude, data = special_df)
cat("kinetic.special = ", round(coef(model)[1], 2), " + ",
    round(coef(model)[2], 4), " * fortitude\n", sep = "")
cat("R²:", round(summary(model)$r.squared, 4), "\n")

cat("\nIf the coefficient were exactly 0.1 (i.e., fort/10):\n")
cat("  Intercept would be:", round(coef(model)[1], 2), "\n")
cat("  This suggests base resist ≈", round(coef(model)[1]), "\n")

###############################################################################
# Does the creature's SKIN determine the base resist?
###############################################################################

cat("\n\n=== SKIN-SPECIFIC BASE RESISTS? ===\n\n")

# Calculate residual from fortitude/10 for each creature
special_df$resid_from_fort10 <- special_df$kinetic.special - special_df$fortitude / 10

# Group by skin
skin_resid <- special_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_resid = mean(resid_from_fort10),
    sd_resid = sd(resid_from_fort10),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  arrange(mean_resid)

cat("Skins with consistent deviation from fort/10:\n\n")
print(skin_resid, n = 30)

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

p1 <- ggplot(special_df, aes(x = fortitude, y = kinetic.special)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 0.1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Kinetic Special Resist vs Fortitude",
    subtitle = "Red dashed = fort/10, Blue = linear fit",
    x = "Fortitude",
    y = "Kinetic Special"
  ) +
  theme_minimal()
print(p1)

p2 <- ggplot(special_df, aes(x = diff_from_rounded)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Deviation from round(fortitude/10)",
    x = "Difference (actual - expected)",
    y = "Count"
  ) +
  theme_minimal()
print(p2)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("FINDINGS:\n\n")
cat("1. kinetic.special is NOT simply round(fortitude/10)\n")
cat("   - Only", sum(special_df$diff_from_rounded == 0), "of", nrow(special_df),
    "exact matches\n\n")

cat("2. The relationship is: kinetic.special ≈", round(coef(model)[1], 1),
    "+ 0.1 * fortitude\n")
cat("   - There appears to be a BASE resist independent of fortitude\n\n")

cat("3. The base resist may vary by skin/creature type\n")
cat("   - Different skins show different average deviations\n")
