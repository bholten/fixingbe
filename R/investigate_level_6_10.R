source("R/data.R")

library(dplyr)
library(ggplot2)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    average_hdi = (hardiness + dexterity + intellect) / 3,
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    avg_damage = (damage_low + damage_high) / 2,
    dps = avg_damage * speed * to_hit,
    average_ham = (health + action + mind) / 3
  )

no_armor_df <- normalized_df %>% filter(armor == 0)

# Build the model to get predictions
model.noarmor.final <- lm(
  level ~ health + action + mind +
          damage_low + damage_high + speed + to_hit +
          kinen + nonkinen,
  data = no_armor_df
)

no_armor_df <- no_armor_df %>%
  mutate(
    predicted_level = predict(model.noarmor.final, newdata = no_armor_df),
    residual = level - predicted_level
  )

###############################################################################
# Get skin minimum levels from the full dataset
###############################################################################

skin_mins <- no_armor_df %>%
  group_by(skin) %>%
  summarise(
    min_level = min(level),
    max_level = max(level),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(min_level)

cat("=== ALL SKIN MINIMUM LEVELS (Unarmored) ===\n\n")
print(skin_mins, n = 50)

###############################################################################
# Investigate creatures at level 6-10
###############################################################################

cat("\n\n=== LEVEL 6-10 CREATURES ===\n\n")

level_6_10 <- no_armor_df %>%
  filter(level >= 6 & level <= 10) %>%
  arrange(level, skin)

cat("Count:", nrow(level_6_10), "creatures\n\n")

# Check which are at their skin minimum
level_6_10_with_min <- level_6_10 %>%
  left_join(skin_mins, by = "skin") %>%
  mutate(at_skin_minimum = (level == min_level)) %>%
  select(serial, skin, level, min_level, at_skin_minimum, predicted_level, residual) %>%
  arrange(at_skin_minimum, residual)

cat("--- Creatures at skin minimum vs not ---\n")
level_6_10_with_min %>%
  group_by(at_skin_minimum) %>%
  summarise(
    n = n(),
    mean_residual = mean(residual),
    sd_residual = sd(residual),
    .groups = "drop"
  ) %>%
  print()

cat("\n--- Creatures AT skin minimum (potential contamination) ---\n")
at_min <- level_6_10_with_min %>% filter(at_skin_minimum)
print(at_min, n = 50)

cat("\n--- Skins where level 6-10 IS the minimum ---\n")
skins_with_min_6_10 <- skin_mins %>%
  filter(min_level >= 6 & min_level <= 10)
print(skins_with_min_6_10, n = 30)

###############################################################################
# Deeper analysis: residuals by whether at skin minimum
###############################################################################

cat("\n\n=== RESIDUAL ANALYSIS ===\n\n")

cat("--- All level 6-10 creatures with large negative residuals (< -2) ---\n")
large_neg_residual <- level_6_10_with_min %>%
  filter(residual < -2) %>%
  arrange(residual)
print(large_neg_residual, n = 30)

cat("\n--- Skin distribution of large negative residuals ---\n")
large_neg_residual %>%
  count(skin, at_skin_minimum, sort = TRUE) %>%
  print(n = 20)

###############################################################################
# Compare stats: at minimum vs not at minimum
###############################################################################

cat("\n\n=== STAT COMPARISON: AT MINIMUM vs NOT ===\n\n")

level_6_10_full <- level_6_10 %>%
  left_join(skin_mins, by = "skin") %>%
  mutate(at_skin_minimum = (level == min_level))

comparison <- level_6_10_full %>%
  group_by(at_skin_minimum) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    mean_health = mean(health),
    mean_action = mean(action),
    mean_mind = mean(mind),
    mean_avg_damage = mean(avg_damage),
    mean_to_hit = mean(to_hit),
    mean_kinen = mean(kinen),
    mean_nonkinen = mean(nonkinen),
    mean_predicted = mean(predicted_level),
    mean_residual = mean(residual),
    .groups = "drop"
  )
print(comparison)

###############################################################################
# Visualize
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

no_armor_df <- no_armor_df %>%
  left_join(skin_mins %>% select(skin, min_level), by = "skin") %>%
  mutate(
    at_skin_minimum = (level == min_level),
    level_group = case_when(
      level <= 10 & at_skin_minimum ~ "6-10 at skin min",
      level <= 10 ~ "6-10 not at min",
      TRUE ~ "11+"
    )
  )

p1 <- ggplot(no_armor_df, aes(x = predicted_level, y = level, color = level_group)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c(
    "6-10 at skin min" = "red",
    "6-10 not at min" = "orange",
    "11+" = "gray50"
  )) +
  labs(
    title = "Unarmored: Predicted vs Actual Level",
    subtitle = "Red = level 6-10 at skin minimum, Orange = level 6-10 not at minimum",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p1)

###############################################################################
# Summary recommendation
###############################################################################

cat("\n\n=== SUMMARY ===\n\n")

n_at_min <- sum(level_6_10_with_min$at_skin_minimum)
n_total <- nrow(level_6_10_with_min)
mean_res_at_min <- mean(level_6_10_with_min$residual[level_6_10_with_min$at_skin_minimum])
mean_res_not_min <- mean(level_6_10_with_min$residual[!level_6_10_with_min$at_skin_minimum])

cat(sprintf("Level 6-10 creatures at skin minimum: %d / %d (%.1f%%)\n",
            n_at_min, n_total, 100 * n_at_min / n_total))
cat(sprintf("Mean residual (at minimum): %.2f\n", mean_res_at_min))
cat(sprintf("Mean residual (not at minimum): %.2f\n", mean_res_not_min))

if (n_at_min > 0 && mean_res_at_min < mean_res_not_min - 1) {
  cat("\n** RECOMMENDATION: Filter out creatures at skin minimum for these skins: **\n")
  skins_to_filter <- level_6_10_with_min %>%
    filter(at_skin_minimum) %>%
    distinct(skin, min_level) %>%
    arrange(skin)
  print(skins_to_filter, n = 30)
}
