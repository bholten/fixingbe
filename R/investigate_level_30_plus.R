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
# Get skin info
###############################################################################

skin_stats <- no_armor_df %>%
  group_by(skin) %>%
  summarise(
    min_level = min(level),
    max_level = max(level),
    count = n(),
    .groups = "drop"
  )

###############################################################################
# Investigate creatures at level 30+
###############################################################################

cat("=== LEVEL 30+ CREATURES ===\n\n")

level_30_plus <- no_armor_df %>%
  filter(level >= 30) %>%
  left_join(skin_stats, by = "skin") %>%
  mutate(at_skin_maximum = (level == max_level)) %>%
  arrange(desc(residual))

cat("Count:", nrow(level_30_plus), "creatures\n\n")

cat("--- Level Distribution ---\n")
level_30_plus %>%
  count(level) %>%
  print(n = 20)

cat("\n--- Skin Distribution ---\n")
level_30_plus %>%
  count(skin, sort = TRUE) %>%
  print(n = 20)

cat("\n--- Basic Info (sorted by residual, descending) ---\n")
level_30_plus %>%
  select(serial, skin, level, max_level, at_skin_maximum, predicted_level, residual) %>%
  print(n = 40)

cat("\n--- Creatures with large positive residuals (> 2) ---\n")
large_pos <- level_30_plus %>%
  filter(residual > 2) %>%
  select(serial, skin, level, predicted_level, residual, health, action, mind,
         avg_damage, to_hit, kinen, nonkinen)
print(large_pos, n = 30)

cat("\n--- Are high-residual creatures at their skin's maximum level? ---\n")
level_30_plus %>%
  group_by(at_skin_maximum) %>%
  summarise(
    n = n(),
    mean_residual = mean(residual),
    sd_residual = sd(residual),
    .groups = "drop"
  ) %>%
  print()

###############################################################################
# Compare stats: high residual vs normal
###############################################################################

cat("\n\n=== STAT COMPARISON: RESIDUAL > 2 vs RESIDUAL <= 2 ===\n\n")

level_30_plus <- level_30_plus %>%
  mutate(high_residual = residual > 2)

comparison <- level_30_plus %>%
  group_by(high_residual) %>%
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
# Check for patterns in the high-residual creatures
###############################################################################

cat("\n\n=== PATTERN ANALYSIS ===\n\n")

cat("--- Resist profile of high-residual creatures ---\n")
high_res_creatures <- level_30_plus %>% filter(residual > 2)

high_res_creatures %>%
  select(serial, skin, level, kinetic, energy, blast, heat, cold, electricity, acid, stun) %>%
  print(n = 20)

cat("\n--- Do high-residual creatures have lower resists (vuln stacking)? ---\n")
level_30_plus %>%
  group_by(high_residual) %>%
  summarise(
    mean_kinen = mean(kinen),
    mean_nonkinen = mean(nonkinen),
    mean_kinetic = mean(kinetic),
    mean_energy = mean(energy),
    min_nonkinen = min(nonkinen),
    .groups = "drop"
  ) %>%
  print()

###############################################################################
# Raw attributes comparison
###############################################################################

cat("\n\n=== RAW ATTRIBUTES OF HIGH-RESIDUAL CREATURES ===\n\n")

high_res_creatures %>%
  select(serial, skin, level, residual, hardiness, fortitude, dexterity,
         intellect, cleverness, power, courage) %>%
  print(n = 20)

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

no_armor_df <- no_armor_df %>%
  mutate(
    level_group = case_when(
      level >= 30 & residual > 2 ~ "30+ high residual",
      level >= 30 ~ "30+ normal",
      TRUE ~ "Below 30"
    )
  )

p1 <- ggplot(no_armor_df, aes(x = predicted_level, y = level, color = level_group)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c(
    "30+ high residual" = "red",
    "30+ normal" = "orange",
    "Below 30" = "gray50"
  )) +
  labs(
    title = "Unarmored: Predicted vs Actual Level",
    subtitle = "Red = level 30+ with residual > 2",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p1)

###############################################################################
# Summary
###############################################################################

cat("\n\n=== SUMMARY ===\n\n")

n_high_res <- sum(level_30_plus$residual > 2)
n_total <- nrow(level_30_plus)
mean_res_high <- mean(level_30_plus$residual[level_30_plus$residual > 2])
mean_res_normal <- mean(level_30_plus$residual[level_30_plus$residual <= 2])

cat(sprintf("Level 30+ creatures with residual > 2: %d / %d (%.1f%%)\n",
            n_high_res, n_total, 100 * n_high_res / n_total))
cat(sprintf("Mean residual (high): %.2f\n", mean_res_high))
cat(sprintf("Mean residual (normal): %.2f\n", mean_res_normal))

cat("\n--- Possible explanations for under-prediction ---\n")
cat("1. Skin maximum levels (like skin minimums but at the top)\n")
cat("2. Non-linear relationship at high stat values\n")
cat("3. A cap or ceiling effect in the level formula\n")
cat("4. Different formula for high-level creatures\n")
