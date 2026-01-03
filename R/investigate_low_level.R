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
# Investigate creatures at level 1-5
###############################################################################

cat("=== LOW LEVEL CREATURES (1-5) ===\n\n")

low_level <- no_armor_df %>%
  filter(level <= 5) %>%
  arrange(level)

cat("Count:", nrow(low_level), "creatures\n\n")

cat("--- Basic Info ---\n")
low_level %>%
  select(serial, skin, level, predicted_level, residual) %>%
  print(n = 20)

cat("\n--- Skin Distribution ---\n")
low_level %>%
  count(skin, sort = TRUE) %>%
  print(n = 20)

cat("\n--- Level Distribution ---\n")
low_level %>%
  count(level) %>%
  print()

cat("\n--- Key Stats ---\n")
low_level %>%
  select(serial, skin, level, health, action, mind, damage_low, damage_high,
         speed, to_hit, kinen, nonkinen, fortitude) %>%
  print(n = 20)

cat("\n--- Raw Attributes ---\n")
low_level %>%
  select(serial, level, hardiness, fortitude, dexterity, endurance,
         intellect, cleverness, dependability, courage, fierceness, power) %>%
  print(n = 20)

cat("\n--- Resistances ---\n")
low_level %>%
  select(serial, level, kinetic, energy, blast, heat, cold, electricity, acid, stun) %>%
  print(n = 20)

###############################################################################
# Compare to creatures at level 6-10 (next bucket)
###############################################################################

cat("\n\n=== COMPARISON: LEVEL 6-10 CREATURES ===\n\n")

mid_level <- no_armor_df %>%
  filter(level >= 6 & level <= 10)

cat("Count:", nrow(mid_level), "creatures\n\n")

# Summary statistics comparison
cat("--- Stat Comparison (Mean) ---\n")
comparison <- bind_rows(
  low_level %>%
    summarise(
      group = "1-5",
      n = n(),
      health = mean(health),
      action = mean(action),
      mind = mean(mind),
      avg_damage = mean(avg_damage),
      speed = mean(speed),
      to_hit = mean(to_hit),
      kinen = mean(kinen),
      nonkinen = mean(nonkinen),
      fortitude = mean(fortitude),
      avg_residual = mean(residual)
    ),
  mid_level %>%
    summarise(
      group = "6-10",
      n = n(),
      health = mean(health),
      action = mean(action),
      mind = mean(mind),
      avg_damage = mean(avg_damage),
      speed = mean(speed),
      to_hit = mean(to_hit),
      kinen = mean(kinen),
      nonkinen = mean(nonkinen),
      fortitude = mean(fortitude),
      avg_residual = mean(residual)
    )
)
print(comparison)

###############################################################################
# Check if these are at skin minimum levels
###############################################################################

cat("\n\n=== SKIN MINIMUM LEVEL CHECK ===\n\n")

# Get all creatures by skin and find the minimum level for each skin
skin_mins <- normalized_df %>%
  group_by(skin) %>%
  summarise(
    min_level = min(level),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(min_level)

cat("Skins with minimum level <= 5:\n")
skin_mins %>%
  filter(min_level <= 5) %>%
  print(n = 30)

cat("\n--- Are the low-level creatures at their skin's minimum? ---\n")
low_level_with_skin_min <- low_level %>%
  left_join(skin_mins, by = "skin") %>%
  mutate(at_skin_minimum = (level == min_level)) %>%
  select(serial, skin, level, min_level, at_skin_minimum, predicted_level, residual)

print(low_level_with_skin_min, n = 20)

cat("\nCreatures at skin minimum:", sum(low_level_with_skin_min$at_skin_minimum), "/", nrow(low_level_with_skin_min), "\n")

###############################################################################
# Visualize where these fall
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

no_armor_df <- no_armor_df %>%
  mutate(level_group = case_when(
    level <= 5 ~ "1-5 (investigate)",
    level <= 10 ~ "6-10",
    TRUE ~ "11+"
  ))

p1 <- ggplot(no_armor_df, aes(x = predicted_level, y = level, color = level_group)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("1-5 (investigate)" = "red", "6-10" = "orange", "11+" = "gray50")) +
  labs(
    title = "Unarmored: Predicted vs Actual Level",
    subtitle = "Red points are level 1-5 creatures",
    x = "Predicted Level",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p1)

# Where do the low-level creatures fall in stat space?
p2 <- ggplot(no_armor_df, aes(x = average_ham, y = level, color = level_group)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("1-5 (investigate)" = "red", "6-10" = "orange", "11+" = "gray50")) +
  labs(
    title = "Average HAM vs Level",
    subtitle = "Red points are level 1-5 creatures",
    x = "Average HAM (health+action+mind)/3",
    y = "Level"
  ) +
  theme_minimal()
print(p2)

p3 <- ggplot(no_armor_df, aes(x = kinen, y = level, color = level_group)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("1-5 (investigate)" = "red", "6-10" = "orange", "11+" = "gray50")) +
  labs(
    title = "Kinetic/Energy Resist vs Level",
    subtitle = "Red points are level 1-5 creatures",
    x = "kinen (kinetic+energy)/2",
    y = "Level"
  ) +
  theme_minimal()
print(p3)
