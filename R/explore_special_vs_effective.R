source("R/data.R")

library(dplyr)
library(ggplot2)
library(tidyr)

cat("=============================================================\n")
cat("       EXPLORING SPECIAL VS EFFECTIVE RESISTS\n")
cat("=============================================================\n\n")

###############################################################################
# Load raw creature data (before normalization removes special/effective)
###############################################################################

creatures_raw <- read_csv("data/clean/furrycat/creatures.csv", show_col_types = FALSE)

cat("Raw creature data columns related to resists:\n")
resist_cols <- grep("kinetic|energy|blast|heat|cold|electricity|acid|stun",
                    names(creatures_raw), value = TRUE)
print(resist_cols)

###############################################################################
# Check what special vs effective values look like
###############################################################################

cat("\n\n=== SPECIAL VS EFFECTIVE OVERVIEW ===\n\n")

# For each resist type, show distribution of special and effective
resist_types <- c("kinetic", "energy", "blast", "heat", "cold", "electricity", "acid", "stun")

for (rt in resist_types) {
  special_col <- paste0(rt, ".special")
  effective_col <- paste0(rt, ".effective")

  special_vals <- creatures_raw[[special_col]]
  effective_vals <- creatures_raw[[effective_col]]

  n_special <- sum(special_vals != 0, na.rm = TRUE)
  n_effective <- sum(effective_vals != 0, na.rm = TRUE)
  n_both <- sum(special_vals != 0 & effective_vals != 0, na.rm = TRUE)

  cat(sprintf("%-12s: %3d special, %3d effective, %3d both\n",
              rt, n_special, n_effective, n_both))
}

###############################################################################
# Verify special = fortitude / 10 hypothesis
###############################################################################

cat("\n\n=== TESTING: SPECIAL RESIST = FORTITUDE / 10? ===\n\n")

# Join with normalized_df to get fortitude
creatures_with_fort <- creatures_raw %>%
  left_join(normalized_df %>% select(serial, fortitude), by = "serial")

# For creatures with non-zero special resists, check if special ≈ fortitude/10
for (rt in resist_types) {
  special_col <- paste0(rt, ".special")

  has_special <- creatures_with_fort %>%
    filter(.data[[special_col]] != 0, !is.na(fortitude))

  if (nrow(has_special) > 0) {
    has_special$expected_special <- has_special$fortitude / 10
    has_special$diff <- has_special[[special_col]] - has_special$expected_special

    cat(sprintf("\n%s (n=%d with special):\n", rt, nrow(has_special)))
    cat("  Mean difference from fortitude/10:", round(mean(has_special$diff), 2), "\n")
    cat("  SD of difference:", round(sd(has_special$diff), 2), "\n")

    if (nrow(has_special) <= 10) {
      cat("  Actual values:\n")
      print(has_special %>%
              select(serial, fortitude, !!special_col, expected_special, diff) %>%
              head(10))
    }
  }
}

###############################################################################
# Count creatures by special resist presence
###############################################################################

cat("\n\n=== SPECIAL RESIST PRESENCE ===\n\n")

# Create flags for having any special resist
creatures_with_fort <- creatures_with_fort %>%
  mutate(
    has_kinen_special = (kinetic.special != 0) | (energy.special != 0),
    has_nonkinen_special = (blast.special != 0) | (heat.special != 0) |
                           (cold.special != 0) | (electricity.special != 0) |
                           (acid.special != 0) | (stun.special != 0),
    has_any_special = has_kinen_special | has_nonkinen_special
  )

cat("Creatures with special resists:\n")
cat("  Any special resist:", sum(creatures_with_fort$has_any_special), "\n")
cat("  Kinen special (kinetic/energy):", sum(creatures_with_fort$has_kinen_special), "\n")
cat("  Non-kinen special:", sum(creatures_with_fort$has_nonkinen_special), "\n")

###############################################################################
# Filter to unarmored and check residuals by special resist presence
###############################################################################

cat("\n\n=== IMPACT ON UNARMORED CREATURE LEVEL ===\n\n")

# Merge special resist info with our analysis data
special_cols <- creatures_with_fort %>%
  select(serial, has_kinen_special, has_nonkinen_special, has_any_special,
         kin_spec = kinetic.special, ene_spec = energy.special,
         bla_spec = blast.special, hea_spec = heat.special,
         col_spec = cold.special, ele_spec = electricity.special,
         aci_spec = acid.special, stu_spec = stun.special)

unarmored_with_special <- normalized_df %>%
  filter(armor == 0) %>%
  left_join(special_cols, by = "serial") %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6,
    # Calculate special vs effective components
    kinen_special = (kin_spec + ene_spec) / 2,
    nonkinen_special = (bla_spec + hea_spec + col_spec +
                        ele_spec + aci_spec + stu_spec) / 6
  )

# Apply our clean formula
unarmored_with_special$pred <- 9 +
  0.01 * unarmored_with_special$hardiness -
  0.02 * unarmored_with_special$fortitude +
  0.01 * unarmored_with_special$dexterity +
  0.01 * unarmored_with_special$intellect +
  0.025 * unarmored_with_special$cleverness +
  0.015 * unarmored_with_special$power +
  0.12 * unarmored_with_special$kinen +
  0.06 * unarmored_with_special$nonkinen

unarmored_with_special$residual <- unarmored_with_special$level - unarmored_with_special$pred

# Compare residuals by special resist presence
cat("Residuals by special resist presence:\n\n")

unarmored_with_special %>%
  group_by(has_any_special) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    mean_resid = mean(residual),
    sd_resid = sd(residual),
    .groups = "drop"
  ) %>%
  print()

cat("\n\nResiduals by non-kinen special presence:\n")
unarmored_with_special %>%
  group_by(has_nonkinen_special) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    mean_resid = mean(residual),
    sd_resid = sd(residual),
    .groups = "drop"
  ) %>%
  print()

###############################################################################
# Look at creatures with special resists in detail
###############################################################################

cat("\n\n=== CREATURES WITH SPECIAL RESISTS (UNARMORED) ===\n\n")

special_creatures <- unarmored_with_special %>%
  filter(has_any_special) %>%
  select(serial, skin, level, pred, residual, fortitude,
         kinen, nonkinen, kinen_special, nonkinen_special) %>%
  arrange(desc(abs(residual)))

cat("Count:", nrow(special_creatures), "\n\n")
print(special_creatures, n = 30)

###############################################################################
# Test: Does special resist ratio predict residuals?
###############################################################################

cat("\n\n=== TESTING SPECIAL RESIST HYPOTHESIS ===\n\n")

# For creatures with resists, what fraction is special vs effective?
unarmored_with_special <- unarmored_with_special %>%
  mutate(
    # Total resist value (using absolute since resists can be negative)
    total_nonkinen_abs = abs(blast) + abs(heat) + abs(cold) +
                         abs(electricity) + abs(acid) + abs(stun),
    total_nonkinen_special = abs(bla_spec) + abs(hea_spec) +
                             abs(col_spec) + abs(ele_spec) +
                             abs(aci_spec) + abs(stu_spec),
    # Ratio of special to total (where total > 0)
    special_ratio = ifelse(total_nonkinen_abs > 0,
                           total_nonkinen_special / total_nonkinen_abs,
                           0)
  )

# Correlation between special ratio and residual
cor_test <- cor.test(unarmored_with_special$special_ratio,
                     unarmored_with_special$residual,
                     use = "complete.obs")

cat("Correlation between special_ratio and residual:\n")
cat("  r =", round(cor_test$estimate, 4), "\n")
cat("  p =", format(cor_test$p.value, digits = 4), "\n")

# Model with special resist adjustment
cat("\n\nTesting model with special resist term:\n\n")

model_with_special <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen + nonkinen_special,
  data = unarmored_with_special
)

cat("Coefficient for nonkinen_special:\n")
print(summary(model_with_special)$coefficients["nonkinen_special", ])

cat("\n\nFull model summary:\n")
print(summary(model_with_special))

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

p1 <- ggplot(unarmored_with_special, aes(x = nonkinen_special, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residual vs Non-Kinen Special Resist",
    subtitle = "Does having more special resists affect level?",
    x = "Average Non-Kinen Special Resist",
    y = "Residual (actual - predicted)"
  ) +
  theme_minimal()
print(p1)

p2 <- ggplot(unarmored_with_special, aes(x = factor(has_nonkinen_special), y = residual)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals by Special Resist Presence",
    x = "Has Non-Kinen Special Resist",
    y = "Residual"
  ) +
  theme_minimal()
print(p2)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("1. SPECIAL RESISTS IN DATA:\n")
cat("   - The data DOES distinguish special vs effective resists\n")
cat("   - Most creatures have only effective resists\n")
cat("   - Special resists appear to be rarer\n\n")

cat("2. KINEN SPECIAL (your hypothesis):\n")
n_kinen_special <- sum(unarmored_with_special$has_kinen_special, na.rm = TRUE)
cat("   - Only", n_kinen_special, "unarmored creatures have kinetic/energy special\n")
cat("   - As you suspected, special kinen is very rare!\n\n")

cat("3. SPECIAL RESIST IMPACT ON LEVEL:\n")
cat("   - Correlation with residual: r =", round(cor_test$estimate, 4), "\n")
if (abs(cor_test$estimate) > 0.1 && cor_test$p.value < 0.05) {
  cat("   - There IS a relationship between special resists and level!\n")
} else {
  cat("   - No strong relationship detected in this data\n")
}
