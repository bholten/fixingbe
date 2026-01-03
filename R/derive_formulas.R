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
cat("       CREATURE LEVEL FORMULA DERIVATION\n")
cat("=============================================================\n\n")

###############################################################################
# QUESTION: Raw attributes vs derived stats?
###############################################################################

cat("=== RAW ATTRIBUTES VS DERIVED STATS ===\n\n")

# Compare R² for different approaches on armored data (cleaner)
model.armor.raw <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                               cleverness + power + courage + kinen + nonkinen,
                      data = armor_df)

model.armor.derived <- lm(level ~ health + action + mind + damage_low + damage_high +
                                   speed + to_hit + fortitude + kinen + nonkinen,
                          data = armor_df)

cat("Armored creatures:\n")
cat("  Raw attributes R²:    ", round(summary(model.armor.raw)$r.squared, 4), "\n")
cat("  Derived stats R²:     ", round(summary(model.armor.derived)$r.squared, 4), "\n")

model.noarmor.raw <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                                 cleverness + power + courage + kinen + nonkinen,
                        data = no_armor_df)

model.noarmor.derived <- lm(level ~ health + action + mind + damage_low + damage_high +
                                     speed + to_hit + kinen + nonkinen,
                            data = no_armor_df)

cat("\nUnarmored creatures:\n")
cat("  Raw attributes R²:    ", round(summary(model.noarmor.raw)$r.squared, 4), "\n")
cat("  Derived stats R²:     ", round(summary(model.noarmor.derived)$r.squared, 4), "\n")

cat("\n>> RECOMMENDATION: Use RAW ATTRIBUTES (higher R² for both)\n\n")

###############################################################################
# ARMORED FORMULA
###############################################################################

cat("=============================================================\n")
cat("                 ARMORED FORMULA (fortitude >= 500)\n")
cat("=============================================================\n\n")

# Linear model (game-dev friendly)
model.armor.linear <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + courage + kinen + nonkinen,
  data = armor_df
)

cat("--- Linear Model Coefficients ---\n\n")
coefs.armor <- coef(model.armor.linear)
print(round(coefs.armor, 6))

cat("\n--- Suggested Rounded Coefficients ---\n\n")

# Round to game-dev friendly values
armor_formula <- list(
  intercept = round(coefs.armor["(Intercept)"], 0),
  hardiness = round(coefs.armor["hardiness"] * 1000) / 1000,
  fortitude = round(coefs.armor["fortitude"] * 1000) / 1000,
  dexterity = round(coefs.armor["dexterity"] * 1000) / 1000,
  intellect = round(coefs.armor["intellect"] * 1000) / 1000,
  cleverness = round(coefs.armor["cleverness"] * 1000) / 1000,
  power = round(coefs.armor["power"] * 1000) / 1000,
  courage = round(coefs.armor["courage"] * 1000) / 1000,
  kinen = round(coefs.armor["kinen"] * 100) / 100,
  nonkinen = round(coefs.armor["nonkinen"] * 100) / 100
)

cat("Proposed armored formula:\n\n")
cat("level = ", armor_formula$intercept, "\n")
cat("      + ", armor_formula$hardiness, " * hardiness\n")
cat("      + ", armor_formula$fortitude, " * fortitude\n")
cat("      + ", armor_formula$dexterity, " * dexterity\n")
cat("      + ", armor_formula$intellect, " * intellect\n")
cat("      + ", armor_formula$cleverness, " * cleverness\n")
cat("      + ", armor_formula$power, " * power\n")
cat("      + ", armor_formula$courage, " * courage\n")
cat("      + ", armor_formula$kinen, " * (kinetic + energy) / 2\n")
cat("      + ", armor_formula$nonkinen, " * (blast + heat + cold + electricity + acid + stun) / 6\n")

# Test the rounded formula
armor_df$pred_formula <- armor_formula$intercept +
  armor_formula$hardiness * armor_df$hardiness +
  armor_formula$fortitude * armor_df$fortitude +
  armor_formula$dexterity * armor_df$dexterity +
  armor_formula$intellect * armor_df$intellect +
  armor_formula$cleverness * armor_df$cleverness +
  armor_formula$power * armor_df$power +
  armor_formula$courage * armor_df$courage +
  armor_formula$kinen * armor_df$kinen +
  armor_formula$nonkinen * armor_df$nonkinen

armor_df$residual_formula <- armor_df$level - armor_df$pred_formula

cat("\n--- Formula Validation ---\n\n")
cat("Mean residual:     ", round(mean(armor_df$residual_formula), 3), "\n")
cat("SD residual:       ", round(sd(armor_df$residual_formula), 3), "\n")
cat("Max abs residual:  ", round(max(abs(armor_df$residual_formula)), 3), "\n")

# R² of rounded formula
ss_res <- sum(armor_df$residual_formula^2)
ss_tot <- sum((armor_df$level - mean(armor_df$level))^2)
r2_formula <- 1 - ss_res / ss_tot
cat("R² (rounded):      ", round(r2_formula, 4), "\n")

###############################################################################
# UNARMORED FORMULA
###############################################################################

cat("\n\n=============================================================\n")
cat("              UNARMORED FORMULA (fortitude < 500)\n")
cat("=============================================================\n\n")

model.noarmor.linear <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + courage + kinen + nonkinen,
  data = no_armor_df
)

cat("--- Linear Model Coefficients ---\n\n")
coefs.noarmor <- coef(model.noarmor.linear)
print(round(coefs.noarmor, 6))

cat("\n--- Suggested Rounded Coefficients ---\n\n")

noarmor_formula <- list(
  intercept = round(coefs.noarmor["(Intercept)"], 0),
  hardiness = round(coefs.noarmor["hardiness"] * 1000) / 1000,
  fortitude = round(coefs.noarmor["fortitude"] * 1000) / 1000,
  dexterity = round(coefs.noarmor["dexterity"] * 1000) / 1000,
  intellect = round(coefs.noarmor["intellect"] * 1000) / 1000,
  cleverness = round(coefs.noarmor["cleverness"] * 1000) / 1000,
  power = round(coefs.noarmor["power"] * 1000) / 1000,
  courage = round(coefs.noarmor["courage"] * 1000) / 1000,
  kinen = round(coefs.noarmor["kinen"] * 100) / 100,
  nonkinen = round(coefs.noarmor["nonkinen"] * 100) / 100
)

cat("Proposed unarmored formula:\n\n")
cat("level = ", noarmor_formula$intercept, "\n")
cat("      + ", noarmor_formula$hardiness, " * hardiness\n")
cat("      + ", noarmor_formula$fortitude, " * fortitude  // NOTE: NEGATIVE!\n")
cat("      + ", noarmor_formula$dexterity, " * dexterity\n")
cat("      + ", noarmor_formula$intellect, " * intellect\n")
cat("      + ", noarmor_formula$cleverness, " * cleverness\n")
cat("      + ", noarmor_formula$power, " * power\n")
cat("      + ", noarmor_formula$courage, " * courage\n")
cat("      + ", noarmor_formula$kinen, " * (kinetic + energy) / 2\n")
cat("      + ", noarmor_formula$nonkinen, " * (blast + heat + cold + electricity + acid + stun) / 6\n")

# Test the rounded formula
no_armor_df$pred_formula <- noarmor_formula$intercept +
  noarmor_formula$hardiness * no_armor_df$hardiness +
  noarmor_formula$fortitude * no_armor_df$fortitude +
  noarmor_formula$dexterity * no_armor_df$dexterity +
  noarmor_formula$intellect * no_armor_df$intellect +
  noarmor_formula$cleverness * no_armor_df$cleverness +
  noarmor_formula$power * no_armor_df$power +
  noarmor_formula$courage * no_armor_df$courage +
  noarmor_formula$kinen * no_armor_df$kinen +
  noarmor_formula$nonkinen * no_armor_df$nonkinen

no_armor_df$residual_formula <- no_armor_df$level - no_armor_df$pred_formula

cat("\n--- Formula Validation ---\n\n")
cat("Mean residual:     ", round(mean(no_armor_df$residual_formula), 3), "\n")
cat("SD residual:       ", round(sd(no_armor_df$residual_formula), 3), "\n")
cat("Max abs residual:  ", round(max(abs(no_armor_df$residual_formula)), 3), "\n")

ss_res <- sum(no_armor_df$residual_formula^2)
ss_tot <- sum((no_armor_df$level - mean(no_armor_df$level))^2)
r2_formula <- 1 - ss_res / ss_tot
cat("R² (rounded):      ", round(r2_formula, 4), "\n")

###############################################################################
# SIMPLIFIED FORMULA (fewer terms)
###############################################################################

cat("\n\n=============================================================\n")
cat("              SIMPLIFIED FORMULAS (fewer terms)\n")
cat("=============================================================\n\n")

cat("Some coefficients are small or not significant. Here are simplified versions:\n\n")

# Armored simplified: drop courage (not significant)
model.armor.simple <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen,
  data = armor_df
)

cat("--- Armored (simplified, 8 terms) ---\n")
cat("R²:", round(summary(model.armor.simple)$r.squared, 4), "\n")
print(round(coef(model.armor.simple), 4))

# Unarmored simplified: drop courage
model.noarmor.simple <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen,
  data = no_armor_df
)

cat("\n--- Unarmored (simplified, 8 terms) ---\n")
cat("R²:", round(summary(model.noarmor.simple)$r.squared, 4), "\n")
print(round(coef(model.noarmor.simple), 4))

###############################################################################
# PSEUDOCODE FOR IMPLEMENTATION
###############################################################################

cat("\n\n=============================================================\n")
cat("              PSEUDOCODE FOR GAME IMPLEMENTATION\n")
cat("=============================================================\n\n")

cat('
int calculateCreatureLevel(Creature* creature) {
    float kinen = (creature->kinetic + creature->energy) / 2.0f;
    float nonkinen = (creature->blast + creature->heat + creature->cold +
                      creature->electricity + creature->acid + creature->stun) / 6.0f;

    float level;

    if (creature->fortitude >= 500) {
        // ARMORED FORMULA
        level = ', armor_formula$intercept, '
              + ', armor_formula$hardiness, ' * creature->hardiness
              + ', armor_formula$fortitude, ' * creature->fortitude
              + ', armor_formula$dexterity, ' * creature->dexterity
              + ', armor_formula$intellect, ' * creature->intellect
              + ', armor_formula$cleverness, ' * creature->cleverness
              + ', armor_formula$power, ' * creature->power
              + ', armor_formula$courage, ' * creature->courage
              + ', armor_formula$kinen, ' * kinen
              + ', armor_formula$nonkinen, ' * nonkinen;
    } else {
        // UNARMORED FORMULA
        level = ', noarmor_formula$intercept, '
              + ', noarmor_formula$hardiness, ' * creature->hardiness
              + ', noarmor_formula$fortitude, ' * creature->fortitude  // NEGATIVE!
              + ', noarmor_formula$dexterity, ' * creature->dexterity
              + ', noarmor_formula$intellect, ' * creature->intellect
              + ', noarmor_formula$cleverness, ' * creature->cleverness
              + ', noarmor_formula$power, ' * creature->power
              + ', noarmor_formula$courage, ' * creature->courage
              + ', noarmor_formula$kinen, ' * kinen
              + ', noarmor_formula$nonkinen, ' * nonkinen;
    }

    // Clamp to valid range and round
    level = max(1.0f, min(level, 75.0f));
    return (int)round(level);
}
', sep = "")

###############################################################################
# VISUALIZATION
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

armor_df$type <- "Armored"
no_armor_df$type <- "Unarmored"

combined <- bind_rows(
  armor_df %>% select(level, pred_formula, residual_formula, type),
  no_armor_df %>% select(level, pred_formula, residual_formula, type)
)

p1 <- ggplot(combined, aes(x = pred_formula, y = level, color = type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Armored" = "blue", "Unarmored" = "red")) +
  labs(
    title = "Final Formulas: Predicted vs Actual Level",
    x = "Predicted Level (from formula)",
    y = "Actual Level"
  ) +
  theme_minimal()
print(p1)

p2 <- ggplot(combined, aes(x = residual_formula, fill = type)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("Armored" = "blue", "Unarmored" = "red")) +
  labs(
    title = "Residual Distribution",
    x = "Residual (Actual - Predicted)",
    y = "Count"
  ) +
  theme_minimal()
print(p2)
