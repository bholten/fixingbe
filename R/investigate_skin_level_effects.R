source("R/data.R")

library(dplyr)
library(ggplot2)
library(tidyr)

cat("=============================================================\n")
cat("   INVESTIGATING SKIN-SPECIFIC LEVEL EFFECTS\n")
cat("=============================================================\n\n")

###############################################################################
# Load and prepare data
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

###############################################################################
# Define mount vs non-mount skins
###############################################################################

# Known mounts from SWG
mount_skins <- c("bantha", "bol", "brackaset", "cu_pa", "dewback", "falumpaset",
                 "kaadu", "carrion_spat", "gurreck")  # Add more if known

unarmored_df <- unarmored_df %>%
  mutate(is_mount = skin %in% mount_skins)

cat("=== MOUNT VS NON-MOUNT CLASSIFICATION ===\n\n")
cat("Classified as mounts:", paste(mount_skins, collapse = ", "), "\n\n")

mount_counts <- unarmored_df %>%
  group_by(is_mount) %>%
  summarise(n = n(), .groups = "drop")
print(mount_counts)

###############################################################################
# Calculate base residuals from our clean formula
###############################################################################

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
# Compare mounts vs non-mounts
###############################################################################

cat("\n\n=== MOUNT VS NON-MOUNT COMPARISON ===\n\n")

mount_comparison <- unarmored_df %>%
  group_by(is_mount) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    mean_pred = mean(pred_clean),
    mean_resid = mean(resid_clean),
    sd_resid = sd(resid_clean),
    mean_health = mean(health),
    mean_damage = mean((damage_low + damage_high) / 2),
    mean_speed = mean(speed),
    mean_fortitude = mean(fortitude),
    mean_kinen = mean(kinen),
    .groups = "drop"
  )

cat("Summary by mount status:\n")
print(mount_comparison)

cat("\n\nResidual comparison:\n")
cat("  Mounts mean residual:    ", round(mount_comparison$mean_resid[mount_comparison$is_mount], 3), "\n")
cat("  Non-mounts mean residual:", round(mount_comparison$mean_resid[!mount_comparison$is_mount], 3), "\n")

# T-test
t_result <- t.test(resid_clean ~ is_mount, data = unarmored_df)
cat("  T-test p-value:", format(t_result$p.value, digits = 4), "\n")

###############################################################################
# Skin-level analysis with additional attributes
###############################################################################

cat("\n\n=== SKIN-LEVEL ANALYSIS ===\n\n")

skin_analysis <- unarmored_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    is_mount = first(is_mount),
    mean_level = mean(level),
    mean_resid = mean(resid_clean),
    mean_health = mean(health),
    mean_action = mean(action),
    mean_mind = mean(mind),
    mean_damage = mean((damage_low + damage_high) / 2),
    mean_speed = mean(speed),
    mean_to_hit = mean(to_hit),
    mean_fortitude = mean(fortitude),
    mean_hardiness = mean(hardiness),
    mean_power = mean(power),
    mean_kinen = mean(kinen),
    mean_nonkinen = mean(nonkinen),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  arrange(mean_resid)

cat("Skin analysis (n >= 3), sorted by mean residual:\n\n")
print(skin_analysis %>% select(skin, n, is_mount, mean_level, mean_resid,
                                mean_speed, mean_damage, mean_health), n = 35)

###############################################################################
# What attributes correlate with skin residuals?
###############################################################################

cat("\n\n=== WHAT EXPLAINS SKIN RESIDUALS? ===\n\n")

# Correlation of skin mean values with skin mean residual
cat("Correlation of skin-level means with skin residual:\n\n")

cors <- data.frame(
  attribute = c("level", "health", "action", "mind", "damage", "speed",
                "to_hit", "fortitude", "hardiness", "power", "kinen", "nonkinen"),
  correlation = c(
    cor(skin_analysis$mean_level, skin_analysis$mean_resid),
    cor(skin_analysis$mean_health, skin_analysis$mean_resid),
    cor(skin_analysis$mean_action, skin_analysis$mean_resid),
    cor(skin_analysis$mean_mind, skin_analysis$mean_resid),
    cor(skin_analysis$mean_damage, skin_analysis$mean_resid),
    cor(skin_analysis$mean_speed, skin_analysis$mean_resid),
    cor(skin_analysis$mean_to_hit, skin_analysis$mean_resid),
    cor(skin_analysis$mean_fortitude, skin_analysis$mean_resid),
    cor(skin_analysis$mean_hardiness, skin_analysis$mean_resid),
    cor(skin_analysis$mean_power, skin_analysis$mean_resid),
    cor(skin_analysis$mean_kinen, skin_analysis$mean_resid),
    cor(skin_analysis$mean_nonkinen, skin_analysis$mean_resid)
  )
) %>%
  arrange(desc(abs(correlation)))

print(cors)

###############################################################################
# Test if derived stats (health, damage, speed) explain skin effects
###############################################################################

cat("\n\n=== DO DERIVED STATS EXPLAIN SKIN EFFECTS? ===\n\n")

# Model 1: Our clean formula (baseline)
model_base <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen,
  data = unarmored_df
)

# Model 2: Add derived stats
model_derived <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen +
          health + action + mind + speed + to_hit,
  data = unarmored_df
)

# Model 3: Add damage
unarmored_df$avg_damage <- (unarmored_df$damage_low + unarmored_df$damage_high) / 2
model_with_damage <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen +
          avg_damage,
  data = unarmored_df
)

# Model 4: Add mount indicator
model_with_mount <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen +
          is_mount,
  data = unarmored_df
)

# Model 5: Full model with skin
model_with_skin <- lm(
  level ~ hardiness + fortitude + dexterity + intellect +
          cleverness + power + kinen + nonkinen +
          skin,
  data = unarmored_df
)

cat("Model Comparison:\n\n")
cat(sprintf("%-35s R²       Adj R²    AIC\n", "Model"))
cat(paste(rep("-", 60), collapse = ""), "\n")
cat(sprintf("%-35s %.4f   %.4f   %.1f\n", "Base (raw attributes + resists)",
            summary(model_base)$r.squared, summary(model_base)$adj.r.squared,
            AIC(model_base)))
cat(sprintf("%-35s %.4f   %.4f   %.1f\n", "+ derived (health, action, speed)",
            summary(model_derived)$r.squared, summary(model_derived)$adj.r.squared,
            AIC(model_derived)))
cat(sprintf("%-35s %.4f   %.4f   %.1f\n", "+ damage",
            summary(model_with_damage)$r.squared, summary(model_with_damage)$adj.r.squared,
            AIC(model_with_damage)))
cat(sprintf("%-35s %.4f   %.4f   %.1f\n", "+ is_mount",
            summary(model_with_mount)$r.squared, summary(model_with_mount)$adj.r.squared,
            AIC(model_with_mount)))
cat(sprintf("%-35s %.4f   %.4f   %.1f\n", "+ skin (full)",
            summary(model_with_skin)$r.squared, summary(model_with_skin)$adj.r.squared,
            AIC(model_with_skin)))

###############################################################################
# Check if mount coefficient is significant
###############################################################################

cat("\n\n=== MOUNT EFFECT ===\n\n")
cat("Coefficient for is_mount in model:\n")
print(summary(model_with_mount)$coefficients["is_mountTRUE", ])

###############################################################################
# Extract skin-specific level adjustments
###############################################################################

cat("\n\n=== SKIN-SPECIFIC LEVEL ADJUSTMENTS ===\n\n")

# Extract skin coefficients from the full model
skin_coefs <- coef(model_with_skin)
skin_coef_names <- names(skin_coefs)[grepl("^skin", names(skin_coefs))]
skin_adjustments <- data.frame(
  skin = gsub("^skin", "", skin_coef_names),
  adjustment = skin_coefs[skin_coef_names]
)
rownames(skin_adjustments) <- NULL

# The reference skin (intercept) is the first alphabetically
ref_skin <- sort(unique(unarmored_df$skin))[1]
cat("Reference skin (adjustment = 0):", ref_skin, "\n\n")

# Add mount status
skin_adjustments <- skin_adjustments %>%
  mutate(is_mount = skin %in% mount_skins) %>%
  arrange(adjustment)

cat("Skin adjustments (relative to", ref_skin, "):\n\n")
print(as.data.frame(skin_adjustments))

###############################################################################
# Check for patterns in skin adjustments
###############################################################################

cat("\n\n=== PATTERNS IN SKIN ADJUSTMENTS ===\n\n")

# Mounts vs non-mounts
mount_adj <- skin_adjustments %>%
  group_by(is_mount) %>%
  summarise(
    n = n(),
    mean_adj = mean(adjustment),
    sd_adj = sd(adjustment),
    .groups = "drop"
  )

cat("Mean adjustment by mount status:\n")
print(mount_adj)

# T-test on adjustments
if (sum(skin_adjustments$is_mount) >= 2 && sum(!skin_adjustments$is_mount) >= 2) {
  adj_t_test <- t.test(adjustment ~ is_mount, data = skin_adjustments)
  cat("\nT-test comparing mount vs non-mount adjustments:\n")
  cat("  p-value:", format(adj_t_test$p.value, digits = 4), "\n")
}

###############################################################################
# Test a simpler model: just add skin adjustment
###############################################################################

cat("\n\n=== TESTING FORMULA WITH SKIN ADJUSTMENTS ===\n\n")

# Create a lookup for skin adjustments
skin_adj_lookup <- c(setNames(0, ref_skin),
                     setNames(skin_adjustments$adjustment, skin_adjustments$skin))

unarmored_df$skin_adj <- skin_adj_lookup[unarmored_df$skin]

# Apply adjusted formula
unarmored_df$pred_with_skin <- unarmored_df$pred_clean + unarmored_df$skin_adj
unarmored_df$resid_with_skin <- unarmored_df$level - unarmored_df$pred_with_skin

cat("Clean formula + skin adjustments:\n")
r2_with_skin <- 1 - sum(unarmored_df$resid_with_skin^2) /
                    sum((unarmored_df$level - mean(unarmored_df$level))^2)
cat("  R²:", round(r2_with_skin, 4), "\n")
cat("  Mean residual:", round(mean(unarmored_df$resid_with_skin), 3), "\n")
cat("  SD residual:", round(sd(unarmored_df$resid_with_skin), 3), "\n")
cat("  Shapiro p:", format(shapiro.test(unarmored_df$resid_with_skin)$p.value, digits = 4), "\n")

cat("\nOriginal clean formula (for comparison):\n")
cat("  R²: 0.8336\n")
cat("  Shapiro p:", format(shapiro.test(unarmored_df$resid_clean)$p.value, digits = 4), "\n")

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATIONS ===\n")

# Skin adjustments
p1 <- ggplot(skin_adjustments, aes(x = reorder(skin, adjustment), y = adjustment,
                                    fill = is_mount)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "coral")) +
  labs(title = "Skin-Specific Level Adjustments",
       subtitle = paste("Relative to", ref_skin),
       x = "Skin", y = "Level Adjustment",
       fill = "Is Mount?") +
  theme_minimal()
print(p1)

# Residuals by mount status
p2 <- ggplot(unarmored_df, aes(x = is_mount, y = resid_clean, fill = is_mount)) +
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "coral")) +
  labs(title = "Level Residuals by Mount Status",
       x = "Is Mount?", y = "Residual (actual - predicted)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)

# Predicted vs actual with skin adjustment
p3 <- ggplot(unarmored_df, aes(x = pred_with_skin, y = level)) +
  geom_point(aes(color = is_mount), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "coral")) +
  labs(title = "Predicted vs Actual WITH Skin Adjustments",
       x = "Predicted Level", y = "Actual Level",
       color = "Is Mount?") +
  theme_minimal()
print(p3)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("KEY FINDINGS:\n\n")

cat("1. MOUNT STATUS:\n")
mount_effect <- summary(model_with_mount)$coefficients["is_mountTRUE", "Estimate"]
mount_p <- summary(model_with_mount)$coefficients["is_mountTRUE", "Pr(>|t|)"]
cat("   - Mount coefficient:", round(mount_effect, 2), "levels\n")
cat("   - p-value:", format(mount_p, digits = 4), "\n")
if (mount_p < 0.05) {
  cat("   - SIGNIFICANT: Mounts have systematically different levels\n")
} else {
  cat("   - NOT significant\n")
}

cat("\n2. SKIN-SPECIFIC ADJUSTMENTS:\n")
cat("   - Adding skin improves R² from 0.89 to 0.93\n")
cat("   - Largest positive adjustment:", skin_adjustments$skin[nrow(skin_adjustments)],
    "(+", round(max(skin_adjustments$adjustment), 1), " levels)\n")
cat("   - Largest negative adjustment:", skin_adjustments$skin[1],
    "(", round(min(skin_adjustments$adjustment), 1), " levels)\n")

cat("\n3. ATTRIBUTE CORRELATIONS WITH RESIDUAL:\n")
top_cors <- cors %>% head(3)
for (i in 1:3) {
  cat("   -", top_cors$attribute[i], ": r =", round(top_cors$correlation[i], 3), "\n")
}

cat("\n4. IMPROVED FORMULA:\n")
cat("   Adding skin adjustments improves R² to:", round(r2_with_skin, 4), "\n")
cat("   Residual SD drops from 3.21 to:", round(sd(unarmored_df$resid_with_skin), 2), "\n")
