source("R/data.R")

library(dplyr)
library(ggplot2)

cat("=============================================================\n")
cat("   SKIN-LEVEL INTERACTION EFFECTS\n")
cat("=============================================================\n\n")

###############################################################################
# Setup
###############################################################################

# Get both armored and unarmored
armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
no_armor_df <- normalized_df %>% filter(armor == 0)

# Calculate linear predictions
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

armor_df$pred_linear <- -23 +
  0.01 * armor_df$hardiness +
  0.06 * armor_df$fortitude +
  0.005 * armor_df$dexterity +
  0.01 * armor_df$intellect +
  0.025 * armor_df$cleverness +
  0.015 * armor_df$power +
  0.1 * armor_df$kinen +
  0.08 * armor_df$nonkinen

armor_df$resid_linear <- armor_df$level - armor_df$pred_linear

###############################################################################
# Skin-specific residuals - Unarmored
###############################################################################

cat("=== UNARMORED SKIN-SPECIFIC EFFECTS ===\n\n")

skin_effects_unarmored <- no_armor_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_level = mean(level),
    mean_pred = mean(pred_linear),
    mean_resid = mean(resid_linear),
    sd_resid = sd(resid_linear),
    se_resid = sd(resid_linear) / sqrt(n()),
    min_level = min(level),
    max_level = max(level),
    .groups = "drop"
  ) %>%
  mutate(
    # 95% CI for skin effect
    ci_lower = mean_resid - 1.96 * se_resid,
    ci_upper = mean_resid + 1.96 * se_resid,
    # Is the skin effect significantly different from 0?
    significant = !(ci_lower <= 0 & ci_upper >= 0) | is.na(se_resid)
  ) %>%
  arrange(mean_resid)

cat("Unarmored skins with significant residual bias (95% CI excludes 0):\n\n")
sig_skins <- skin_effects_unarmored %>%
  filter(significant & n >= 3)
print(as.data.frame(sig_skins))

###############################################################################
# Add skin effects to model
###############################################################################

cat("\n\n=== MODEL WITH SKIN FIXED EFFECTS ===\n\n")

# Filter to skins with n >= 3 for reliable estimates
common_skins <- skin_effects_unarmored %>%
  filter(n >= 3) %>%
  pull(skin)

no_armor_common <- no_armor_df %>%
  filter(skin %in% common_skins)

cat("Using skins with n >= 3:\n")
cat("  Skins:", length(common_skins), "\n")
cat("  Creatures:", nrow(no_armor_common), "\n\n")

# Model with skin fixed effects
model_skin <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                   cleverness + power + kinen + nonkinen + skin,
                 data = no_armor_common)

cat("Model with skin fixed effects:\n")
cat("  R²:", round(summary(model_skin)$r.squared, 4), "\n")

no_armor_common$pred_skin <- predict(model_skin)
no_armor_common$resid_skin <- no_armor_common$level - no_armor_common$pred_skin

sw_skin <- shapiro.test(no_armor_common$resid_skin)
cat("  SD:", round(sd(no_armor_common$resid_skin), 3), "\n")
cat("  Shapiro p:", format(sw_skin$p.value, digits = 4),
    ifelse(sw_skin$p.value > 0.05, " <-- NORMAL!", ""), "\n")

# Compare to baseline
sw_baseline <- shapiro.test(no_armor_common$resid_linear)
cat("\nBaseline (no skin effects):\n")
cat("  SD:", round(sd(no_armor_common$resid_linear), 3), "\n")
cat("  Shapiro p:", format(sw_baseline$p.value, digits = 4), "\n")

###############################################################################
# Extract skin coefficients
###############################################################################

cat("\n\n=== SKIN LEVEL ADJUSTMENTS ===\n\n")

skin_coefs <- coef(model_skin)
skin_coefs <- skin_coefs[grep("^skin", names(skin_coefs))]
skin_coefs <- data.frame(
  skin = gsub("^skin", "", names(skin_coefs)),
  adjustment = as.numeric(skin_coefs)
) %>%
  arrange(adjustment)

cat("Skin-specific level adjustments (relative to baseline):\n\n")
print(as.data.frame(skin_coefs))

# What's the baseline skin?
baseline_skin <- setdiff(common_skins, skin_coefs$skin)
cat("\nBaseline skin (adjustment = 0):", baseline_skin, "\n")

###############################################################################
# Simpler approach: Just use skin mean residual as adjustment
###############################################################################

cat("\n\n=== SIMPLE SKIN ADJUSTMENT ===\n\n")

# Calculate skin adjustment from mean residuals
skin_adj <- skin_effects_unarmored %>%
  select(skin, skin_adjustment = mean_resid)

no_armor_df <- no_armor_df %>%
  left_join(skin_adj, by = "skin")

# Apply adjustment
no_armor_df$pred_adj <- no_armor_df$pred_linear + no_armor_df$skin_adjustment
no_armor_df$resid_adj <- no_armor_df$level - no_armor_df$pred_adj

cat("With simple skin adjustment:\n")
cat("  Mean resid:", round(mean(no_armor_df$resid_adj, na.rm = TRUE), 4), "\n")
cat("  SD resid:", round(sd(no_armor_df$resid_adj, na.rm = TRUE), 3), "\n")
sw_adj <- shapiro.test(no_armor_df$resid_adj)
cat("  Shapiro p:", format(sw_adj$p.value, digits = 4),
    ifelse(sw_adj$p.value > 0.05, " <-- NORMAL!", ""), "\n")

###############################################################################
# Check if it's the aggressive skins
###############################################################################

cat("\n\n=== AGGRESSIVE VS NON-AGGRESSIVE SKINS ===\n\n")

# From historical guide
aggressive_skins <- c("angler", "boar_wolf", "bocatt", "choku", "huurton",
                      "kusak", "langlatch", "shear_mite", "bordok",
                      "dune_lizard", "narglatch", "woolamander", "kliknik",
                      "vesp", "graul", "rancor", "kimogila")

no_armor_df$is_aggressive <- no_armor_df$skin %in% aggressive_skins

cat("Mean residual by aggression type:\n")
agg_resids <- no_armor_df %>%
  group_by(is_aggressive) %>%
  summarise(
    n = n(),
    mean_resid = mean(resid_linear),
    sd_resid = sd(resid_linear),
    .groups = "drop"
  )
print(as.data.frame(agg_resids))

# T-test
agg_t <- t.test(resid_linear ~ is_aggressive, data = no_armor_df)
cat("\nT-test p-value:", format(agg_t$p.value, digits = 4), "\n")

###############################################################################
# Check for level-dependent effects
###############################################################################

cat("\n\n=== LEVEL-DEPENDENT EFFECTS ===\n\n")

# Maybe residuals depend on level itself?
level_cor <- cor.test(no_armor_df$level, no_armor_df$resid_linear)
cat("Correlation of level with residual:\n")
cat("  r =", round(level_cor$estimate, 3), "\n")
cat("  p =", format(level_cor$p.value, digits = 4), "\n")

# Look at residuals by level brackets
no_armor_df$level_bracket <- cut(no_armor_df$level,
                                  breaks = c(0, 10, 15, 20, 25, 30, 100),
                                  labels = c("5-10", "11-15", "16-20", "21-25", "26-30", "30+"))

cat("\nResiduals by level bracket:\n")
level_resids <- no_armor_df %>%
  group_by(level_bracket) %>%
  summarise(
    n = n(),
    mean_resid = mean(resid_linear),
    sd_resid = sd(resid_linear),
    .groups = "drop"
  )
print(as.data.frame(level_resids))

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# Skin effects
p1 <- ggplot(skin_effects_unarmored %>% filter(n >= 3),
             aes(x = reorder(skin, mean_resid), y = mean_resid)) +
  geom_col(aes(fill = significant)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "gray70", "TRUE" = "steelblue")) +
  labs(title = "Skin-Specific Level Effects (Unarmored, n >= 3)",
       x = "Skin", y = "Mean Residual (actual - predicted)",
       fill = "Significant?") +
  theme_minimal()
print(p1)

# Residuals by level
p2 <- ggplot(no_armor_df, aes(x = level, y = resid_linear)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual vs Level",
       x = "Actual Level", y = "Residual") +
  theme_minimal()
print(p2)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("FINDINGS:\n\n")

cat("1. SKIN EFFECTS:\n")
cat("   - Adding skin fixed effects dramatically improves model\n")
cat("   - Shapiro p goes from", format(sw_baseline$p.value, digits = 3),
    "to", format(sw_skin$p.value, digits = 3), "\n")
cat("   - Different skins have inherent level bonuses/penalties\n\n")

cat("2. SIGNIFICANT SKIN ADJUSTMENTS:\n")
significant_effects <- sig_skins %>%
  select(skin, n, mean_resid) %>%
  mutate(mean_resid = round(mean_resid, 2))
cat("   (Skins where 95% CI excludes 0, n >= 3)\n")
for (i in 1:nrow(significant_effects)) {
  direction <- ifelse(significant_effects$mean_resid[i] > 0, "+", "")
  cat(sprintf("   - %s: %s%.1f levels (n=%d)\n",
              significant_effects$skin[i],
              direction,
              significant_effects$mean_resid[i],
              significant_effects$n[i]))
}

cat("\n3. AGGRESSIVE SKINS:\n")
cat("   - Aggressive skins have mean residual:",
    round(filter(agg_resids, is_aggressive == TRUE)$mean_resid, 2), "\n")
cat("   - Non-aggressive skins have mean residual:",
    round(filter(agg_resids, is_aggressive == FALSE)$mean_resid, 2), "\n")
cat("   - Difference is", ifelse(agg_t$p.value < 0.05, "SIGNIFICANT", "not significant"),
    "(p =", format(agg_t$p.value, digits = 3), ")\n")
