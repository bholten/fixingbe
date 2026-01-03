source("R/data.R")

library(dplyr)
library(ggplot2)

cat("=============================================================\n")
cat("   RESIDUAL ANALYSIS WITH FILTERED DATA\n")
cat("=============================================================\n\n")

###############################################################################
# Use normalized_df which has all filters applied
###############################################################################

cat("Using normalized_df from data.R (filters applied)\n")
cat("Total creatures:", nrow(normalized_df), "\n\n")

# Split by armor status
armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)
no_armor_df <- normalized_df %>% filter(armor == 0)

cat("Armored creatures:", nrow(armor_df), "\n")
cat("Unarmored creatures:", nrow(no_armor_df), "\n\n")

###############################################################################
# Apply clean formulas
###############################################################################

# Armored
armor_df$pred <- -23 +
  0.01 * armor_df$hardiness +
  0.06 * armor_df$fortitude +
  0.005 * armor_df$dexterity +
  0.01 * armor_df$intellect +
  0.025 * armor_df$cleverness +
  0.015 * armor_df$power +
  0.1 * armor_df$kinen +
  0.08 * armor_df$nonkinen

armor_df$resid <- armor_df$level - armor_df$pred

# Unarmored
no_armor_df$pred <- 9 +
  0.01 * no_armor_df$hardiness -
  0.02 * no_armor_df$fortitude +
  0.01 * no_armor_df$dexterity +
  0.01 * no_armor_df$intellect +
  0.025 * no_armor_df$cleverness +
  0.015 * no_armor_df$power +
  0.12 * no_armor_df$kinen +
  0.06 * no_armor_df$nonkinen

no_armor_df$resid <- no_armor_df$level - no_armor_df$pred

###############################################################################
# Residual statistics
###############################################################################

cat("=== RESIDUAL STATISTICS ===\n\n")

cat("ARMORED:\n")
cat("  Mean:   ", round(mean(armor_df$resid), 3), "\n")
cat("  SD:     ", round(sd(armor_df$resid), 3), "\n")
cat("  Min:    ", round(min(armor_df$resid), 3), "\n")
cat("  Max:    ", round(max(armor_df$resid), 3), "\n")

r2_armor <- 1 - sum(armor_df$resid^2) / sum((armor_df$level - mean(armor_df$level))^2)
cat("  R²:     ", round(r2_armor, 4), "\n")

sw_armor <- shapiro.test(armor_df$resid)
cat("  Shapiro p:", format(sw_armor$p.value, digits = 4), "\n")
cat("  Normal? ", ifelse(sw_armor$p.value > 0.05, "YES!", "No"), "\n")

cat("\nUNARMORED:\n")
cat("  Mean:   ", round(mean(no_armor_df$resid), 3), "\n")
cat("  SD:     ", round(sd(no_armor_df$resid), 3), "\n")
cat("  Min:    ", round(min(no_armor_df$resid), 3), "\n")
cat("  Max:    ", round(max(no_armor_df$resid), 3), "\n")

r2_unarmor <- 1 - sum(no_armor_df$resid^2) / sum((no_armor_df$level - mean(no_armor_df$level))^2)
cat("  R²:     ", round(r2_unarmor, 4), "\n")

sw_unarmor <- shapiro.test(no_armor_df$resid)
cat("  Shapiro p:", format(sw_unarmor$p.value, digits = 4), "\n")
cat("  Normal? ", ifelse(sw_unarmor$p.value > 0.05, "YES!", "No"), "\n")

###############################################################################
# Test with level floor for unarmored
###############################################################################

cat("\n\n=== TESTING LEVEL FLOOR FOR UNARMORED ===\n\n")

# Test different floors
for (floor_val in c(7, 8, 9, 10)) {
  pred_floored <- pmax(no_armor_df$pred, floor_val)
  resid_floored <- no_armor_df$level - pred_floored

  r2 <- 1 - sum(resid_floored^2) / sum((no_armor_df$level - mean(no_armor_df$level))^2)
  sw <- shapiro.test(resid_floored)

  cat(sprintf("Floor=%d: R²=%.4f, SD=%.2f, Shapiro p=%.6f %s\n",
              floor_val, r2, sd(resid_floored), sw$p.value,
              ifelse(sw$p.value > 0.05, "<-- NORMAL!", "")))
}

###############################################################################
# Check remaining outliers
###############################################################################

cat("\n\n=== REMAINING OUTLIERS (|resid| > 5) ===\n\n")

cat("Unarmored outliers:\n")
outliers <- no_armor_df %>%
  filter(abs(resid) > 5) %>%
  select(serial, skin, level, pred, resid, fortitude, kinen, nonkinen) %>%
  arrange(desc(abs(resid)))

print(as.data.frame(outliers))

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATION ===\n")

# Combined plot
armor_df$type <- "Armored"
no_armor_df$type <- "Unarmored"

combined <- bind_rows(
  armor_df %>% select(level, pred, resid, type),
  no_armor_df %>% select(level, pred, resid, type)
)

p1 <- ggplot(combined, aes(x = pred, y = level, color = type)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Armored" = "blue", "Unarmored" = "red")) +
  labs(title = "Predicted vs Actual Level (Filtered Data)",
       x = "Predicted Level", y = "Actual Level") +
  theme_minimal()
print(p1)

# Residual histograms
p2 <- ggplot(armor_df, aes(x = resid)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(armor_df$resid), sd = sd(armor_df$resid)),
                color = "darkblue", linewidth = 1) +
  labs(title = sprintf("Armored Residuals (Shapiro p=%.4f)", sw_armor$p.value),
       x = "Residual", y = "Density") +
  theme_minimal()
print(p2)

p3 <- ggplot(no_armor_df, aes(x = resid)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "red", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(no_armor_df$resid), sd = sd(no_armor_df$resid)),
                color = "darkred", linewidth = 1) +
  labs(title = sprintf("Unarmored Residuals (Shapiro p=%.4f)", sw_unarmor$p.value),
       x = "Residual", y = "Density") +
  theme_minimal()
print(p3)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("ARMORED (n=", nrow(armor_df), "):\n", sep = "")
cat("  R² = ", round(r2_armor, 4), "\n")
cat("  SD = ", round(sd(armor_df$resid), 2), " levels\n")
cat("  Shapiro p = ", format(sw_armor$p.value, digits = 4), "\n")
if (sw_armor$p.value > 0.05) {
  cat("  *** RESIDUALS ARE NORMALLY DISTRIBUTED ***\n")
  cat("  The remaining error is likely due to crafting system randomness.\n")
}

cat("\nUNARMORED (n=", nrow(no_armor_df), "):\n", sep = "")
cat("  R² = ", round(r2_unarmor, 4), "\n")
cat("  SD = ", round(sd(no_armor_df$resid), 2), " levels\n")
cat("  Shapiro p = ", format(sw_unarmor$p.value, digits = 4), "\n")
if (sw_unarmor$p.value > 0.05) {
  cat("  *** RESIDUALS ARE NORMALLY DISTRIBUTED ***\n")
} else {
  cat("  Residuals are still not normal. Additional factors may be at play.\n")
}
