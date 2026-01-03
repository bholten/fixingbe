source("R/data.R")

library(dplyr)
library(ggplot2)

# Feature engineering
normalized_df <- normalized_df %>%
  mutate(
    kinen = (kinetic + energy) / 2,
    nonkinen = (blast + heat + cold + electricity + acid + stun) / 6
  )

no_armor_df <- normalized_df %>% filter(armor == 0)

cat("=============================================================\n")
cat("       TESTING LEVEL FLOOR HYPOTHESIS\n")
cat("=============================================================\n\n")

###############################################################################
# Calculate raw predictions (no floor)
###############################################################################

no_armor_df$pred_raw <- 9 +
  0.01 * no_armor_df$hardiness -
  0.02 * no_armor_df$fortitude +
  0.01 * no_armor_df$dexterity +
  0.01 * no_armor_df$intellect +
  0.025 * no_armor_df$cleverness +
  0.015 * no_armor_df$power +
  0.12 * no_armor_df$kinen +
  0.06 * no_armor_df$nonkinen

###############################################################################
# Test different floor values
###############################################################################

cat("=== TESTING DIFFERENT FLOOR VALUES ===\n\n")

test_floor <- function(df, floor_val) {
  pred_floored <- pmax(df$pred_raw, floor_val)
  residual <- df$level - pred_floored

  # Calculate metrics
  r2 <- 1 - sum(residual^2) / sum((df$level - mean(df$level))^2)
  sw_test <- shapiro.test(residual)

  list(
    floor = floor_val,
    r2 = r2,
    mean_resid = mean(residual),
    sd_resid = sd(residual),
    shapiro_w = sw_test$statistic,
    shapiro_p = sw_test$p.value,
    n_affected = sum(df$pred_raw < floor_val)
  )
}

# Test floors from 1 to 12
floors_to_test <- 1:12
results <- lapply(floors_to_test, function(f) test_floor(no_armor_df, f))
results_df <- do.call(rbind, lapply(results, as.data.frame))

cat("Floor Value Comparison:\n\n")
cat(sprintf("%-6s %8s %10s %8s %10s %12s %10s\n",
            "Floor", "R²", "Mean Res", "SD Res", "Shapiro W", "Shapiro p", "N Affected"))
cat(paste(rep("-", 75), collapse = ""), "\n")

for (i in 1:nrow(results_df)) {
  row <- results_df[i, ]
  normal_flag <- ifelse(row$shapiro_p > 0.05, " *NORMAL*", "")
  cat(sprintf("%-6d %8.4f %10.2f %8.2f %10.4f %12.6f %10d%s\n",
              row$floor, row$r2, row$mean_resid, row$sd_resid,
              row$shapiro_w, row$shapiro_p, row$n_affected, normal_flag))
}

###############################################################################
# Find best floor
###############################################################################

cat("\n\n=== BEST FLOOR ANALYSIS ===\n\n")

# Best by Shapiro-Wilk p-value
best_shapiro <- results_df[which.max(results_df$shapiro_p), ]
cat("Best floor by Shapiro-Wilk p-value:", best_shapiro$floor, "\n")
cat("  p-value:", round(best_shapiro$shapiro_p, 4), "\n")
cat("  R²:", round(best_shapiro$r2, 4), "\n")
cat("  Creatures affected:", best_shapiro$n_affected, "\n")

# Best by R²
best_r2 <- results_df[which.max(results_df$r2), ]
cat("\nBest floor by R²:", best_r2$floor, "\n")
cat("  R²:", round(best_r2$r2, 4), "\n")
cat("  Shapiro p:", round(best_r2$shapiro_p, 4), "\n")

###############################################################################
# Examine creatures affected by floor = 10
###############################################################################

cat("\n\n=== CREATURES AFFECTED BY FLOOR = 10 ===\n\n")

affected <- no_armor_df %>%
  filter(pred_raw < 10) %>%
  arrange(pred_raw) %>%
  select(serial, skin, level, pred_raw, fortitude, kinen, nonkinen, hardiness)

cat("Creatures with pred_raw < 10:", nrow(affected), "\n\n")

if (nrow(affected) > 0) {
  cat("These creatures would be 'floored' to level 10:\n\n")
  print(affected, n = 30)

  cat("\n\nActual level distribution of affected creatures:\n")
  print(table(affected$level))

  cat("\n\nSkin distribution of affected creatures:\n")
  print(table(affected$skin))
}

###############################################################################
# Compare residual distributions: No floor vs Floor=10
###############################################################################

cat("\n\n=== RESIDUAL COMPARISON: NO FLOOR vs FLOOR=10 ===\n\n")

no_armor_df$pred_floor10 <- pmax(no_armor_df$pred_raw, 10)
no_armor_df$resid_raw <- no_armor_df$level - no_armor_df$pred_raw
no_armor_df$resid_floor10 <- no_armor_df$level - no_armor_df$pred_floor10

cat("Without floor:\n")
cat("  Mean residual:", round(mean(no_armor_df$resid_raw), 3), "\n")
cat("  SD residual:  ", round(sd(no_armor_df$resid_raw), 3), "\n")
cat("  Shapiro p:    ", round(shapiro.test(no_armor_df$resid_raw)$p.value, 6), "\n")

cat("\nWith floor = 10:\n")
cat("  Mean residual:", round(mean(no_armor_df$resid_floor10), 3), "\n")
cat("  SD residual:  ", round(sd(no_armor_df$resid_floor10), 3), "\n")
cat("  Shapiro p:    ", round(shapiro.test(no_armor_df$resid_floor10)$p.value, 6), "\n")

###############################################################################
# Check if floor fixes the positive outliers
###############################################################################

cat("\n\n=== POSITIVE OUTLIER CHECK WITH FLOOR=10 ===\n\n")

# Original positive outliers (resid > 3)
orig_pos_outliers <- no_armor_df %>% filter(resid_raw > 3)
cat("Original positive outliers (resid > 3):", nrow(orig_pos_outliers), "\n")

# After floor
new_pos_outliers <- no_armor_df %>% filter(resid_floor10 > 3)
cat("Positive outliers after floor=10:", nrow(new_pos_outliers), "\n")

if (nrow(new_pos_outliers) > 0) {
  cat("\nRemaining positive outliers:\n")
  print(new_pos_outliers %>%
          select(serial, skin, level, pred_raw, pred_floor10, resid_floor10) %>%
          arrange(desc(resid_floor10)), n = 10)
}

###############################################################################
# Visualization
###############################################################################

cat("\n\n=== VISUALIZATIONS ===\n")

# Side by side histograms
p1 <- ggplot(no_armor_df, aes(x = resid_raw)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5,
                 fill = "red", alpha = 0.6) +
  stat_function(fun = dnorm,
                args = list(mean = mean(no_armor_df$resid_raw),
                           sd = sd(no_armor_df$resid_raw)),
                color = "darkred", linewidth = 1) +
  labs(title = "Residuals WITHOUT Floor",
       subtitle = sprintf("Shapiro p = %.4f", shapiro.test(no_armor_df$resid_raw)$p.value),
       x = "Residual", y = "Density") +
  theme_minimal()
print(p1)

p2 <- ggplot(no_armor_df, aes(x = resid_floor10)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5,
                 fill = "blue", alpha = 0.6) +
  stat_function(fun = dnorm,
                args = list(mean = mean(no_armor_df$resid_floor10),
                           sd = sd(no_armor_df$resid_floor10)),
                color = "darkblue", linewidth = 1) +
  labs(title = "Residuals WITH Floor = 10",
       subtitle = sprintf("Shapiro p = %.4f", shapiro.test(no_armor_df$resid_floor10)$p.value),
       x = "Residual", y = "Density") +
  theme_minimal()
print(p2)

# Q-Q plots
p3 <- ggplot(no_armor_df, aes(sample = resid_raw)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot WITHOUT Floor") +
  theme_minimal()
print(p3)

p4 <- ggplot(no_armor_df, aes(sample = resid_floor10)) +
  stat_qq() + stat_qq_line(color = "blue") +
  labs(title = "Q-Q Plot WITH Floor = 10") +
  theme_minimal()
print(p4)

# Predicted vs Actual comparison
p5 <- ggplot(no_armor_df, aes(x = pred_floor10, y = level)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual WITH Floor = 10",
       x = "Predicted Level (floored at 10)",
       y = "Actual Level") +
  theme_minimal()
print(p5)

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("                    SUMMARY\n")
cat("=============================================================\n\n")

cat("FLOOR HYPOTHESIS TEST RESULTS:\n\n")

if (best_shapiro$shapiro_p > 0.05) {
  cat("SUCCESS! Floor =", best_shapiro$floor, "makes residuals NORMALLY DISTRIBUTED\n")
  cat("  Shapiro-Wilk p-value:", round(best_shapiro$shapiro_p, 4), "\n")
  cat("  This suggests the game has a minimum level floor of", best_shapiro$floor, "\n")
} else {
  cat("Floor alone does not achieve normality.\n")
  cat("Best floor =", best_shapiro$floor, "with p =", round(best_shapiro$shapiro_p, 4), "\n")
  cat("There may be additional factors to consider.\n")
}

cat("\n\nUPDATED FORMULA (with floor):\n")
cat("level = max(10,\n")
cat("            9 + 0.01*hardiness - 0.02*fortitude + 0.01*dexterity\n")
cat("              + 0.01*intellect + 0.025*cleverness + 0.015*power\n")
cat("              + 0.12*kinen + 0.06*nonkinen)\n")
