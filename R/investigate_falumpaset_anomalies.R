###############################################################################
# Investigate the two falumpaset anomalies that survive M8:
#   1lc95n55  CL30 (clev 193)  resid +8.76
#   01oatm1v  CL32 (clev 191)  resid +5.43
#
# From raw HTML:
#   - Both have attacks "Dizzy/Strong poison/Ranged"
#   - Both have unusually high hardiness (557, 575)
#   - Both have unusual dependability (503, 417)
#   - Both are ranged
#
# Tests:
#   (F1) How do these two compare to the OTHER 8 falumpasets? z-scores
#        on every stat and resist.
#   (F2) Are there other unarmored creatures with similar profile (low
#        cleverness, high hardiness, ranged) and how do they fit?
#   (F3) Do the special-attack flags or `ranged` flag predict residual?
#   (F4) Does the unused `dependability` or `courage` stat help?
#   (F5) Is there a hardiness hinge? Or a HAM hinge?
###############################################################################
source("R/data.R")

suppressPackageStartupMessages({
  library(dplyr)
})

# Inline M8 (current canonical) to compute residuals
m8_predict <- function(d) {
  ke_floor  <- pmax(pmin(d$kinetic, d$energy), 0)
  nk        <- (d$blast + d$heat + d$cold +
                d$electricity + d$acid + d$stun) / 6
  clev_h400 <- pmax(d$cleverness - 400, 0)
  8.132249 +
    0.012301 * d$hardiness +
   -0.019403 * d$fortitude +
    0.004439 * d$dexterity +
    0.011387 * d$intellect +
    0.019508 * d$cleverness +
    0.015615 * d$power +
    0.169649 * ke_floor +
    0.050378 * nk +
    0.105771 * clev_h400
}

df <- normalized_df %>% filter(armor == 0)
df$pred_m8  <- m8_predict(df)
df$resid_m8 <- df$level - df$pred_m8

# What columns are actually available on normalized_df?
cat("Columns available (sample):\n")
print(grep("dep|cour|fier|sa1|sa2|ranged|special|health|action|mind",
           names(df), value = TRUE))

###############################################################################
# (F1) Z-score profile of the two anomalies vs the rest of the unarmored set
###############################################################################
cat("\n=== (F1) Z-score profile of the two anomalies ===\n\n")

anom_serials <- c("1lc95n55", "01oatm1v")
anom <- df %>% filter(serial %in% anom_serials)
rest <- df %>% filter(!(serial %in% anom_serials))

stat_cols <- c("hardiness", "fortitude", "dexterity", "endurance",
               "intellect", "cleverness", "dependability", "courage",
               "fierceness", "power",
               "kinetic", "energy", "blast", "heat", "cold",
               "electricity", "acid", "stun",
               "health", "action", "mind", "speed", "to_hit",
               "damage_low", "damage_high")

cat("  Stat            anom1_value  anom1_z   anom2_value  anom2_z   pop_mean   pop_sd\n")
cat("  ----            -----------  -------   -----------  -------   --------   ------\n")
for (col in stat_cols) {
  if (!(col %in% names(df))) next
  pop_mean <- mean(rest[[col]], na.rm = TRUE)
  pop_sd   <- sd(rest[[col]], na.rm = TRUE)
  v1 <- anom[[col]][anom$serial == "1lc95n55"]
  v2 <- anom[[col]][anom$serial == "01oatm1v"]
  z1 <- (v1 - pop_mean) / pop_sd
  z2 <- (v2 - pop_mean) / pop_sd
  cat(sprintf("  %-15s %11.1f  %+7.2f   %11.1f  %+7.2f   %8.1f   %6.1f\n",
              col, v1, z1, v2, z2, pop_mean, pop_sd))
}

###############################################################################
# (F2) Other unarmored creatures with similar "low-clev, high-hardiness" profile
###############################################################################
cat("\n=== (F2) Look-alike creatures (low cleverness, high hardiness) ===\n")
cat("Filter: cleverness 100-300 AND hardiness >= 500\n\n")

look_alikes <- df %>%
  filter(cleverness >= 100, cleverness <= 300, hardiness >= 500) %>%
  arrange(desc(resid_m8)) %>%
  select(serial, skin, level, hardiness, fortitude, cleverness, power,
         pred_m8, resid_m8)
print(as.data.frame(look_alikes %>%
                      mutate(across(c(pred_m8, resid_m8), ~round(.x, 2)))))

cat(sprintf("\n  n look-alikes: %d   mean resid: %+.2f   median resid: %+.2f\n",
            nrow(look_alikes), mean(look_alikes$resid_m8),
            median(look_alikes$resid_m8)))

###############################################################################
# (F3) Does the special-attack profile predict residual?
###############################################################################
cat("\n=== (F3) Special attacks and residual ===\n\n")

# All falumpaset attacks
cat("  Falumpaset entries with their attacks:\n")
print(df %>% filter(skin == "falumpaset") %>%
        arrange(desc(resid_m8)) %>%
        select(serial, level, cleverness, power, sa1, sa2, ranged,
               pred_m8, resid_m8) %>%
        mutate(across(c(pred_m8, resid_m8), ~round(.x, 2))) %>%
        as.data.frame())

# Population: residual by `ranged`
cat("\n  Mean residual by ranged status (full unarmored set):\n")
print(df %>% group_by(ranged) %>%
        summarise(n = n(),
                  mean_resid = round(mean(resid_m8), 3),
                  median_resid = round(median(resid_m8), 3),
                  sd_resid = round(sd(resid_m8), 3),
                  .groups = "drop") %>%
        as.data.frame())

# By sa1
cat("\n  Top 10 sa1 values by mean residual (n>=4):\n")
print(df %>% group_by(sa1) %>%
        summarise(n = n(), mean_resid = mean(resid_m8), .groups = "drop") %>%
        filter(n >= 4) %>% arrange(desc(mean_resid)) %>%
        head(10) %>%
        mutate(mean_resid = round(mean_resid, 2)) %>%
        as.data.frame())

# By sa2
cat("\n  Top 10 sa2 values by mean residual (n>=4):\n")
print(df %>% group_by(sa2) %>%
        summarise(n = n(), mean_resid = mean(resid_m8), .groups = "drop") %>%
        filter(n >= 4) %>% arrange(desc(mean_resid)) %>%
        head(10) %>%
        mutate(mean_resid = round(mean_resid, 2)) %>%
        as.data.frame())

###############################################################################
# (F4) Does dependability or courage help?
###############################################################################
cat("\n=== (F4) Test currently-unused stats ===\n\n")

cat("  Correlation of M8 residual with currently-unused stats:\n")
for (col in c("dependability", "courage", "fierceness",
              "endurance", "health", "action", "mind",
              "speed", "to_hit", "damage_low", "damage_high")) {
  if (!(col %in% names(df))) next
  r <- cor(df[[col]], df$resid_m8, use = "pairwise.complete.obs")
  cat(sprintf("    %-15s  r = %+.3f\n", col, r))
}

# Add one term at a time, see which improves M8
cat("\n  Adding each as a linear term to M8, dAIC vs M8:\n")
m8_form <- level ~ hardiness + fortitude + dexterity + intellect +
                   cleverness + power +
                   I(pmax(pmin(kinetic, energy), 0)) +
                   I((blast + heat + cold + electricity + acid + stun)/6) +
                   I(pmax(cleverness - 400, 0))
fit_m8 <- lm(m8_form, data = df)
cat(sprintf("    %-25s   dAIC = %+.2f\n", "(M8 baseline)", 0))
for (col in c("dependability", "courage", "fierceness",
              "endurance", "health", "action", "mind",
              "speed", "to_hit", "damage_low", "damage_high",
              "ranged")) {
  if (!(col %in% names(df))) next
  fit <- update(fit_m8, as.formula(paste("~ . +", col)))
  cat(sprintf("    + %-23s   dAIC = %+.2f   coef=%+.5f\n",
              col,
              AIC(fit_m8) - AIC(fit),
              coef(fit)[col]))
}

###############################################################################
# (F5) Is there a hardiness hinge?
###############################################################################
cat("\n=== (F5) Hardiness / HAM hinge scan ===\n")

cat("  Hardiness hinge: + pmax(hardiness - K, 0), K in 100..600 step 50\n")
cat(sprintf("  %5s %8s %8s %10s %10s\n", "K", "R^2", "SD", "dAIC", "coef"))
for (K in seq(100, 600, by = 50)) {
  df$.h <- pmax(df$hardiness - K, 0)
  fit <- update(fit_m8, ~ . + .h)
  cat(sprintf("  %5d %8.4f %8.3f %10.1f %10.5f\n",
              K, summary(fit)$r.squared, sd(resid(fit)),
              AIC(fit_m8) - AIC(fit), coef(fit)[".h"]))
}

cat("\n  HAM-average hinge: + pmax((H+A+M)/3 - K, 0), K in 1000..10000 step 1000\n")
df$ham_avg <- (df$health + df$action + df$mind) / 3
cat(sprintf("  %6s %8s %8s %10s %10s\n", "K", "R^2", "SD", "dAIC", "coef"))
for (K in seq(1000, 10000, by = 1000)) {
  df$.h <- pmax(df$ham_avg - K, 0)
  fit <- update(fit_m8, ~ . + .h)
  cat(sprintf("  %6d %8.4f %8.3f %10.1f %10.7f\n",
              K, summary(fit)$r.squared, sd(resid(fit)),
              AIC(fit_m8) - AIC(fit), coef(fit)[".h"]))
}

cat("\n=== END ===\n")
