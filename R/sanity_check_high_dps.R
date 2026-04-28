###############################################################################
# Sanity check: is the cleverness hinge driven by one bad data point?
#
# Three angles:
#   (S1) Formal influence diagnostics on M7+hinge(K=400). Cook's D, DFFITS,
#        DFBETAS specifically for the hinge coefficient.
#   (S2) Refit with rancor 6j048r2a removed AND with the second-strongest
#        influencer removed; how much does the coefficient move?
#   (S3) Typo plausibility: for 6j048r2a, what would residuals be under
#        single-digit alterations to (level, cleverness, power)?
###############################################################################
source("R/data.R")

suppressPackageStartupMessages({
  library(dplyr)
})

build_features <- function(d) {
  d$ke_floor <- pmax(pmin(d$kinetic, d$energy), 0)
  d$nk       <- (d$blast + d$heat + d$cold +
                 d$electricity + d$acid + d$stun) / 6
  d$clev_h   <- pmax(d$cleverness - 400, 0)
  d
}

df <- normalized_df %>% filter(armor == 0) %>% build_features()

m7_hinge_form <- level ~ hardiness + fortitude + dexterity + intellect +
                         cleverness + power + ke_floor + nk + clev_h

fit <- lm(m7_hinge_form, data = df)

###############################################################################
# (S1) Influence diagnostics
###############################################################################
cat("=== (S1) Influence diagnostics on M7 + hinge(K=400) ===\n\n")

infl <- influence.measures(fit)
inf_mat <- as.data.frame(infl$infmat)
inf_mat$serial <- df$serial
inf_mat$skin   <- df$skin
inf_mat$level  <- df$level
inf_mat$cleverness <- df$cleverness
inf_mat$resid  <- residuals(fit)
inf_mat$hatval <- hatvalues(fit)

# Sort by influence on the hinge coefficient (DFBETAS for clev_h)
cat("Top 10 by |DFBETAS for hinge coefficient (clev_h)|:\n")
print(inf_mat %>%
        arrange(desc(abs(dfb.clv_))) %>%
        select(serial, skin, level, cleverness, resid,
               dfb.clv_, cook.d, hatval) %>%
        head(10) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        as.data.frame())

cat("\nTop 10 by Cook's D:\n")
print(inf_mat %>%
        arrange(desc(cook.d)) %>%
        select(serial, skin, level, cleverness, resid,
               cook.d, dfb.clv_, hatval) %>%
        head(10) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        as.data.frame())

cat("\nTop 10 by leverage (hatvalue):\n")
print(inf_mat %>%
        arrange(desc(hatval)) %>%
        select(serial, skin, level, cleverness, resid,
               hatval, cook.d, dfb.clv_) %>%
        head(10) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        as.data.frame())

# Standard influence cutoffs for context
n <- nrow(df); p <- length(coef(fit))
cat(sprintf("\n  Cutoffs (n=%d, p=%d):\n", n, p))
cat(sprintf("    Cook's D > 4/n      = %.4f\n", 4/n))
cat(sprintf("    |DFFITS| > 2*sqrt(p/n) = %.3f\n", 2 * sqrt(p/n)))
cat(sprintf("    |DFBETAS| > 2/sqrt(n)  = %.3f\n", 2/sqrt(n)))
cat(sprintf("    Hat > 2*p/n         = %.3f\n", 2 * p/n))

###############################################################################
# (S2) Sequential removal of top influencers
###############################################################################
cat("\n=== (S2) Sequential removal of top hinge-influencers ===\n\n")

# Take the top 5 by |DFBETAS for clev_h|
top_inf <- inf_mat %>% arrange(desc(abs(dfb.clv_))) %>% head(5) %>% pull(serial)
cat("Removing top influencers one at a time and refitting:\n")
cat("(coef = hinge coef, ratio = coef/full_coef)\n\n")

cat(sprintf("  %-30s  %12s  %8s\n", "removed", "hinge coef", "ratio"))
full_coef <- coef(fit)["clev_h"]
cat(sprintf("  %-30s  %12.4f  %8s\n", "(none — full data)", full_coef, "1.00"))

removed <- character()
for (s in top_inf) {
  removed <- c(removed, s)
  d_sub <- df %>% filter(!(serial %in% removed))
  fit_sub <- lm(m7_hinge_form, data = d_sub)
  cf <- coef(fit_sub)["clev_h"]
  cat(sprintf("  %-30s  %12.4f  %8.2f\n",
              paste(removed, collapse = ", "),
              cf,
              cf / full_coef))
}

# Also: refit with ALL rancors removed
cat("\n  Drop scenarios:\n")
for (scen in list(
  list(name = "drop rancor 6j048r2a only",      keep = function(d) d %>% filter(serial != "6j048r2a")),
  list(name = "drop both CL48 rancors",         keep = function(d) d %>% filter(!(serial %in% c("6j048r2a","0iafqb8b")))),
  list(name = "drop ALL 3 rancors",             keep = function(d) d %>% filter(skin != "rancor")),
  list(name = "drop falumpaset 1lc95n55 only",  keep = function(d) d %>% filter(serial != "1lc95n55")),
  list(name = "drop both falumpaset outliers",  keep = function(d) d %>% filter(!(serial %in% c("1lc95n55","01oatm1v"))))
)) {
  d_sub <- scen$keep(df)
  fit_sub <- lm(m7_hinge_form, data = d_sub)
  cf <- coef(fit_sub)["clev_h"]
  cat(sprintf("  %-32s  hinge coef = %.4f  (ratio %.2f, n=%d)\n",
              scen$name, cf, cf / full_coef, nrow(d_sub)))
}

###############################################################################
# (S3) Typo plausibility for 6j048r2a
###############################################################################
cat("\n=== (S3) Typo plausibility for 6j048r2a ===\n\n")

target <- df %>% filter(serial == "6j048r2a")
cat("Raw record (from CSV):\n")
print(target %>% select(serial, skin, level, hardiness, fortitude, dexterity,
                        intellect, cleverness, power, kinetic, energy) %>%
        as.data.frame())

# Build M8 = M7 + hinge(K=400) for predictions
predict_m8 <- function(d, fit_obj) predict(fit_obj, newdata = build_features(d))

cat(sprintf("\nObserved level: %d  |  M8 prediction: %.2f  |  residual: %.2f\n",
            target$level,
            predict_m8(target, fit),
            target$level - predict_m8(target, fit)))

# (a) Single-digit alterations to LEVEL
cat("\nWhat alternative LEVEL values would zero the residual?\n")
pred <- predict_m8(target, fit)
cat(sprintf("  Pure M8 prediction = %.2f -> nearest integer level = %d\n",
            pred, round(pred)))
cat("  Plausible single-digit typos for '48':\n")
for (alt in c(40, 38, 28, 18, 84, 44, 49, 47, 41)) {
  cat(sprintf("    level=%2d  ->  resid = %+5.2f  %s\n",
              alt, alt - pred,
              ifelse(abs(alt - pred) < 1.5, "<- plausible", "")))
}

# (b) Single-digit alterations to CLEVERNESS (476)
cat("\nWhat alternative CLEVERNESS values would explain level=48 under M7 (no hinge)?\n")
m7_only_form <- update(m7_hinge_form, ~ . - clev_h)
fit_m7_only <- lm(m7_only_form, data = df)
target_v <- target
for (alt in c(476, 467, 647, 746, 764, 467, 376, 276, 576, 676)) {
  target_v$cleverness <- alt
  target_v <- build_features(target_v)
  pred_v <- predict(fit_m7_only, newdata = target_v)
  cat(sprintf("    cleverness=%3d  ->  M7 pred = %.2f  resid = %+5.2f  %s\n",
              alt, pred_v, target_v$level - pred_v,
              ifelse(abs(target_v$level - pred_v) < 1.5, "<- plausible", "")))
}

# (c) Single-digit alterations to POWER (338)
cat("\nWhat alternative POWER values would explain level=48 under M7 (no hinge)?\n")
target_v <- target
for (alt in c(338, 383, 833, 388, 538, 738, 838, 938, 438, 338)) {
  target_v$power <- alt
  target_v$cleverness <- 476
  target_v <- build_features(target_v)
  pred_v <- predict(fit_m7_only, newdata = target_v)
  cat(sprintf("    power=%3d       ->  M7 pred = %.2f  resid = %+5.2f  %s\n",
              alt, pred_v, target_v$level - pred_v,
              ifelse(abs(target_v$level - pred_v) < 1.5, "<- plausible", "")))
}

# (d) Quick check: does the OTHER CL48 rancor (0iafqb8b) corroborate?
cat("\nFor reference, the OTHER CL48 rancor:\n")
o <- df %>% filter(serial == "0iafqb8b")
print(o %>% select(serial, skin, level, hardiness, fortitude, cleverness, power) %>% as.data.frame())
cat(sprintf("  M7 (no hinge) pred = %.2f  resid = %+.2f\n",
            predict(fit_m7_only, newdata = o), o$level - predict(fit_m7_only, newdata = o)))
cat(sprintf("  M8 (with hinge) pred = %.2f  resid = %+.2f\n",
            predict_m8(o, fit), o$level - predict_m8(o, fit)))
cat("  -> Both CL48 rancors land at the SAME observed level (48) despite\n")
cat("     different stats. Suggests a possible per-skin level cap/floor at 48.\n")

# (e) Are there any other CL48 creatures across the full dataset that might
#     give us a CL48 cluster?
cat("\nAll level=48 unarmored creatures in dataset:\n")
print(df %>% filter(level == 48) %>%
        select(serial, skin, cleverness, power, fortitude) %>%
        as.data.frame())

cat("\nMax level by skin (unarmored, n>=2):\n")
print(df %>% group_by(skin) %>%
        summarise(n = n(), max_lvl = max(level), .groups = "drop") %>%
        filter(n >= 2) %>%
        arrange(desc(max_lvl)) %>%
        head(15) %>%
        as.data.frame())

cat("\n=== END ===\n")
