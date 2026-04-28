###############################################################################
# High-DPS under-prediction investigation
#
# Goal: figure out why M7 under-predicts the apex of the unarmored CL
# distribution (rancor CL48 +7.0, falumpaset CL32 +5.9, razor cat CL49 +5.4).
#
# Tests:
#   (A) Reproduce M7 residuals; list worst under-predictions.
#   (B) Skin-specificity: do these skins under-predict consistently, or only
#       at high cleverness?
#   (C) Cleverness/power threshold effects under M7.
#   (D) Multiplicative DPS terms: dps = damage * speed * to_hit, and the
#       cleverness * power interaction.
###############################################################################
source("R/data.R")

suppressPackageStartupMessages({
  library(dplyr)
})

# M7 (canonical unarmored CL formula, copied from R/creature_level_model.R
# so we don't trigger the script-level code at the bottom of that file).
m7_predict <- function(d) {
  ke_floor <- pmax(pmin(d$kinetic, d$energy), 0)
  nk <- (d$blast + d$heat + d$cold + d$electricity + d$acid + d$stun) / 6
  7.873163 +
    0.013015 * d$hardiness +
   -0.019361 * d$fortitude +
    0.004101 * d$dexterity +
    0.010102 * d$intellect +
    0.024797 * d$cleverness +
    0.015077 * d$power +
    0.166537 * ke_floor +
    0.054300 * nk
}

cat("=============================================================\n")
cat("   HIGH-DPS UNDER-PREDICTION INVESTIGATION (under M7)\n")
cat("=============================================================\n\n")

###############################################################################
# (A) M7 baseline on unarmored set
###############################################################################

no_armor_df <- normalized_df %>% filter(armor == 0)
no_armor_df$pred_m7 <- m7_predict(no_armor_df)
no_armor_df$resid_m7 <- no_armor_df$level - no_armor_df$pred_m7

cat("--- (A) M7 baseline ---\n")
cat(sprintf("  n unarmored:      %d\n", nrow(no_armor_df)))
cat(sprintf("  R^2 (M7):         %.4f\n",
            1 - var(no_armor_df$resid_m7) / var(no_armor_df$level)))
cat(sprintf("  SD of residuals:  %.3f levels\n", sd(no_armor_df$resid_m7)))
cat(sprintf("  Mean residual:    %.3f\n", mean(no_armor_df$resid_m7)))
cat("\n  Top 10 under-predicted (actual > predicted):\n")
print(no_armor_df %>%
        arrange(desc(resid_m7)) %>%
        select(serial, skin, level, pred_m7, resid_m7,
               cleverness, power, fortitude, kinen, nonkinen) %>%
        head(10) %>%
        mutate(across(c(pred_m7, resid_m7), ~round(.x, 2))) %>%
        as.data.frame())

###############################################################################
# (B) Skin-specific under-prediction
###############################################################################

cat("\n\n--- (B) Skin-specific bias ---\n")

skin_bias <- no_armor_df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_resid = mean(resid_m7),
    sd_resid = sd(resid_m7),
    max_clev = max(cleverness),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  arrange(desc(mean_resid))

cat("  Top 10 skins by mean M7 residual (n>=3):\n")
print(skin_bias %>% head(10) %>%
        mutate(across(c(mean_resid, sd_resid), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  Bottom 5 skins (most over-predicted, n>=3):\n")
print(skin_bias %>% tail(5) %>%
        mutate(across(c(mean_resid, sd_resid), ~round(.x, 2))) %>%
        as.data.frame())

# Are the apex creatures' skins universally biased, or only their high-clev members?
cat("\n  Suspect skins -- residual stratified by within-skin cleverness rank:\n")
for (s in c("rancor", "falumpaset", "razor_cat")) {
  sub <- no_armor_df %>% filter(skin == s)
  if (nrow(sub) == 0) next
  cat(sprintf("\n  Skin: %s (n=%d)\n", s, nrow(sub)))
  print(sub %>%
          arrange(desc(cleverness)) %>%
          select(serial, level, pred_m7, resid_m7, cleverness, power, fortitude) %>%
          mutate(across(c(pred_m7, resid_m7), ~round(.x, 2))) %>%
          as.data.frame())
}

###############################################################################
# (C) Cleverness / power threshold effects under M7
###############################################################################

cat("\n\n--- (C) Threshold effects under M7 ---\n")

# Augment data with hinge variables
df <- no_armor_df %>%
  mutate(
    ke_floor = pmax(pmin(kinetic, energy), 0),
    clev200 = pmax(cleverness - 200, 0),
    clev300 = pmax(cleverness - 300, 0),
    clev400 = pmax(cleverness - 400, 0),
    pow200 = pmax(power - 200, 0),
    pow300 = pmax(power - 300, 0),
    pow400 = pmax(power - 400, 0)
  )

# Baseline M7 as a fitted lm so we can compare AIC honestly
m7_form <- level ~ hardiness + fortitude + dexterity + intellect +
                   cleverness + power + ke_floor + nonkinen
fit_m7 <- lm(m7_form, data = df)

cat(sprintf("  Refit M7 (lm) -- R^2: %.4f, SD: %.3f, AIC: %.1f\n",
            summary(fit_m7)$r.squared, sd(resid(fit_m7)), AIC(fit_m7)))

threshold_terms <- list(
  "+ clev200"               = ~ . + clev200,
  "+ clev300"               = ~ . + clev300,
  "+ clev400"               = ~ . + clev400,
  "+ pow200"                = ~ . + pow200,
  "+ pow300"                = ~ . + pow300,
  "+ pow400"                = ~ . + pow400,
  "+ clev300 + pow300"      = ~ . + clev300 + pow300,
  "+ clev200+clev300+clev400" = ~ . + clev200 + clev300 + clev400,
  "+ clev400 + pow400"      = ~ . + clev400 + pow400
)

cat("\n  Adding threshold terms to M7 (delta_AIC vs M7; positive = improvement):\n")
cat(sprintf("  %-32s %8s %8s %10s %10s\n",
            "model", "R^2", "SD", "dAIC", "newCoef(s)"))
for (nm in names(threshold_terms)) {
  fit <- update(fit_m7, threshold_terms[[nm]])
  added <- setdiff(names(coef(fit)), names(coef(fit_m7)))
  coefs <- paste(sprintf("%s=%.4f", added, coef(fit)[added]), collapse = ", ")
  cat(sprintf("  %-32s %8.4f %8.3f %10.1f   %s\n",
              nm,
              summary(fit)$r.squared,
              sd(resid(fit)),
              AIC(fit_m7) - AIC(fit),
              coefs))
}

###############################################################################
# (D) Multiplicative DPS terms
###############################################################################

cat("\n\n--- (D) Multiplicative DPS / interaction terms ---\n")

df <- df %>%
  mutate(
    dps_raw = ((damage_high + damage_low) / 2) * to_hit * speed,
    dps_log = log1p(dps_raw),
    clev_pow = cleverness * power / 1000,    # rescale so coef isn't tiny
    clev_pow_sqrt = sqrt(cleverness * power)
  )

cat("  Correlation of M7 residual with candidate DPS terms:\n")
for (v in c("dps_raw", "dps_log", "clev_pow", "clev_pow_sqrt",
            "cleverness", "power", "damage_high", "damage_low",
            "speed", "to_hit")) {
  cat(sprintf("    %-16s  r = %+.3f\n", v, cor(df[[v]], df$resid_m7)))
}

dps_terms <- list(
  "+ dps_raw"        = ~ . + dps_raw,
  "+ dps_log"        = ~ . + dps_log,
  "+ clev_pow"       = ~ . + clev_pow,
  "+ clev_pow_sqrt"  = ~ . + clev_pow_sqrt,
  "+ I(cleverness*power)" = ~ . + I(cleverness * power),
  "+ I(damage_high*speed)" = ~ . + I(damage_high * speed)
)

cat("\n  Adding multiplicative terms to M7:\n")
cat(sprintf("  %-32s %8s %8s %10s %s\n",
            "model", "R^2", "SD", "dAIC", "newCoef(s)"))
for (nm in names(dps_terms)) {
  fit <- update(fit_m7, dps_terms[[nm]])
  added <- setdiff(names(coef(fit)), names(coef(fit_m7)))
  coefs <- paste(sprintf("%s=%.5g", added, coef(fit)[added]), collapse = ", ")
  cat(sprintf("  %-32s %8.4f %8.3f %10.1f   %s\n",
              nm,
              summary(fit)$r.squared,
              sd(resid(fit)),
              AIC(fit_m7) - AIC(fit),
              coefs))
}

###############################################################################
# (E) Did any term close the rancor/falumpaset/razor_cat gap specifically?
###############################################################################

cat("\n\n--- (E) Apex-creature residuals before vs after best candidate(s) ---\n")

apex_serials <- c("6j048r2a", "01oatm1v", "pdefjush", "1lc95n55")

# Pick the most promising candidates from above to eyeball: clev400, dps_raw,
# clev_pow, and a combined model.
candidates <- list(
  "M7 baseline"      = fit_m7,
  "M7 + clev400"     = update(fit_m7, ~ . + clev400),
  "M7 + dps_raw"     = update(fit_m7, ~ . + dps_raw),
  "M7 + clev_pow"    = update(fit_m7, ~ . + clev_pow),
  "M7 + clev400 + dps_raw" = update(fit_m7, ~ . + clev400 + dps_raw)
)

for (nm in names(candidates)) {
  fit <- candidates[[nm]]
  preds <- predict(fit, newdata = df)
  resids <- df$level - preds
  cat(sprintf("\n  %s   (R^2=%.4f, SD=%.3f, AIC=%.1f)\n",
              nm, 1 - var(resids) / var(df$level), sd(resids), AIC(fit)))
  apex_rows <- df %>%
    mutate(.pred = preds, .resid = resids) %>%
    filter(serial %in% apex_serials) %>%
    select(serial, skin, level, .pred, .resid, cleverness, power) %>%
    mutate(across(c(.pred, .resid), ~round(.x, 2)))
  print(as.data.frame(apex_rows))
}

###############################################################################
# (F) Knot-location scan for the cleverness hinge
###############################################################################

cat("\n\n--- (F) Cleverness knot-location scan ---\n")
cat("  Single hinge pmax(cleverness - K, 0), K in 100..500 step 25:\n")
cat(sprintf("  %5s %8s %8s %10s %10s\n", "K", "R^2", "SD", "dAIC", "coef"))
best <- list(K = NA, dAIC = -Inf, fit = NULL)
for (K in seq(100, 500, by = 25)) {
  df$.hinge <- pmax(df$cleverness - K, 0)
  fit <- update(fit_m7, ~ . + .hinge)
  d <- AIC(fit_m7) - AIC(fit)
  cat(sprintf("  %5d %8.4f %8.3f %10.1f %10.4f\n",
              K, summary(fit)$r.squared, sd(resid(fit)), d, coef(fit)[".hinge"]))
  if (d > best$dAIC) best <- list(K = K, dAIC = d, fit = fit)
}
cat(sprintf("\n  Best knot: K=%d  (dAIC=%.1f)\n", best$K, best$dAIC))

###############################################################################
# (G) Skin-bias check after the best-knot fix
###############################################################################

cat("\n--- (G) Skin residuals after M7 + best cleverness hinge ---\n")
df$.hinge_best <- pmax(df$cleverness - best$K, 0)
fit_best <- update(fit_m7, ~ . + .hinge_best)
df$resid_best <- df$level - predict(fit_best, newdata = df)

skin_after <- df %>%
  group_by(skin) %>%
  summarise(
    n = n(),
    mean_resid_m7 = mean(resid_m7),
    mean_resid_best = mean(resid_best),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  arrange(desc(abs(mean_resid_best)))

cat(sprintf("  (using K=%d hinge)\n", best$K))
print(skin_after %>% head(15) %>%
        mutate(across(starts_with("mean_"), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  Apex residuals after best-knot fix:\n")
print(df %>%
        filter(serial %in% apex_serials) %>%
        mutate(.pred = predict(fit_best, newdata = .),
               .resid = level - .pred) %>%
        select(serial, skin, level, cleverness, .pred, .resid) %>%
        mutate(across(c(.pred, .resid), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  Coefficient summary of best-knot fit:\n")
print(round(coef(fit_best), 5))

###############################################################################
# (H) Rancor skin: residual still biased after the fix?
###############################################################################

cat("\n--- (H) Rancor-skin specific check after fix ---\n")
df_rancor <- df %>% filter(skin == "rancor")
df_rancor$.pred_best <- predict(fit_best, newdata = df_rancor)
df_rancor$.resid_best <- df_rancor$level - df_rancor$.pred_best
print(df_rancor %>%
        select(serial, level, cleverness, power, fortitude,
               pred_m7, .pred_best, resid_m7, .resid_best) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n=============================================================\n")
cat("   END\n")
cat("=============================================================\n")
