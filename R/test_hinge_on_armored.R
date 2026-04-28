###############################################################################
# Does the cleverness hinge that improves the unarmored formula (M8) also
# improve the armored formula?
#
# Approach:
#   (A) Refit the armored baseline (no hinge) cleanly. Print residuals,
#       list top under- and over-predictions, focus on high-cleverness.
#   (B) Knot scan: pmax(cleverness - K, 0) for K in 100..500.
#   (C) Compare three competing models with formal AIC / CV:
#         - armored baseline
#         - armored + free-coef hinge at K=400
#         - armored + hinge with coef forced to the unarmored value (0.106)
#   (D) Bootstrap the hinge coefficient at K=400 to see if 0 is in the CI.
###############################################################################
source("R/data.R")

suppressPackageStartupMessages({
  library(dplyr)
})

set.seed(20260427)

build_features <- function(d) {
  d$ke_floor <- pmax(pmin(d$kinetic, d$energy), 0)
  d$nk       <- (d$blast + d$heat + d$cold +
                 d$electricity + d$acid + d$stun) / 6
  d$clev_h400 <- pmax(d$cleverness - 400, 0)
  d
}

###############################################################################
# Data
###############################################################################
df <- normalized_df %>% filter(armor == 1) %>% build_features()

cat(sprintf("n armored = %d\n", nrow(df)))
cat(sprintf("creatures with cleverness >= 300: %d\n",
            sum(df$cleverness >= 300)))
cat(sprintf("creatures with cleverness >= 400: %d\n",
            sum(df$cleverness >= 400)))
cat(sprintf("max cleverness: %d\n", max(df$cleverness)))

###############################################################################
# (A) Armored baseline
#
# The current canonical armored formula uses kinetic/energy as separate terms
# and the 6 nonkinen resists as separate terms (see creature_level_armor in
# R/creature_level_model.R). For an apples-to-apples comparison with the
# unarmored M8 hinge analysis, refit a structurally similar OLS:
#   level ~ hardiness + fortitude + dex + int + clev + power + kinetic
#         + energy + (each nonkinen resist)
# which is the same parameterisation the canonical fit was derived from.
#
# Note: the armored formula does NOT use a ke_floor term; it uses kinetic
# and energy separately because for armored creatures both are typically
# positive and uncorrelated with vulnerabilities.
###############################################################################
cat("\n=== (A) Armored baseline ===\n")

armored_form <- level ~ hardiness + fortitude + dexterity + intellect +
                        cleverness + power +
                        kinetic + energy +
                        blast + heat + cold + electricity + acid + stun

fit_base <- lm(armored_form, data = df)
df$pred_base  <- predict(fit_base)
df$resid_base <- df$level - df$pred_base

cat(sprintf("  R^2:  %.4f\n", summary(fit_base)$r.squared))
cat(sprintf("  SD:   %.4f\n", sd(resid(fit_base))))
cat(sprintf("  AIC:  %.2f\n", AIC(fit_base)))

cat("\n  Top 8 most under-predicted (level > pred):\n")
print(df %>% arrange(desc(resid_base)) %>%
        select(serial, skin, level, pred_base, resid_base,
               cleverness, power, fortitude) %>%
        head(8) %>%
        mutate(across(c(pred_base, resid_base), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  Top 8 most over-predicted (level < pred):\n")
print(df %>% arrange(resid_base) %>%
        select(serial, skin, level, pred_base, resid_base,
               cleverness, power, fortitude) %>%
        head(8) %>%
        mutate(across(c(pred_base, resid_base), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  Top 5 highest-cleverness armored creatures:\n")
print(df %>% arrange(desc(cleverness)) %>%
        select(serial, skin, level, cleverness, pred_base, resid_base) %>%
        head(5) %>%
        mutate(across(c(pred_base, resid_base), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  Residual stratified by cleverness band:\n")
df$clev_band <- cut(df$cleverness,
                    breaks = c(-Inf, 200, 300, 400, 500, Inf),
                    labels = c("<=200", "200-300", "300-400", "400-500", ">500"))
print(df %>% group_by(clev_band) %>%
        summarise(n = n(),
                  mean_resid = round(mean(resid_base), 3),
                  sd_resid   = round(sd(resid_base), 3),
                  .groups = "drop") %>%
        as.data.frame())

cat(sprintf("\n  Correlation of cleverness with armored residual: r = %+.3f\n",
            cor(df$cleverness, df$resid_base)))

###############################################################################
# (B) Knot scan
###############################################################################
cat("\n=== (B) Knot scan: armored + pmax(cleverness - K, 0) ===\n")
cat(sprintf("  %5s %8s %8s %10s %12s\n", "K", "R^2", "SD", "dAIC", "hinge_coef"))
cat(sprintf("  %5s %8.4f %8.3f %10.1f %12s\n",
            "(M_A)", summary(fit_base)$r.squared, sd(resid(fit_base)), 0, "(no hinge)"))

best <- list(K = NA, dAIC = -Inf, fit = NULL, coef = NA)
for (K in seq(100, 500, by = 25)) {
  df$.h <- pmax(df$cleverness - K, 0)
  fit <- update(fit_base, ~ . + .h)
  cf <- coef(fit)[".h"]
  d <- AIC(fit_base) - AIC(fit)
  cat(sprintf("  %5d %8.4f %8.3f %10.1f %12.4f\n",
              K, summary(fit)$r.squared, sd(resid(fit)), d, cf))
  if (!is.na(d) && d > best$dAIC) {
    best <- list(K = K, dAIC = d, fit = fit, coef = cf)
  }
}
cat(sprintf("\n  Best knot: K=%s  dAIC=%.1f  coef=%.4f\n",
            best$K, best$dAIC, best$coef))

###############################################################################
# (C) Three-way comparison
###############################################################################
cat("\n=== (C) Three-way model comparison ===\n")

fit_free  <- update(fit_base, ~ . + I(pmax(cleverness - 400, 0)))

# Force the unarmored hinge coefficient (0.105771) by adding an offset term
df$.fixed_hinge <- 0.105771 * pmax(df$cleverness - 400, 0)
fit_forced <- lm(I(level - .fixed_hinge) ~ hardiness + fortitude + dexterity +
                   intellect + cleverness + power +
                   kinetic + energy + blast + heat + cold +
                   electricity + acid + stun, data = df)

# Compare on a common (level) scale
df$pred_forced <- predict(fit_forced) + df$.fixed_hinge
sse_forced  <- sum((df$level - df$pred_forced)^2)
n  <- nrow(df); p_forced <- length(coef(fit_forced))   # offset costs no params
# AIC for the forced model on (level) scale: same RSS, same p as base
aic_forced  <- n * log(sse_forced / n) + 2 * (p_forced + 1)
sd_forced   <- sd(df$level - df$pred_forced)
r2_forced   <- 1 - sse_forced / sum((df$level - mean(df$level))^2)

cat(sprintf("  %-40s  %8s %8s %10s %12s\n",
            "model", "R^2", "SD", "dAIC_base", "hinge coef"))
cat(sprintf("  %-40s  %8.4f %8.3f %10.1f %12s\n",
            "armored baseline (no hinge)",
            summary(fit_base)$r.squared, sd(resid(fit_base)), 0, "—"))
cat(sprintf("  %-40s  %8.4f %8.3f %10.1f %12.4f\n",
            "armored + free hinge at K=400",
            summary(fit_free)$r.squared, sd(resid(fit_free)),
            AIC(fit_base) - AIC(fit_free),
            coef(fit_free)["I(pmax(cleverness - 400, 0))"]))
cat(sprintf("  %-40s  %8.4f %8.3f %10.1f %12.4f\n",
            "armored + hinge forced to 0.106 (unarm)",
            r2_forced, sd_forced, AIC(fit_base) - aic_forced, 0.105771))

cat("\n  ANOVA: does the K=400 hinge significantly improve fit?\n")
print(anova(fit_base, fit_free))

###############################################################################
# (D) Bootstrap the hinge coefficient at K=400
###############################################################################
cat("\n=== (D) Bootstrap hinge coefficient (K=400, n_boot=500) ===\n")

n_boot <- 500
coefs <- numeric(n_boot)
for (b in seq_len(n_boot)) {
  idx <- sample(seq_len(nrow(df)), replace = TRUE)
  d_b <- df[idx, ]
  d_b$.h <- pmax(d_b$cleverness - 400, 0)
  fit_b <- update(fit_base, ~ . + .h, data = d_b)
  cf <- coef(fit_b)[".h"]
  coefs[b] <- ifelse(length(cf) == 1, as.numeric(cf), NA_real_)
}
ok <- coefs[!is.na(coefs)]
cat(sprintf("  n_valid_bootstraps = %d\n", length(ok)))
cat(sprintf("  median coef     = %.4f\n", median(ok)))
cat(sprintf("  95%% CI         = [%.4f, %.4f]\n",
            quantile(ok, 0.025), quantile(ok, 0.975)))
cat(sprintf("  P(coef > 0)     = %.3f\n", mean(ok > 0)))
cat(sprintf("  P(coef > 0.05)  = %.3f\n", mean(ok > 0.05)))
cat(sprintf("  P(coef > 0.106) = %.3f   (unarmored point estimate)\n",
            mean(ok > 0.105771)))
cat(sprintf("  Unarmored M8 coef for reference: 0.1058\n"))

###############################################################################
# (E) Sanity: top influencers on the armored hinge (if it exists)
###############################################################################
cat("\n=== (E) Top influencers on the armored hinge coefficient ===\n")
infl <- influence.measures(fit_free)
inf_mat <- as.data.frame(infl$infmat)
inf_mat$serial <- df$serial
inf_mat$skin   <- df$skin
inf_mat$level  <- df$level
inf_mat$cleverness <- df$cleverness
inf_mat$resid  <- residuals(fit_free)

# Find the DFBETAS column for the hinge term
hinge_col <- grep("pmax", names(inf_mat), value = TRUE)
cat(sprintf("  DFBETAS column for hinge: %s\n", hinge_col))
cat("\n  Top 8 by |DFBETAS hinge|:\n")
print(inf_mat %>% arrange(desc(abs(.data[[hinge_col]]))) %>%
        select(serial, skin, level, cleverness, resid,
               !!hinge_col, cook.d) %>%
        head(8) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        as.data.frame())

cat("\n=== END ===\n")
