###############################################################################
# Near-armor-border zone investigation.
#
# The custom_model uses fortitude >= 500 as the splitter between unarmored
# (M8) and armored formulas. Both formulas fit worst around fort 380-540.
#
# Hypotheses to test (in order of explanatory power):
#   (B1) The `armor` flag in creatures.csv is the ground truth and the
#        fort=500 split is just a noisy proxy. If armor flag perfectly
#        agrees with fort>=500, this hypothesis is moot. If they disagree,
#        we test which side better predicts the disputed creatures.
#   (B2) Three-segment fortitude piecewise: fort<450, 450..540, >=540.
#        If a middle segment exists with its own coefficients, the boundary
#        confusion may be a real "light armor" mechanic.
#   (B3) Skin-stratified residuals in the 380-540 band.
###############################################################################
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# Load RAW creatures data WITHOUT the data.R bad_data / minimum-CL filters,
# because we want to see the disagreements that may have been filtered out.
creatures_raw <- read_csv("data/clean/furrycat/creatures.csv",
                          show_col_types = FALSE)
templates_raw <- read_csv("data/clean/furrycat/templates.csv",
                          show_col_types = FALSE)

cat(sprintf("Raw counts: %d creatures, %d templates\n",
            nrow(creatures_raw), nrow(templates_raw)))

# We need fortitude (template-side) joined to armor (creature-side).
# Each creature has a template_id; each template has fortitude.
# But to_template column in templates.csv is "id" not "template_id"...
cat("Templates columns: ", paste(head(names(templates_raw), 10), collapse=", "), "\n")
cat("Creatures columns: ", paste(head(names(creatures_raw), 10), collapse=", "), "\n")

# Looking at headers from earlier: templates uses `serial` AND `id`.
# creatures has `template_id`.
# Try the join.
joined <- creatures_raw %>%
  inner_join(templates_raw %>%
               select(serial, fortitude, hardiness, dexterity, intellect,
                      cleverness, power),
             by = c("template_id" = "serial"))

cat(sprintf("\nJoined rows: %d\n", nrow(joined)))

###############################################################################
# (B1) Armor flag vs fortitude=500 split
###############################################################################
cat("\n=== (B1) Armor flag vs fortitude>=500 split ===\n\n")

joined <- joined %>% mutate(fort_armored = fortitude >= 500)

cat("  Cross-tab: armor flag (creature) x fort>=500 (template)\n\n")
xt <- table(armor_flag = joined$armor, fort_ge_500 = joined$fort_armored)
print(xt)

cat("\n  Disagreement summary:\n")
cat(sprintf("    armor=1 but fort<500:   %d  (would 'light armor' explain?)\n",
            sum(joined$armor == 1 & joined$fortitude < 500)))
cat(sprintf("    armor=0 but fort>=500:  %d  (high fort, no AR slot used)\n",
            sum(joined$armor == 0 & joined$fortitude >= 500)))
cat(sprintf("    Total disagreements:    %d / %d  (%.1f%%)\n",
            sum((joined$armor == 1) != joined$fort_armored),
            nrow(joined),
            100 * sum((joined$armor == 1) != joined$fort_armored) / nrow(joined)))

cat("\n  Distribution of `armor` values:\n")
print(table(joined$armor))

# Inspect the disagreements
cat("\n  All armor=1 with fort<500 (n<=20 shown):\n")
print(joined %>% filter(armor == 1, fortitude < 500) %>%
        select(serial, skin, level, fortitude, hardiness, cleverness, power, armor) %>%
        arrange(fortitude) %>%
        head(20) %>% as.data.frame())

cat("\n  All armor=0 with fort>=500 (n<=20 shown):\n")
print(joined %>% filter(armor == 0, fortitude >= 500) %>%
        select(serial, skin, level, fortitude, hardiness, cleverness, power, armor) %>%
        arrange(fortitude) %>%
        head(20) %>% as.data.frame())

###############################################################################
# Now do the formal test: for each disagreement, predict the level under both
# formulas (M8 unarmored and current armored) and see which is closer.
###############################################################################

# Inline both formulas (copied from R/creature_level_model.R)
predict_unarmored_m8 <- function(d) {
  ke_floor  <- pmax(pmin(d$kinetic, d$energy), 0)
  nk        <- (d$blast + d$heat + d$cold +
                d$electricity + d$acid + d$stun) / 6
  clev_h400 <- pmax(d$cleverness - 400, 0)
  8.132249 +
    0.012301 * d$hardiness - 0.019403 * d$fortitude +
    0.004439 * d$dexterity + 0.011387 * d$intellect +
    0.019508 * d$cleverness + 0.015615 * d$power +
    0.169649 * ke_floor + 0.050378 * nk +
    0.105771 * clev_h400
}
predict_armored <- function(d) {
  -21.331842 +
    (0.027648 / 3) * d$hardiness +
    (0.027648 / 3) * d$dexterity +
    (0.027648 / 3) * d$intellect +
    0.056252 * d$fortitude +
    0.024034 * d$cleverness +
    0.015740 * d$power +
    (0.096920 / 2) * d$kinetic +
    (0.096920 / 2) * d$energy +
    (0.085904 / 6) * d$blast +
    (0.085904 / 6) * d$cold +
    (0.085904 / 6) * d$heat +
    (0.085904 / 6) * d$electricity +
    (0.085904 / 6) * d$acid +
    (0.085904 / 6) * d$stun
}

# Need the raw stats (kinetic etc.) on creatures.csv directly (not template)
# Check that creatures.csv has them
cat("\n  creatures.csv resist columns: ",
    paste(grep("kinetic|energy|blast", names(creatures_raw), value = TRUE),
          collapse=", "), "\n")

# Use joined but the kinetic etc. should come from creatures.csv side
# In the join above we only kept template stats; need to keep creature resists
# Let's redo the join keeping all creature columns
joined_full <- creatures_raw %>%
  inner_join(templates_raw %>%
               select(serial, fortitude, hardiness, dexterity, intellect,
                      cleverness, power),
             by = c("template_id" = "serial"))

# (creature resists 'kinetic', 'energy', etc. are creature side; stats are
# template side.) Predict with both formulas.
joined_full$pred_unarm <- predict_unarmored_m8(joined_full)
joined_full$pred_arm   <- predict_armored(joined_full)
joined_full$err_unarm  <- joined_full$level - joined_full$pred_unarm
joined_full$err_arm    <- joined_full$level - joined_full$pred_arm

cat("\n=== (B1b) Which formula better fits each disagreement? ===\n")

cat("\n  armor=1 with fort<500 (the 'light armor' candidates):\n")
disagree1 <- joined_full %>% filter(armor == 1, fortitude < 500) %>%
  arrange(fortitude) %>%
  mutate(better = ifelse(abs(err_unarm) < abs(err_arm), "unarm", "armored"),
         improvement = abs(err_arm) - abs(err_unarm))
print(disagree1 %>%
        select(serial, skin, level, fortitude, pred_unarm, pred_arm,
               err_unarm, err_arm, better) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  armor=0 with fort>=500 (high-fort no-AR creatures):\n")
disagree2 <- joined_full %>% filter(armor == 0, fortitude >= 500) %>%
  arrange(fortitude) %>%
  mutate(better = ifelse(abs(err_unarm) < abs(err_arm), "unarm", "armored"),
         improvement = abs(err_arm) - abs(err_unarm))
print(disagree2 %>%
        select(serial, skin, level, fortitude, pred_unarm, pred_arm,
               err_unarm, err_arm, better) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  Summary of which formula wins for each disagreement type:\n")
cat(sprintf("    armor=1, fort<500: unarm wins for %d/%d  (mean improvement using %s: %.2f)\n",
            sum(disagree1$better == "unarm"), nrow(disagree1),
            ifelse(mean(disagree1$improvement) > 0, "armored", "unarm"),
            abs(mean(disagree1$improvement))))
cat(sprintf("    armor=0, fort>=500: unarm wins for %d/%d  (mean improvement using %s: %.2f)\n",
            sum(disagree2$better == "unarm"), nrow(disagree2),
            ifelse(mean(disagree2$improvement) > 0, "armored", "unarm"),
            abs(mean(disagree2$improvement))))

###############################################################################
# Now: use the FILTERED dataset (the production analysis set) and check the
# residual pattern in the 380-540 fortitude band stratified by skin.
###############################################################################
cat("\n=== (B3) Filtered set: residuals near the border ===\n")

source("R/data.R")    # this gives us normalized_df with the standard filters
filtered <- normalized_df

filtered$pred_unarm <- predict_unarmored_m8(filtered)
filtered$pred_arm   <- predict_armored(filtered)
filtered$pred_used  <- ifelse(filtered$fortitude < 500,
                              filtered$pred_unarm, filtered$pred_arm)
filtered$resid      <- filtered$level - filtered$pred_used
filtered$which_form <- ifelse(filtered$fortitude < 500, "unarm", "arm")

cat("\n  Mean residual stratified by fortitude band (using current splitter):\n")
filtered$fort_band <- cut(filtered$fortitude,
                          breaks = c(-Inf, 200, 380, 450, 500, 540, 600, Inf),
                          labels = c("<200", "200-380", "380-450",
                                     "450-499", "500-540", "540-600", ">=600"))
print(filtered %>%
        group_by(fort_band, which_form) %>%
        summarise(n = n(),
                  mean_resid = round(mean(resid), 2),
                  sd_resid   = round(sd(resid), 2),
                  .groups = "drop") %>%
        as.data.frame())

cat("\n  In the 380-540 band, residuals by skin (n>=3):\n")
print(filtered %>%
        filter(fortitude >= 380, fortitude < 540) %>%
        group_by(skin, which_form) %>%
        summarise(n = n(),
                  mean_resid = round(mean(resid), 2),
                  .groups = "drop") %>%
        filter(n >= 3) %>%
        arrange(which_form, desc(abs(mean_resid))) %>%
        as.data.frame())

###############################################################################
# (B2) Three-segment fortitude piecewise model on the FULL filtered dataset
#
# Fit a single combined model with three regimes and let the data choose.
# This is most directly comparable to the current two-formula custom_model.
###############################################################################
cat("\n=== (B2) Three-segment piecewise model ===\n")

# Build the model on the full filtered set with regime indicators
df <- filtered %>%
  mutate(ke_floor = pmax(pmin(kinetic, energy), 0),
         nk       = (blast + heat + cold + electricity + acid + stun) / 6,
         clev_h400 = pmax(cleverness - 400, 0),
         seg = case_when(
           fortitude < 450  ~ "low",
           fortitude < 540  ~ "mid",
           TRUE             ~ "high"
         ),
         seg = factor(seg, levels = c("low", "mid", "high")))

cat("  Segment counts:\n")
print(table(df$seg))

# Model 1: TWO-FORMULA equivalent (fort<500 / fort>=500), but as one fit
df$two <- ifelse(df$fortitude < 500, "unarm", "arm")
fit_two <- lm(level ~ two + two:hardiness + two:fortitude + two:dexterity +
                       two:intellect + two:cleverness + two:power +
                       two:ke_floor + two:nk + two:clev_h400, data = df)

# Model 2: THREE-SEGMENT
fit_three <- lm(level ~ seg + seg:hardiness + seg:fortitude + seg:dexterity +
                         seg:intellect + seg:cleverness + seg:power +
                         seg:ke_floor + seg:nk + seg:clev_h400, data = df)

cat(sprintf("\n  Two-segment (fort<500 / >=500):  R^2=%.4f  SD=%.4f  AIC=%.2f  df=%d\n",
            summary(fit_two)$r.squared, sd(resid(fit_two)),
            AIC(fit_two), length(coef(fit_two))))
cat(sprintf("  Three-segment (low/mid/high):     R^2=%.4f  SD=%.4f  AIC=%.2f  df=%d\n",
            summary(fit_three)$r.squared, sd(resid(fit_three)),
            AIC(fit_three), length(coef(fit_three))))
cat(sprintf("  dAIC (3 vs 2): %+.2f   (positive = 3-seg better)\n",
            AIC(fit_two) - AIC(fit_three)))

cat("\n  ANOVA: 3-segment vs 2-segment\n")
print(anova(fit_two, fit_three))

###############################################################################
# (B4) Fortitude-cutoff scan: is 500 the optimal split?
###############################################################################
cat("\n=== (B4) Fortitude-cutoff scan ===\n")
cat("  For each candidate cutoff T, refit unarmored M8 and armored on the\n")
cat("  resulting partition, then sum SSE and report combined fit.\n\n")

cutoffs <- seq(440, 560, by = 10)

scan_one <- function(T) {
  un <- df %>% filter(fortitude < T)
  ar <- df %>% filter(fortitude >= T)
  if (nrow(un) < 50 || nrow(ar) < 30) return(NULL)
  fit_u <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                       cleverness + power + ke_floor + nk + clev_h400,
              data = un)
  fit_a <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                       cleverness + power +
                       kinetic + energy + blast + heat + cold +
                       electricity + acid + stun,
              data = ar)
  sse <- sum(resid(fit_u)^2) + sum(resid(fit_a)^2)
  n   <- nrow(un) + nrow(ar)
  p   <- length(coef(fit_u)) + length(coef(fit_a)) + 2  # +2 for sigmas
  list(T = T, n_un = nrow(un), n_ar = nrow(ar),
       sse = sse, sd = sqrt(sse / n),
       aic = n * log(sse/n) + 2 * p)
}
results <- lapply(cutoffs, scan_one)
results <- do.call(rbind, lapply(Filter(Negate(is.null), results),
                                 as.data.frame))
results$dAIC <- min(results$aic) - results$aic
print(results %>%
        mutate(across(c(sse, sd, aic, dAIC), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n  3f0lpuko detail (the gurrcat with fort=501, armor=0):\n")
print(filtered %>% filter(serial == "3f0lpuko") %>%
        select(serial, skin, level, fortitude, hardiness, cleverness, power,
               armor, pred_unarm, pred_arm, resid) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        as.data.frame())

cat("\n=== END ===\n")
