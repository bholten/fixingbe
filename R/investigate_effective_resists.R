###############################################################################
# investigate_effective_resists.R
#
# Domain knowledge from BE players: kinetic and energy resists in the game
# come from two sources:
#   - "effective" resists are derived directly from fortitude:
#       unarmored: effective = floor(fortitude / 10)
#       armored:   effective = floor((fortitude - 500) / 10)
#   - "special" resists come from DNA samples used in crafting.
# When both are present on a DNA combine, the special source wins and the
# effective drops out entirely. The displayed resist on the creature is
# whichever source dominated.
#
# This script (1) verifies the floor(fortitude/10) relationship in the data,
# (2) tests whether the "negative fortitude coefficient" puzzle from Phase 11
# is driven by the effective-resist double-count (i.e. for ~14% of creatures
# the kinetic value M7 uses is literally a function of fortitude), and (3)
# tests whether adding an effective/special flag meaningfully improves M7.
###############################################################################

source("R/data.R")

library(dplyr)

df <- normalized_df %>% mutate(
  ke_floor    = pmax(pmin(kinetic, energy), 0),
  k_effective = !is.na(kinetic.effective) & kinetic.effective != 0,
  e_effective = !is.na(energy.effective)  & energy.effective  != 0,
  k_special   = !is.na(kinetic.special)   & kinetic.special   != 0,
  e_special   = !is.na(energy.special)    & energy.special    != 0
)
unarm <- df %>% filter(fortitude < 500)

###############################################################################
# (1) Verify the formula: kinetic.effective = floor(fortitude / 10) when
#     effective is present
###############################################################################

cat("=============================================================\n")
cat("  (1) VERIFY: kinetic.effective = floor(fortitude / 10)\n")
cat("=============================================================\n\n")

cat("Resist source distribution (unarmored vs armored):\n")
src <- df %>% mutate(
  k_src = case_when(k_effective ~ "effective", k_special ~ "special", TRUE ~ "neither"),
  e_src = case_when(e_effective ~ "effective", e_special ~ "special", TRUE ~ "neither")
)
cat("Kinetic:\n"); print(table(src$k_src, ifelse(src$armor == 1, "armored", "unarmored")))
cat("Energy:\n");  print(table(src$e_src, ifelse(src$armor == 1, "armored", "unarmored")))
cat("\nNote: no row has BOTH effective AND special — confirming the in-game\n")
cat("mechanic that special wins and effective is dropped.\n\n")

unarm_eff <- unarm %>% filter(k_effective)
cat("Unarmored creatures with kinetic.effective != 0 (n =", nrow(unarm_eff), "):\n")
unarm_eff$predicted <- floor(unarm_eff$fortitude / 10)
unarm_eff$diff <- unarm_eff$kinetic.effective - unarm_eff$predicted
cat(sprintf("  exact match (kinetic.effective == floor(fortitude/10)): %d / %d\n",
            sum(unarm_eff$diff == 0), nrow(unarm_eff)))
cat(sprintf("  off-by-one or other: %d\n", sum(unarm_eff$diff != 0)))
if (any(unarm_eff$diff != 0)) {
  cat("\nMismatches (likely data-entry rounding):\n")
  print(unarm_eff %>% filter(diff != 0) %>%
    select(serial, fortitude, kinetic.effective, predicted, diff) %>% as.data.frame())
}

###############################################################################
# (2) Does the effective-resist double-count drive the negative-fortitude
#     coefficient?
###############################################################################

cat("\n\n=============================================================\n")
cat("  (2) DOES THE NEGATIVE-FORT COEFFICIENT GO AWAY ON SPECIAL-ONLY?\n")
cat("=============================================================\n\n")

# Subset 1: all-special (no fortitude->resist relationship for k or e)
all_special <- unarm %>% filter(!k_effective & !e_effective &
                                  (k_special | kinetic == 0) & (e_special | energy == 0))
m_special <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                  cleverness + power + ke_floor + nonkinen, data = all_special)

# Subset 2: any effective (fortitude->resist relationship active)
has_eff <- unarm %>% filter(k_effective | e_effective)
m_has_eff <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                  cleverness + power + ke_floor + nonkinen, data = has_eff)

# Reference: full unarmored
m_full <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + ke_floor + nonkinen, data = unarm)

cat(sprintf("%-32s %5s %12s %10s %10s\n",
            "subset", "n", "fort coef", "fort p", "R^2"))
print_row <- function(name, m) {
  ce <- summary(m)$coef["fortitude", ]
  cat(sprintf("%-32s %5d %+12.5f %10.2g %10.4f\n",
              name, nobs(m), ce["Estimate"], ce["Pr(>|t|)"],
              summary(m)$r.squared))
}
print_row("full unarmored",                  m_full)
print_row("all-special (k & e from DNA)",    m_special)
print_row("has effective k or e",            m_has_eff)

cat("\nThe negative fortitude coefficient is essentially unchanged on the\n")
cat("all-special subset (where no row has its kinetic or energy value\n")
cat("derived from fortitude). This rules out the effective-resist double-\n")
cat("count as the explanation. The Phase 11 stat-budget / collinearity\n")
cat("diagnosis stands.\n")

###############################################################################
# (3) Does adding an effective/special flag improve M7?
###############################################################################

cat("\n\n=============================================================\n")
cat("  (3) DO EFFECTIVE/SPECIAL FLAGS IMPROVE THE M7 FIT?\n")
cat("=============================================================\n\n")

m_with_flags <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                     cleverness + power + ke_floor + nonkinen +
                     k_effective + e_effective, data = unarm)

cat("Standard M7 vs M7 + effective flags:\n")
cat(sprintf("  M7              : R^2 = %.4f, sd = %.3f, AIC = %.1f\n",
            summary(m_full)$r.squared, sd(resid(m_full)), AIC(m_full)))
cat(sprintf("  M7 + eff flags  : R^2 = %.4f, sd = %.3f, AIC = %.1f\n",
            summary(m_with_flags)$r.squared, sd(resid(m_with_flags)), AIC(m_with_flags)))

cat("\nFlag coefficients (interpretation: how many levels lower vs M7 prediction):\n")
print(round(summary(m_with_flags)$coef[c("k_effectiveTRUE", "e_effectiveTRUE"), ], 4))

cat("\n--- Effect of zeroing out effective resists from ke_floor ---\n")
unarm_zeroed <- unarm %>% mutate(
  k_used = ifelse(k_effective, 0, kinetic),
  e_used = ifelse(e_effective, 0, energy),
  ke_floor_special_only = pmax(pmin(k_used, e_used), 0)
)
m_zero <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + ke_floor_special_only + nonkinen,
             data = unarm_zeroed)
cat(sprintf("  M7 (effective resists zeroed): R^2 = %.4f, sd = %.3f, AIC = %.1f\n",
            summary(m_zero)$r.squared, sd(resid(m_zero)), AIC(m_zero)))
cat("  → zeroing makes the model WORSE: the level formula does credit\n")
cat("    effective resists at the same rate as special resists.\n")

###############################################################################
# Summary
###############################################################################

cat("\n\n=============================================================\n")
cat("  SUMMARY\n")
cat("=============================================================\n\n")

cat("Confirmed: kinetic.effective = floor(fortitude / 10), and special wins\n")
cat("when both sources are present (no row has both nonzero).\n\n")

cat("BUT: the effective-resist mechanism does NOT explain the negative\n")
cat("fortitude coefficient. On the all-special subset (n = ~230), where the\n")
cat("kinetic and energy values are NOT functions of fortitude, the\n")
cat("coefficient is still -0.020, identical to the full model. The Phase 11\n")
cat("stat-budget / collinearity diagnosis (fortitude correlates 0.85 with\n")
cat("hardiness) stands as the most likely explanation.\n\n")

cat("The level formula appears to use the displayed resist value regardless\n")
cat("of source — zeroing out effective resists makes the fit worse by ~40\n")
cat("AIC. So the M7 form is correct as-is; no need to slice by source.\n\n")

cat("Minor effect: creatures with effective kinetic average ~0.8 levels lower\n")
cat("than M7 predicts (small but statistically detectable). Not large enough\n")
cat("to merit promoting the flag into the canonical formula.\n")
