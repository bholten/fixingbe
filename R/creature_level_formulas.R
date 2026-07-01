###############################################################################
# creature_level_formulas.R - Canonical creature-level prediction functions
#
# Pure functions only (no data loading, no side effects). This is the single
# source of truth for the reverse-engineered ("simple model") level formulas.
# Both R/creature_level_model.R and R/core3_comparison.R source this file so
# the M8 / armored coefficients can never drift between the analysis and the
# Core3 head-to-head comparison.
#
# Expects a data frame with columns: hardiness, fortitude, dexterity,
# intellect, cleverness, power, kinetic, energy, blast, heat, cold,
# electricity, acid, stun.
###############################################################################

# Unarmored branch (M8): resist floor on the weaker of kinetic/energy plus a
# cleverness hinge above 400 for apex-DPS pets.
creature_level_no_armor_lm <- function(data) {
  ke_floor  <- pmax(pmin(data$kinetic, data$energy), 0)
  nonkinen  <- (data$blast + data$heat + data$cold +
                data$electricity + data$acid + data$stun) / 6
  clev_h400 <- pmax(data$cleverness - 400, 0)
  return(
    8.132249 +
      0.012301 * data$hardiness +
     -0.019403 * data$fortitude +
      0.004439 * data$dexterity +
      0.011387 * data$intellect +
      0.019508 * data$cleverness +
      0.015615 * data$power +
      0.169649 * ke_floor +
      0.050378 * nonkinen +
      0.105771 * clev_h400
  )
}

# Armored branch: clean linear fit.
creature_level_armor <- function(data) {
  return(
    -21.331842 +
      (0.027648 / 3) * data$hardiness +
      (0.027648 / 3) * data$dexterity +
      (0.027648 / 3) * data$intellect +
      0.056252 * data$fortitude +
      0.024034 * data$cleverness +
      0.015740 * data$power +
      (0.096920 / 2) * data$kinetic +
      (0.096920 / 2) * data$energy +
      (0.085904 / 6) * data$blast +
      (0.085904 / 6) * data$cold +
      (0.085904 / 6) * data$heat +
      (0.085904 / 6) * data$electricity +
      (0.085904 / 6) * data$acid +
      (0.085904 / 6) * data$stun
  )
}

# Hard switch at fortitude = 500 (the armored/unarmored split).
custom_model <- function(data) {
  ifelse(
    data$fortitude < 500,
    creature_level_no_armor_lm(data),
    creature_level_armor(data)
  )
}
