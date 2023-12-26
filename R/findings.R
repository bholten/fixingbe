

health <- function(hardiness, dexterity) {
  random_factor <- rnorm(1, mean = 0, sd = 5)

  40 + 15 * hardiness + 3 * dexterity + random_factor
}

action <- function(dexterity, intellect) {
  random_factor <- rnorm(1, mean = 0, sd = 5)

  40 + 15 * dexterity + 3 * intellect + random_factor
}

mind <- function(intellect, hardiness) {
  random_factor <- rnorm(1, mean = 0, sd = 5)

  40 + 15 * intellect + 3 * hardiness + random_factor
}


to_hit <- function(cleverness) {
  # 1.951e-01 + 6.459e-04 * cleverness
  0.2 + 0.00065 * cleverness # but was rounding
}
