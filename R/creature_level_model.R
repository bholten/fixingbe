source("R/data.R")
library(stats)

# Canonical prediction functions (creature_level_no_armor_lm, creature_level_armor,
# custom_model) live in creature_level_formulas.R so the Core3 comparison harness
# shares exactly the same coefficients.
source("R/creature_level_formulas.R")


creature_level_no_armor <- function(data) {
  hardiness <- data$hardiness
  dexterity <- data$dexterity
  intellect <- data$intellect

  avg_ham <- (hardiness + dexterity + intellect) / 3

  ham <-
    ifelse(
      avg_ham <= 91.000,
      3.5691 + 0.098002 * avg_ham,
      ifelse(
        avg_ham <= 401.395,
        10.5240 + 0.021577 * avg_ham,
        -6.3097 + 0.063514 * avg_ham
      )
    )

  fort <- -0.020018 * data$fortitude
  cle <- 0.025625 * data$cleverness
  pow <- 0.013442 * data$power

  kinen <- (data$kinetic + data$energy) / 2

  k <-
    ifelse(
      kinen <= 0,
      -6.3682,
      6.0448 + 0.178020 * kinen
    )

  nonkinen <- (data$blast + data$cold + data$heat + data$electricity + data$acid + data$stun) / 6

  nk <-
    ifelse(
      nonkinen <= 0,
      -5.3629,
      7.2562 + 0.0894400 * nonkinen
    )


  lvl <- -2.727275 + ham + fort + cle + pow + k + nk

  return(
    ifelse(
      lvl <= 2,
      2,
      lvl
    )
  )
}


predict_function <- function(intercept, df) {
  return(intercept + creature_level_no_armor(df))
}

ssr_function <- function(intercept, df) {
  predicted_values <- predict_function(intercept, df)
  residuals <- df$level - predicted_values
  return(sum(residuals^2))
}

initial_intercept <- 0  # Starting value for the optimization
optim_result <- optim(initial_intercept,
                      ssr_function,
                      method = "Brent",
                      lower = -25,
                      upper = 25,
                      df = (normalized_df %>% filter(armor == 0)))
best_intercept <- optim_result$par





no_armor_df$predicted_level <- custom_model(no_armor_df)
no_armor_df$residuals <- no_armor_df$predicted_level - no_armor_df$level
mean(no_armor_df$residuals)
median(no_armor_df$residuals)
var(no_armor_df$residuals)
IQR(no_armor_df$residuals)
min(no_armor_df$residuals)
max(no_armor_df$residuals)
qqnorm(no_armor_df$residuals)
qqline(no_armor_df$residuals)
shapiro.test(no_armor_df$residuals)


normalized_df$predicted_level <- custom_model(normalized_df)
normalized_df$residuals <- normalized_df$level -normalized_df$predicted_level
mean(normalized_df$residuals)
median(normalized_df$residuals)
var(normalized_df$residuals)
IQR(normalized_df$residuals)
min(normalized_df$residuals)
max(normalized_df$residuals)
qqnorm(normalized_df$residuals)
qqline(normalized_df$residuals)
shapiro.test(normalized_df$residuals)


ggplot(normalized_df, aes(x = predicted_level, y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")

