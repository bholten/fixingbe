library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(gbm)

# Some of these are obvious typos -- fix later
bad_data <- c("dta2275t", "r8tfng1v",              # weird hardiness
              "er9nq4et", "38u9raer", "6ko7k4ql",  # weird action/dex
              "6td1segp", "6f9lmdbu",              # bad speed
              "frfksnne",                          # Maybe bad to-hit but interesting...
              "kjvtv7d7",                          # has fortitude < 500 AND light armor?
              "4231uemm",                          # trash mob in piket skin (min cl 25) that throws off CL calculations
              "at8d24tc",                          # rancor skin (min cl 35) also throwing off cl calculations
              "8mgpmeev",                          # thune skin also throwing off cl
              "v22scdcg",                          # fambaa
              "ceh4m90v",                          # kimogila
              "mm8gn9sj"                           # kimogila
)

creatures <- read_csv("data/clean/furrycat/creature.csv") %>%
  filter(!creature_id %in% bad_data)
final_exp <- read_csv("data/clean/furrycat/final_experiment.csv")
combined_data <- inner_join(creatures, final_exp, by = c("final_experimentation_id" = "serial"))
normalized_df <- combined_data %>%
  mutate_at(
    vars("kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "acid.x", "stun.x"),
    ~ ifelse(. < 0, -99, .)
  ) %>%
  mutate_at(
    vars("kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "acid.x", "stun.x"),
    ~ ifelse(is.na(.), 0, .)
  )

gbm_model <- gbm(
  action ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x,
  data = normalized_df,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 3,
  n.minobsinnode = 5,
  bag.fraction = 0.5,
  cv.folds = 10
)
summary(gbm_model)
plot.gbm(gbm_model, i.var = "dexterity", n.trees = 500)
plot.gbm(gbm_model, i.var = "intellect", n.trees = 500)


# Action
model <- lm(action ~ dexterity + intellect, data = combined_data)
summary(model)
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)
print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

normalized_df$predicted_action <- predict(model, newdata = normalized_df)
normalized_df$predicted_action_error <- normalized_df$action - normalized_df$predicted_action
normalized_df$predicted_action_gbm <- predict(gbm_model, newdata = normalized_df)
normalized_df$predicted_action_gbm_error <- normalized_df$action - normalized_df$predicted_action_gbm

ggplot(normalized_df, aes(x = action, y = predicted_action)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("Actual Action") +
  ylab("Predicted Action") +
  ggtitle("Comparison of Actual vs Predicted Action")

ggplot(normalized_df, aes(x = action, y = predicted_action_gbm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("Actual Action") +
  ylab("Predicted Action") +
  ggtitle("Comparison of Actual vs Predicted Action")



standardized_residuals <- rstandard(model)
# A common cut-off for outliers is 2 or 3 standard deviations from the mean
cutoff <- 1  # or 3 for a stricter definition of outliers
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- normalized_df[outlier_indices, ]

View(outlier_records)
