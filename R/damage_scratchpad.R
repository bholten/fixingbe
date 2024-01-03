source("R/data.R")

library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(gbm)
library(mgcv)
library(broom)
library(cluster)
library(tidyr)


#############
normalized_df <- normalized_df %>% mutate(damage_spread = damage_high - damage_low)

clustering_data <- normalized_df %>%
  select(c(damage_spread)) %>%
  select(where(is.numeric)) %>%
  mutate_all(replace_na, 0)

kmeans_result <- kmeans(clustering_data, centers = 3)
normalized_df$cluster <- as.factor(kmeans_result$cluster)
#############

ggplot(normalized_df, aes(x = power, y = damage_high, color = (damage_spread > 10))) +
  geom_point() +
  ggtitle("power vs damage_high")

ggplot(normalized_df, aes(x = power, y = damage_low, color = (damage_spread > 10))) +
  geom_point() +
  ggtitle("power vs damage_low")


long_df <- pivot_longer(
  normalized_df,
  cols = c(damage_low, damage_high),
  names_to = "damage_type",
  values_to = "damage_value"
)

ggplot(long_df, aes(x = power, y = damage_value, color = damage_type)) +
  geom_point() +
  scale_color_manual(values = c("damage_low" = "blue", "damage_high" = "red")) +
  ggtitle("Power vs. Damage")

ggplot(normalized_df, aes(x = damage_low, y = damage_high, color = (damage_spread > 10))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 10, color = "red", linetype = "dashed") +
  ggtitle("damage_low vs damage_high")

ggplot(normalized_df, aes(x = damage_low, y = damage_high, color = (damage_spread > 10))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 10, color = "red", linetype = "dashed") +
  ggtitle("damage_low vs damage_high")

ggplot(normalized_df, aes(x = damage_low, y = damage_high, color = (damage_low >= 300) & (damage_spread > 10))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 10, color = "red", linetype = "dashed") +
  ggtitle("damage_low vs damage_high")

normalized_df %>% filter(damage_spread < 10) %>% select(serial, level, damage_low, damage_high, power) %>% View()
normalized_df %>% filter(power < 80) %>% select(serial, level, damage_low, damage_high, power) %>% View()


##################
gbm_model <- gbm(
  formula = damage_low ~
    hardiness + fortitude + dexterity + endurance + intellect +
    dependability +
    cleverness + courage + power +
    fierceness,
  data = normalized_df,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 6,
  n.minobsinnode = 5,
  bag.fraction = 0.5,
  cv.folds = 10#,
  #shrinkage = 0.01
)

summary(gbm_model, plotit = FALSE)
plot.gbm(gbm_model, i.var = "power")
plot.gbm(gbm_model, i.var = "fierceness")

model <- gam(
  formula = damage_low ~
    s(hardiness) + s(fortitude) + s(dexterity) + s(endurance) + s(intellect) +
    s(dependability) +
    s(cleverness) + s(courage) + s(power) +
    s(fierceness),
  family = gaussian(),
  data = normalized_df
)

plot(model, select = 10)

normalized_df$predicted_damage_low <- predict(gbm_model, newdata = normalized_df, type = "response", trees = 500)
normalized_df$residuals <- normalized_df$damage_low - normalized_df$predicted_damage_low

ggplot(normalized_df, aes(x = damage_low, y = predicted_damage_low, color = (fierceness > 400))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted Level vs Level")

### Segmentation

low_power <- normalized_df %>% filter(power < 380)
high_power <- normalized_df %>% filter(power >= 380)

model <- lm(formula = damage_low ~ power, data = low_power)
summary(model)

model <- lm(formula = damage_high ~ power, data = low_power)
summary(model)

normalized_df %>% filter(damage_spread > 10) %>% View()
normalized_df %>% select(power, damage_low, damage_high) %>% View()

ggplot(low_power, aes(x = damage_low, y = damage_high, color = (power >= 380) & (damage_spread > 10))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 10, color = "red", linetype = "dashed") +
  ggtitle("damage_low vs damage_high")

ggplot(high_power, aes(x = damage_low, y = damage_high, color = (power >= 380) & (damage_spread > 10))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 10, color = "red", linetype = "dashed") +
  ggtitle("damage_low vs damage_high")


fit_data <- normalized_df %>% filter(power > 380, damage_spread > 10)

model <- lm(formula = damage_low ~ power, data = fit_data)
summary(model)

model <- lm(formula = damage_high ~ power, data = fit_data)
summary(model)


############## CLEANING THE DATASET

# approximate bad data
post_damage_spread_change_df <- normalized_df %>%
  filter((damage_spread > 10 & power >= 380) | power < 380)


long_df <- pivot_longer(
  post_damage_spread_change_df,
  cols = c(damage_low, damage_high),
  names_to = "damage_type",
  values_to = "damage_value"
)
ggplot(long_df, aes(x = power, y = damage_value, color = damage_type)) +
  geom_point() +
  scale_color_manual(values = c("damage_low" = "blue", "damage_high" = "red")) +
  ggtitle("Power vs. Damage")

post_damage_spread_change_df %>% filter(power >= 380, damage_spread > 10) %>% View()
post_damage_spread_change_df %>% filter(power < 380) %>% View()
### DO IT
damage_high_lowp_model <- lm(formula = damage_high ~ damage_low, data = post_damage_spread_change_df %>% filter(power < 380))
summary(damage_high_lowp_model)

damage_high_lowp_model <- lm(formula = damage_high ~ power, data = post_damage_spread_change_df %>% filter(power < 380))
summary(damage_high_lowp_model)

damage_high_highp_model <- lm(formula = damage_high ~ power, data = post_damage_spread_change_df %>% filter(power >= 380, damage_spread > 10))
summary(damage_high_highp_model)

damage_high_highp_model <- lm(formula = damage_high ~ damage_low, data = post_damage_spread_change_df %>% filter(power >= 380, damage_spread > 10))
summary(damage_high_highp_model)


damage_low_lowp_model <- lm(formula = damage_low ~ power, data = post_damage_spread_change_df %>% filter(power < 380))
summary(damage_low_lowp_model)

damage_low_highp_model <- lm(formula = damage_low ~ power, data = post_damage_spread_change_df %>% filter(power >= 380, damage_spread > 10))
summary(damage_low_highp_model)

hakry_dh <- function(power) {
  (0.8 * power) * (1 + ((power / 5000)))
  0.8 * power + 0.00016 * power^2
  power * (0.8 + 0.00016 * power)
}

damage_high_calc_v1 <- function(data) {
  return(ifelse(data$power < 380,
         23 + 0.776 * data$power,
         1.06 * data$power - 82.5))
}

damage_high_calc_v2 <- function(data) {
  return(ifelse(data$damage_low <= 300,
                23 + 0.776 * data$power,
                2 * data$damage_low - 290))
}

damage_low_calc_v1 <- function(data) {
  return(ifelse(data$power < 380,
         13 + 0.776 * data$power,
         1 + 0.5 * data$power))
}

## Make prediction
post_damage_spread_change_df <- post_damage_spread_change_df %>%
  mutate(predicted_damage_low = damage_low_calc_v1(post_damage_spread_change_df))
post_damage_spread_change_df <- post_damage_spread_change_df %>%
  mutate(predicted_damage_low_resid = damage_low - predicted_damage_low)
mse <- mean(post_damage_spread_change_df$predicted_damage_low_resid^2)
sqrt(mse)
mean(abs(post_damage_spread_change_df$predicted_damage_low_resid))


ggplot(post_damage_spread_change_df, aes(x = predicted_damage_low, y = predicted_damage_low_resid, color = cluster)) +
  geom_point() +
  ggtitle("predicted_damage_low vs residual")



predictions_high_d <- damage_high_calc_v1(post_damage_spread_change_df)
post_damage_spread_change_df <- post_damage_spread_change_df %>%
  mutate(predicted_damage_high = damage_high_calc_v1(post_damage_spread_change_df))
post_damage_spread_change_df <- post_damage_spread_change_df %>%
  mutate(predicted_damage_high_resid = damage_low - predicted_damage_high)
mse <- mean(post_damage_spread_change_df$predicted_damage_high_resid^2)
sqrt(mse)
mean(abs(post_damage_spread_change_df$predicted_damage_high_resid))


ggplot(post_damage_spread_change_df, aes(x = predicted_damage_high, y = predicted_damage_high_resid, color = cluster)) +
  geom_point() +
  ggtitle("predicted_damage_high vs residual")














normalized_df %>% select(damage_high, predicted_dh, resid_dh) %>% View()

gbm_model <- gbm(
  formula = damage_low ~ power + fierceness,
  data = low_power,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 6,
  n.minobsinnode = 5,
  bag.fraction = 0.5,
  cv.folds = 10#,
  #shrinkage = 0.01
)

summary(gbm_model, plotit = FALSE)
plot.gbm(gbm_model, i.var = "power")
plot.gbm(gbm_model, i.var = "fierceness")

model <- gam(
  formula = damage_low ~ s(power) + s(fierceness),
  family = gaussian(),
  data = low_power
)
plot(model, select = 2)
gbm_model <- gbm(
  formula = damage_low ~ power + fierceness + quality,
  data = high_power,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 6,
  n.minobsinnode = 5,
  bag.fraction = 0.5,
  cv.folds = 10#,
  #shrinkage = 0.01
)

summary(gbm_model, plotit = FALSE)
plot.gbm(gbm_model, i.var = "power")
plot.gbm(gbm_model, i.var = "fierceness")
plot.gbm(gbm_model, i.var = "quality")


model <- gam(
  formula = damage_low ~ s(power) + s(fierceness) + quality,
  family = gaussian(),
  data = high_power
)
plot(model, select = 1)


model <- lm(damage_low ~ power * fierceness * quality, data = low_power)
summary(model)
rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(model) %>% View()

model <- lm(damage_low ~ power * fierceness * quality, data = high_power)
summary(model)
rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(model) %>% View()



model <- lm(damage_high ~ power, data = low_power)
summary(model)
rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(model) %>% View()

model <- lm(damage_high ~ power, data = high_power)
summary(model)
rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(model) %>% View()



### Feature engineer it

normalized_df <- normalized_df %>% mutate(pf = (power + fierceness) / 2)
low_power <- low_power %>% mutate(pf = (power + fierceness) / 2)
high_power <- high_power %>% mutate(pf = (power + fierceness) / 2)

model <- gam(
  formula = damage_low ~ s(pf),
  family = gaussian(),
  data = normalized_df
)
plot(model, select = 1)
model <- gam(
  formula = damage_high ~ s(pf),
  family = gaussian(),
  data = normalized_df
)
plot(model, select = 1)


## I am highly suspicious fierceness plays a role
model <- lm(damage_low ~ power * fierceness, data = normalized_df)
summary(model)
rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(model) %>% View()


model <- lm(damage_low ~ power, data = normalized_df)
summary(model)
rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(model) %>% View()

model <- lm(damage_low ~ pf, data = normalized_df)
summary(model)
rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(model) %>% View()


library(changepoint)
result <- cpt.mean(normalized_df$damage_low, method = "PELT")
cpts <- cpts(result)

library(segmented)
df_2 <- post_damage_spread_change_df %>% filter(serial != "4rt3st55")
model <- lm(damage_low ~ power, data = df_2)
summary(model)


segmented.model <- segmented(model, seg.Z = ~power, psi=380)
summary(segmented.model)
df_2$predicted_damage_low <-segmented.model$fitted.values
df_2$predicted_damage_low_residuals <- df_2$damage_low - df_2$predicted_damage_low

#plot original data
ggplot(df_2, aes(x = power, y = damage_low)) +
  geom_point() +
  geom_line(aes(x = power, y = predicted_damage_low), color = 'red', linetype = 'dotted')

rs <- resid(segmented.model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
augment(segmented.model) %>% View()

library(tidyr)
post_damage_spread_change_df %>% filter(damage_spread < 10) %>% View()

min_damage_v1 <- function(p) {
  if (p <= 368.222) {
    return(12.917708 + 0.778108 * p)
  } else {
    return(292.6084 + 0.528404 * p)
  }
}


min_damage_v2 <- function(p) {
  damage <- ifelse(p <= 370, 13 + (7/9) * p, 290 + (1/2) * p)
  return(round(max(damage, 30) / 5) * 5)
}

library(purrr)

post_damage_spread_change_df$damage_low_v2_pred <- post_damage_spread_change_df$power %>%
  map(min_damage_v2)

post_damage_spread_change_df %>%
  select(serial, skin, level, damage_low, damage_low_v2_pred, power) %>%
  View()







