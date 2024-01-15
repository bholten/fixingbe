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

normalized_df %>% select(health, hardiness, dexterity, quality) %>% View()
normalized_df %>% select(health, action, mind, hardiness, dexterity, intellect) %>% View()

# model <- gam(
#   formula = health ~ t2(hardiness, quality) + t2(dexterity, quality),
#   data = normalized_df,
#   family = gaussian()
# )
# summary(model)
# plot(model, selection = 1)
# plot(model, selection = 2)
# augment(model) %>% View()
#
model <- gam(
  formula = health ~ s(hardiness) + s(dexterity),
  data = normalized_df,
  family = gaussian()
)
summary(model)
plot(model, select = 1)
plot(model, select = 2)


normalized_df$predicted_health <- predict.gam(model, normalized_df)
normalized_df$health_resid <- normalized_df$health - normalized_df$predicted_health

model <- lm(
  formula = health ~ hardiness + dexterity,
  data = normalized_df
)
summary(model)
augment(model) %>% View()
res <- resid(model)
qqnorm(res)
qqline(res)
shapiro.test(res)






standardized_residuals <- rstandard(model)
cutoff <- 1
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- normalized_df[outlier_indices, ]
outlier_records %>% select(serial, health, hardiness, dexterity) %>% View()

# Drop outliers
normalized_df <- normalized_df %>% filter(!serial %in% c("64v2qh2v", "78mq8sed", "lnk4d7fn"))
0
model <- lm(
  formula = health ~ hardiness + dexterity,
  data = normalized_df
)
summary(model)
augment(model) %>% View()
res <- resid(model)
hist(res, breaks = 30, main = "Residuals Distribution")
qqnorm(res)
qqline(res)
shapiro.test(res)

library(lmtest)
bptest(model)

normalized_df$predicted_health <- predict.gam(model, normalized_df)
normalized_df$health_resid <- normalized_df$health - normalized_df$predicted_health

ggplot(normalized_df, aes(x = health, y = health_resid)) +
  geom_point()

ggplot(normalized_df, aes(x = health, y = predicted_health, color = health_resid)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")

standardized_residuals <- rstandard(model)
cutoff <- 1
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- normalized_df[outlier_indices, ]
outlier_records %>% select(serial, health, predicted_health, health_resid, hardiness, dexterity) %>% View()

ggplot(normalized_df, aes(x = health, y = predicted_health, color = health_resid)) +
  geom_point()



library(plotly)
plot_ly(
  x = normalized_df$hardiness,
  y = normalized_df$dexterity,
  z = normalized_df$health,
  type = "scatter3d",
  mode = "markers",
  color = normalized_df$health_resid
)


ggplot(normalized_df, aes(x = hardiness, y = health)) +
  geom_point() +
  xlim(0, 50) +
  ylim(0, 1000)


health_v1 <- function(hardiness, dexterity) {
  floor(41.5 + 14.99 * hardiness + 2.99 * dexterity)
}


