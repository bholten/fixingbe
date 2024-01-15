source("R/data.R")

library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(gbm)
library(mgcv)
library(broom)
library(lmtest)
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(caret)      # ML model building
library(h2o)        # ML model building

h2o.init()
h2o.no_progress()
normalized_df <- normalized_df %>% mutate(damage_spread = damage_high - damage_low)

post_damage_spread_change_df <- normalized_df %>%
  filter((damage_spread > 10 & power >= 380) | power < 380)

d <- post_damage_spread_change_df %>% filter(!serial == "00pas7q5")
d1 <- d %>% select(average_hdi, fortitude, power, courage, cleverness, average_res)
levels <- d %>% pull(level)

model <- train(
  x = d1,
  y = levels,
  trControl = trainControl(method = "cv", number = 5, repeats = 10),
  method = "gbm",
  family = "gaussian"
)


post_damage_spread_change_df$predicted_level <- predict(model, newdata = post_damage_spread_change_df)
post_damage_spread_change_df$residuals <- post_damage_spread_change_df$level - post_damage_spread_change_df$predicted_level
ggplot(post_damage_spread_change_df, aes(x = predicted_level, y = level)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted Level vs Level")


explainer <- lime::lime(x = post_damage_spread_change_df, model = model)

example <- normalized_df %>% filter(serial == "00pas7q5")

explanation <- lime::explain(example, explainer, n_labels = 7, n_features = 4)
plot_features(explanation)

plot(varImp(model, scale = FALSE), top = 7)
