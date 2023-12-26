library(magrittr)

# Load Data
library(dplyr)
library(readr)
library(ggplot2)

bad_data <- c("dta2275t", "r8tfng1v", # weird hardiness
              "er9nq4et", "38u9raer", "6ko7k4ql"  # weird action/dex
              )

creatures <- read_csv("data/clean/furrycat/creature.csv") %>%
  filter(!creature_id %in% bad_data)


final_exp <- read_csv("data/clean/furrycat/final_experiment.csv")

combined_data <- inner_join(creatures, final_exp, by = c("final_experimentation_id" = "serial"))

# Hardiness - Health
health_hardiness_correlation <- cor(combined_data$health, combined_data$hardiness, use = "complete.obs")
print(health_hardiness_correlation)

ggplot(combined_data, aes(x = health, y = hardiness)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal() +
  ggtitle("Scatter plot of Health vs Hardiness")


# Dexterity - Action
action_dexterity <- cor(combined_data$action, combined_data$dexterity, use = "complete.obs")
print(action_dexterity)

ggplot(combined_data, aes(x = action, y = dexterity)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal() +
  ggtitle("Scatter plot of Action vs Dexterity")

# Intellect - Mind
mind_intellect <- cor(combined_data$mind, combined_data$intellect, use = "complete.obs")
print(mind_intellect)

ggplot(combined_data, aes(x = mind, y = intellect)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal() +
  ggtitle("Scatter plot of Mind vs Intellect")

linear_model <- lm(combined_data$mind ~ combined_data$intellect, data = combined_data)
residuals <- resid(linear_model)
summary(residuals)
hist(residuals, breaks = 30, main = "Residuals Distribution")
plot(fitted(linear_model), residuals)
abline(h = 0, col = "red")
shapiro.test(residuals)
sd_residuals <- sd(residuals)

library(pls)

# predictors <- c("hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power")
predictors <- c("hardiness", "dexterity")
formula <- as.formula(paste("health ~", paste(predictors, collapse = " + ")))

pls_result <- plsr(formula, data = combined_data, scale = TRUE, validation = "CV")
# Summarize and plot PLSR results
summary(pls_result)
plot(pls_result)
loadings(pls_result)


filtered_data <- combined_data %>%
  filter(abs(hardiness - fortitude) > 200)
cor_health_hardiness <- cor(filtered_data$health, filtered_data$hardiness, use = "complete.obs")
cor_health_fortitude <- cor(filtered_data$health, filtered_data$fortitude, use = "complete.obs")

print(cor_health_hardiness)
print(cor_health_fortitude)

model_filtered <- lm(health ~ hardiness + fortitude, data = filtered_data)
summary(model_filtered)

# CL
predictors <- c("hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power",
                "kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "stun.x")
formula <- as.formula(paste("creature_level ~", paste(predictors, collapse = " + ")))

pls_result <- plsr(formula, data = combined_data, scale = TRUE, validation = "CV")
# Summarize and plot PLSR results
summary(pls_result)
plot(pls_result)
loadings(pls_result)

model_hardiness <- lm(health ~ hardiness, data = combined_data)
model_fortitude <- lm(health ~ fortitude, data = combined_data)
model_dexterity <- lm(health ~ dexterity, data = combined_data)
model_intellect <- lm(health ~ intellect, data = combined_data)

summary(model_hardiness)
summary(model_fortitude)
summary(model_dexterity)
summary(model_intellect)

model_combined <- lm(health ~ hardiness, data = combined_data)
summary(model_combined)

# MODELS
# HEATLH
model <- lm(health ~ hardiness, data = combined_data)
summary(model)

# What is the "random factor"?
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)

## Is Health actually hardiness + dexterity?

model <- lm(health ~ hardiness + dexterity, data = combined_data)
summary(model)

# What is the "random factor"?
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)

get_health <- function(hardiness, dexterity) {
  random_factor <- rnorm(1, mean = 0, sd = 5)

  49 + 15 * hardiness + 3 * dexterity + random_factor
}

## Mind same???

model <- lm(mind ~ intellect + hardiness, data = combined_data)
summary(model)

# What is the "random factor"?
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)

get_mind <- function(hardiness, intellect) {
  40 + 15 * intellect + 3 * hardiness
}



## ACTION WHAT IS GOING ON

model <- lm(action ~ dexterity + intellect, data = combined_data) #!!!!
summary(model)

# What is the "random factor"?
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)


# EDUCATED Guess: 40 + 15d + 3i + random 5 with stddev 1
get_action <- function(dexterity, intellect) {
  48 + 15 * dexterity + 3 * intellect
}


# DAMAGE
model <- lm(damage_high ~ power, data = combined_data) #!!!!
summary(model)

# What is the "random factor"?
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)




# ACTION
model <- lm(action ~ dexterity, data = combined_data)
summary(model)

# What is the "random factor"?
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)

# MIND
model <- lm(mind ~ intellect, data = combined_data)
summary(model)

# What is the "random factor"?
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)



# Running PCA
predictors <- c("hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power",
                "kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "stun.x")
pca_result <- prcomp(combined_data[predictors], scale. = TRUE)

summary(pca_result)
biplot(pca_result)

# Study HAM more
hardy <- combined_data %>% group_by(hardiness)
hard_sum_stat <- hardy %>%
  summarise(mean_health = mean(health, na.rm = TRUE))


### CL part 2
model <- lm(creature_level ~ kinetic.x, data = combined_data)
summary(model)



# Speed -> courage
# Some of these are obvious typos -- fix later
bad_data <- c("dta2275t", "r8tfng1v",              # weird hardiness
              "er9nq4et", "38u9raer", "6ko7k4ql",  # weird action/dex
              "6td1segp", "6f9lmdbu",              # bad speed
              "frfksnne",                          # Maybe bad to-hit but interesting...
              "kjvtv7d7",                          # has fortitude < 500 AND light armor?
)

creatures <- read_csv("data/clean/furrycat/creature.csv") %>%
  filter(!creature_id %in% bad_data)
final_exp <- read_csv("data/clean/furrycat/final_experiment.csv")
combined_data <- inner_join(creatures, final_exp, by = c("final_experimentation_id" = "serial"))

# Speed and courage
model <- lm(speed ~ courage + dependability, data = combined_data)
summary(model)
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

## PLSR analysis
predictors <- c("courage", "dependability")
formula <- as.formula(paste("speed ~", paste(predictors, collapse = " + ")))

pls_result <- plsr(formula, data = combined_data, scale = TRUE, validation = "CV")
# Summarize and plot PLSR results
summary(pls_result)
plot(pls_result)
loadings(pls_result)


ggplot(combined_data, aes(x = courage, y = speed)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal() +
  ggtitle("Scatter plot of Courage vs Speed")



## To-Hit and cleverness + what? -- maybe power?
model <- lm(to_hit ~ cleverness, data = combined_data)
summary(model)
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

ggplot(combined_data, aes(x = cleverness, y = to_hit)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal() +
  ggtitle("Scatter plot of Cleverness vs To-Hit")




############ DUN DUN DUN
## Creature Level
predictors <- c(
  #"hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power",
                "kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "stun.x")

formula <- as.formula(paste("creature_level ~", paste(predictors, collapse = " + ")))
pls_result <- plsr(formula, data = combined_data, scale = TRUE, validation = "CV")
# Summarize and plot PLSR results
summary(pls_result)
plot(pls_result)
loadings(pls_result)

# Let's modify vulns to all be -99 in the final combine

vuln_df <- combined_data %>%
  mutate_at(
    vars("kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "stun.x"),
    ~ ifelse(. < 0, -99, .)
    )


predictors <- c(
  #"hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power",
  "kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "acid.x", "stun.x")


predictors <- c(
  #"hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power",
  "kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "acid.x", "stun.x")

formula <- as.formula(paste("creature_level ~", paste(predictors, collapse = " + ")))
pls_result <- plsr(formula, data = vuln_df, scale = TRUE, validation = "CV")
# Summarize and plot PLSR results
summary(pls_result)
plot(pls_result)
loadings(pls_result)

model <- lm(creature_level ~ kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + stun.x, data = vuln_df)
#model <- lm(creature_level ~ kinetic.x, data = vuln_df)
#model <- lm(creature_level ~ energy.x, data = vuln_df)
#model <- lm(creature_level ~ blast.x, data = vuln_df)
summary(model)
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

### Non-vuln creatures
non_vuln_df <- combined_data %>%
  filter(kinetic.x >= 0, energy.x >= 0, blast.x >= 0, heat.x >= 0, cold.x >= 0, electricity.x >= 0, acid.x >= 0, stun.x >= 0)

# analysis on non-vuln creatures
# model <- lm(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = non_vuln_df)
model <- glm(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = non_vuln_df, family = Gamma())
#model <- glm(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = combined_data, family = Gamma())
#model <- glm(creature_level ~ health + action + mind + speed + to_hit + damage_low + damage_high + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = non_vuln_df, family = Gamma())
#model <- lm(creature_level ~ kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = non_vuln_df)
#model <- lm(creature_level ~ cold.x, data = non_vuln_df)
#model <- lm(creature_level ~ hardiness + dexterity + intellect + power, data = non_vuln_df)
summary(model)
exp(coef(model))
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)
shapiro.test(residuals)
print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)


non_vuln_df$predicted_level <- predict(model, newdata = non_vuln_df, type = "response")
non_vuln_df$predicted_level_error <- non_vuln_df$creature_level - non_vuln_df$predicted_level

model <- glm(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = combined_data, family = Gamma())

vuln_mut_df <- combined_data %>%
  mutate_at(
    vars("kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "acid.x", "stun.x"),
    ~ ifelse(. < 0, -99, .)
  )

vuln_df <- vuln_mut_df %>%
  filter(kinetic.x < 0 | energy.x < 0 | blast.x < 0 | heat.x < 0 | cold.x < 0 | electricity.x < 0 | acid.x < 0 | stun.x < 0)

model <- glm(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = vuln_df, family = Gamma())
summary(model)
exp(coef(model))
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)
shapiro.test(residuals)
print(residual_variance)
print(residual_sd)

vuln_df$predicted_level <- predict(model, newdata = vuln_df, type = "response")
vuln_df$predicted_level_error <- vuln_df$creature_level - vuln_df$predicted_level

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)
standardized_residuals <- rstandard(model)
# A common cut-off for outliers is 2 or 3 standard deviations from the mean
cutoff <- 2  # or 3 for a stricter definition of outliers
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- vuln_df[outlier_indices, ]

View(outlier_records)


library(randomForest)
rf_model <- randomForest(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = non_vuln_df)
summary(rf_model)

importance(rf_model)
varImpPlot(rf_model)

predictions_rf <- predict(rf_model, newdata = non_vuln_df)
non_vuln_df$predicted_level <- predict(rf_model, newdata = non_vuln_df, type = "response")
non_vuln_df$predicted_level_error <- non_vuln_df$creature_level - non_vuln_df$predicted_level

bad_data <- c("dta2275t", "r8tfng1v",              # weird hardiness
              "er9nq4et", "38u9raer", "6ko7k4ql",  # weird action/dex
              "6td1segp", "6f9lmdbu",              # bad speed
              "frfksnne",                          # Maybe bad to-hit but interesting...
              "kjvtv7d7",                          # has fortitude < 500 AND light armor?
              "4231uemm",                          # trash mob in piket skin (min cl 25) that throws off CL calculations
              "at8d24tc",                          # rancor skin (min cl 35) also throwing off cl calculations
              "8mgpmeev",                          # thune skin also throwing off cl
              "v22scdcg"                           # fambaa
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



rf_model <- randomForest(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = normalized_df)
#rf_model <- randomForest(creature_level ~ kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = normalized_df)
#rf_model <- randomForest(creature_level ~ health + action + mind + to_hit + speed + damage_low + damage_high + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = normalized_df)
importance(rf_model)
varImpPlot(rf_model)

predictions_rf <- predict(rf_model, newdata = normalized_df)
mse_rf <- mean((normalized_df$creature_level - predictions_rf)^2)

normalized_df$predicted_creature_level <- predict(rf_model, newdata = normalized_df)
normalized_df$predicted_creature_level_error <- normalized_df$creature_level - normalized_df$predicted_creature_level

residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)
shapiro.test(residuals)
hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)
standardized_residuals <- rstandard(model)
# A common cut-off for outliers is 2 or 3 standard deviations from the mean
cutoff <- 2  # or 3 for a stricter definition of outliers
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- normalized_df[outlier_indices, ]

View(outlier_records)
ggplot(normalized_df, aes(x = creature_level, y = predictions_rf)) +
  geom_point() +
  geom_smooth(method = lm, color = "blue") +
  ggtitle("Random Forest Predictions vs Actual")

















library(gbm)
library(caret)
train_control <- trainControl(method = "cv", number = 10)  # 10-fold CV
gbm_model <- gbm(
  creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x,
  data = normalized_df,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 3,
  n.minobsinnode = 5,
  bag.fraction = 0.5,
  cv.folds = 10
)
gbm_model_final_attributes <- gbm(
  creature_level ~ health + action + mind + speed + to_hit + damage_low + damage_high  + armor.x + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x,
  data = normalized_df,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 3,
  n.minobsinnode = 5,
  bag.fraction = 0.5,
  cv.folds = 10
)

summary(gbm_model)
#summary(gbm_model_final_attributes)

predictions_gbm <- predict(gbm_model, newdata = normalized_df, n.trees = 500)
mse_gbm <- mean((normalized_df$creature_level - predictions_gbm)^2)

ggplot(normalized_df, aes(x = creature_level, y = predictions_gbm)) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  ggtitle("GBM Predictions vs Actual")

normalized_df$predicted_level <- predict(gbm_model, newdata = normalized_df, type = "response")
normalized_df$predicted_level_error <- normalized_df$creature_level - normalized_df$predicted_level


library(gbm)
plot.gbm(gbm_model, i.var = "fortitude", n.trees = 500)

plot.gbm(gbm_model_final_attributes, i.var = "mind", n.trees = 500)


combined_data$predicted_level <- predict(model, newdata = combined_data, type = "response")
combined_data$predicted_level_error <- combined_data$creature_level - combined_data$predicted_level

standardized_residuals <- rstandard(model)
# A common cut-off for outliers is 2 or 3 standard deviations from the mean
cutoff <- 1  # or 3 for a stricter definition of outliers
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- non_vuln_df[outlier_indices, ]

View(outlier_records)



ggplot(non_vuln_df, aes(x = creature_level, y = predicted_level)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("Actual Creature Level") +
  ylab("Predicted Creature Level") +
  ggtitle("Comparison of Actual vs Predicted Creature Level")


ggplot(combined_data, aes(x = creature_level, y = predicted_level)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("Actual Creature Level") +
  ylab("Predicted Creature Level") +
  ggtitle("Comparison of Actual vs Predicted Creature Level")


# Now let's see how wrong it is against the full data
combined_data$predicted_level <- predict(model, newdata = combined_data)

ggplot(combined_data, aes(x = creature_level, y = predicted_level)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("Actual Creature Level") +
  ylab("Predicted Creature Level") +
  ggtitle("Comparison of Actual vs Predicted Creature Level")



###
library(nnls)
predictors <- c(
  "hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power",
  "kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "acid.x", "stun.x")


#model_nnls <- nnls(as.matrix(non_vuln_df[predictors]), non_vuln_df$creature_level)
#non_vuln_df$predicted_level <- c(nnls::predict(model_nnls, as.matrix(non_vuln_df[predictors])))



pls_result <- plsr(creature_level ~ hardiness + fortitude + dexterity + endurance + intellect + cleverness + courage + dependability + fierceness + power + kinetic.x + energy.x + blast.x + heat.x + cold.x +  electricity.x + acid.x + stun.x, data = non_vuln_df, scale = TRUE, validation = "CV")
summary(pls_result)
plot(pls_result)
loadings(pls_result)




# Weird one -- armor and fortitude?
# I've seen some > 500 fortitude pets not get armor in Furrycat's samples
model <- lm(armor.x ~ fortitude, data = non_vuln_df)
summary(model)
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)

print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

ggplot(combined_data, aes(x = fortitude, y = armor.x)) +
  geom_point() +
  theme_minimal() +
  xlab("Fortitude") +
  ylab("Armor Level") +
  ggtitle("Comparison of Fortitude vs Armor")

# ... there's one I don't trust
degenerate_records <- combined_data %>%
  filter(armor.x == 1, fortitude < 500)


## Armor probably isn't linear

model <- glm(creature_level ~ armor.x, data = combined_data, family = gaussian())
summary(model)

effect_size <- coef(model)["armor.x"]

ggplot(combined_data, aes(x = as.factor(armor.x), y = creature_level)) +
  geom_boxplot() +
  labs(x = "Armor", y = "Creature Level") +
  theme_minimal()

data_near_500_cutoff <- combined_data %>%
  filter(fortitude > (500 - 100), fortitude < (500 + 100))

model <- glm(creature_level ~ armor.x, data = data_near_500_cutoff, family = gaussian())
summary(model)
effect_size <- coef(model)["armor.x"]
ggplot(data_near_500_cutoff, aes(x = as.factor(armor.x), y = creature_level)) +
  geom_boxplot() +
  labs(x = "Armor", y = "Creature Level") +
  theme_minimal()

