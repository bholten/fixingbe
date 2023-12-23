library(magrittr)

# Load Data
library(dplyr)
library(readr)
library(ggplot2)

creatures <- read_csv("data/clean/furrycat/creature.csv")
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

predictors <- c("hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power")
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



# Running PCA
predictors <- c("hardiness", "fortitude", "dexterity", "endurance", "intellect", "cleverness", "dependability", "courage", "fierceness", "power",
                "kinetic.x", "energy.x", "blast.x", "heat.x", "cold.x", "electricity.x", "stun.x")
pca_result <- prcomp(combined_data[predictors], scale. = TRUE)

summary(pca_result)
biplot(pca_result)

