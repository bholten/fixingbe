library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)

# Some of these are obvious typos -- fix later
bad_data <- c("dta2275t", "r8tfng1v",              # weird hardiness
              "er9nq4et", "38u9raer", "6ko7k4ql",  # weird action/dex
              "6td1segp", "6f9lmdbu",              # bad speed
              "frfksnne",                          # Maybe bad to-hit but interesting...
              "kjvtv7d7"                           # has fortitude < 500 AND light armor?
)


data <- creatures <- read_csv("data/clean/furrycat/creature.csv") %>%
  filter(!creature_id %in% bad_data)
final_exp <- read_csv("data/clean/furrycat/final_experiment.csv")
combined_data <- inner_join(data, final_exp, by = c("final_experimentation_id" = "serial"))

# Mind
model <- lm(mind ~ intellect + hardiness, data = combined_data)
summary(model)
residuals <- resid(model)
residual_variance <- var(residuals)
residual_sd <- sd(residuals)
print(residual_variance)
print(residual_sd)

hist(residuals, breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)

combined_data$predicted_mind <- predict(model, newdata = combined_data)
combined_data$predicted_mind_error <- combined_data$mind - combined_data$predicted_mind

ggplot(combined_data, aes(x = mind, y = predicted_mind)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlab("Actual Mind") +
  ylab("Predicted Mind") +
  ggtitle("Comparison of Actual vs Predicted Mind")

standardized_residuals <- rstandard(model)
# A common cut-off for outliers is 2 or 3 standard deviations from the mean
cutoff <- 1  # or 3 for a stricter definition of outliers
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- combined_data[outlier_indices, ]

View(outlier_records)
