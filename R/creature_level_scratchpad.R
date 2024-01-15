source("R/data.R")

library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(lmtest)
library(lime)
library(gbm)
library(caret)
library(cluster)
library(tidyr)

phy_sample <-samples %>% rename_with(~ paste0(., ".physique"), -sample_id)
pro_sample <-samples %>% rename_with(~ paste0(., ".prowess"), -sample_id)
men_sample <-samples %>% rename_with(~ paste0(., ".mental"), -sample_id)
psy_sample <-samples %>% rename_with(~ paste0(., ".psychology"), -sample_id)
agg_sample <-samples %>% rename_with(~ paste0(., ".aggression"), -sample_id)

normalized_df <- normalized_df %>%
  left_join(phy_sample, c("physique" = "sample_id")) %>%
  left_join(pro_sample, c("prowess" = "sample_id")) %>%
  left_join(men_sample, c("mental" = "sample_id")) %>%
  left_join(psy_sample, c("psychology" = "sample_id")) %>%
  left_join(agg_sample, c("aggression" = "sample_id"))

add_vuln_column <- function(data, resist) {
  resist_special <- sym(paste0(resist, ".special.x"))
  resist_special_physique <- sym(paste0(resist, ".special.physique"))
  resist_special_prowess <- sym(paste0(resist, ".special.prowess"))
  resist_special_mental <- sym(paste0(resist, ".special.mental"))
  resist_special_psychology <- sym(paste0(resist, ".special.psychology"))
  resist_special_aggression <- sym(paste0(resist, ".special.aggression"))

  resist_effective_physique <- sym(paste0(resist, ".effective.physique"))
  resist_effective_prowess <- sym(paste0(resist, ".effective.prowess"))
  resist_effective_mental <- sym(paste0(resist, ".effective.mental"))
  resist_effective_psychology <- sym(paste0(resist, ".effective.psychology"))
  resist_effective_aggression <- sym(paste0(resist, ".effective.aggression"))

  vuln_column_name <- sym(paste0(resist, ".vuln"))

  data %>%
    mutate(!!vuln_column_name := ifelse(
      .data[[resist_special]] < 0 &
        ((.data[[resist_special_physique]]     < 0 |
          .data[[resist_special_prowess]]      < 0 |
          .data[[resist_special_mental]]       < 0 |
          .data[[resist_special_psychology]]   < 0 |
          .data[[resist_special_aggression]]   < 0) &
         (.data[[resist_special_physique]]     > 0 |
          .data[[resist_special_prowess]]      > 0 |
          .data[[resist_special_mental]]       > 0 |
          .data[[resist_special_psychology]]   > 0 |
          .data[[resist_special_aggression]]   > 0 |
          .data[[resist_effective_physique]]   > 0 |
          .data[[resist_effective_prowess]]    > 0 |
          .data[[resist_effective_mental]]     > 0 |
          .data[[resist_effective_psychology]] > 0 |
          .data[[resist_effective_aggression]] > 0)),
      1, 0))
}

# Find which resists were flipped vuln
normalized_df <- normalized_df %>%
  add_vuln_column("kinetic") %>%
  add_vuln_column("energy") %>%
  add_vuln_column("blast") %>%
  add_vuln_column("heat") %>%
  add_vuln_column("cold") %>%
  add_vuln_column("electricity") %>%
  add_vuln_column("acid") %>%
  add_vuln_column("stun")

normalized_df <-
  normalized_df %>%
  mutate(total_vulns = kinetic.vuln + energy.vuln +blast.vuln + heat.vuln + cold.vuln + electricity.vuln + acid.vuln + stun.vuln)



#### ~ FEATURE ~ ENGINEERING ~~~~~
normalized_df <- normalized_df %>%
  mutate(average_ham = (health + action + mind) / 3)
normalized_df <- normalized_df %>%
  mutate(average_res = (kinetic + energy + blast + heat + cold + electricity + acid + stun) / 8)
normalized_df <- normalized_df %>%
  mutate(average_dps = ((damage_high + damage_low) / 2) * speed * to_hit)
normalized_df <- normalized_df %>%
  mutate(average_others = (blast + heat + cold + electricity + acid + stun) / 6)
normalized_df <- normalized_df %>%
  mutate(average_hdi = (hardiness + dexterity + intellect) / 3)
normalized_df <- normalized_df %>%
  mutate(average_sec = (fortitude + endurance + dependability) / 3)
normalized_df <- normalized_df %>%
  mutate(average_dps_attr = (power + courage + cleverness) / 3)
normalized_df <- normalized_df %>%
  mutate(armor_factor = fortitude ** (1 + armor))

##########################
# Clustering experiments #
##########################
clustering_data <- normalized_df %>%
  select(c(average_hdi, armor, average_res, average_dps_attr, quality, average_sec)) %>%
  select(where(is.numeric)) %>%
  mutate_all(replace_na, 0)

kmeans_result <- kmeans(clustering_data, centers = 5)
normalized_df$cluster <- as.factor(kmeans_result$cluster)

ggplot(normalized_df, aes(x = hardiness, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "hardiness", y = "level") +
  scale_color_discrete(name = "Cluster")

### Some plots
ggplot(normalized_df, aes(x = average_ham, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_ham", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = average_dps_attr, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_dps_attr", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = average_res, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_res", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = armor_factor, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "armor_factor", y = "level") +
  scale_color_discrete(name = "Cluster")


parameters <- normalized_df %>%
  select(
    average_hdi,
    average_res,
    average_dps_attr,
    average_sec,
    armor
)
levels <- normalized_df %>% pull(level)
svmControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

set.seed(825)
grid <- expand.grid(interaction.depth=c(1, 2, 3),
                    n.trees=c(10, 20),
                    shrinkage=c(0.001, 0.01, 0.1),
                    n.minobsinnode = 20)


model <- train(
  level ~ average_hdi + average_res + average_dps_attr + average_sec + armor,
  data = normalized_df,
  trControl = svmControl,
  tuneGrid = grid,
  method = "gbm",
  verbose = TRUE
)

plot(model)
plot(varImp(model))
summary(model)


featurePlot(x = parameters,
            y = normalized_df$level,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 2))

explainer <- lime::lime(x = normalized_df, model = model)
example <- normalized_df %>% filter(serial == "00pas7q5")
explanation <- lime::explain(example, explainer, n_labels = 5, n_features = 4)

plot_features(explanation)

normalized_df$predicted_level <- predict(model, newdata = normalized_df)
normalized_df$residuals <- normalized_df$level - normalized_df$predicted_level

ggplot(normalized_df, aes(x = level, y = predicted_level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted Level vs Level")

#### PREVIEW #####
normalized_df %>%
  select(serial, level, predicted_level, armor, average_hdi, average_res, average_dps_attr, average_sec) %>%
  View()

