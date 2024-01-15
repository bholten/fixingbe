source("R/data.R")

library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(lmtest)
library(gbm)
library(cluster)
library(tidyr)
library(mgcv)


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
  mutate(kinen = (kinetic + energy) / 2)
normalized_df <- normalized_df %>%
  mutate(average_res_nonkinen = ( blast + heat + cold + electricity + acid + stun) / 6)
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
  mutate(armor_factor = fortitude * (1 + armor))

##########################
# Clustering experiments #
##########################
clustering_data <- normalized_df %>%
  select(c(average_hdi, armor_factor, average_res, average_dps_attr)) %>%
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

ggplot(normalized_df, aes(x = fortitude, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "fortitude", y = "level") +
  scale_color_discrete(name = "Cluster")


#### GAM

model.gam <- gam(
  level ~ s(average_hdi) +
    s(average_dps_attr) +
    s(average_res) +
    s(fortitude),
  data = normalized_df
)
summary(model.gam)
plot(model.gam, select = 1)
plot(model.gam, select = 2)
plot(model.gam, select = 3)
plot(model.gam, select = 4)
predictions.gam.level <- predict.gam(model.gam, newdata = normalized_df)
predictions.gam.terms <- predict.gam(model.gam, newdata = normalized_df, type = "terms")

max_term_index <- apply(predictions.gam.terms, 1, function(x) which.max(abs(x)))
max_relative_term <- factor(max_term_index)
normalized_df$max_relative_term <- max_relative_term

ggplot(normalized_df, aes(x = predictions.gam.level, y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

#### GBM

model.gbm <- gbm(
  level ~ average_hdi + average_dps_attr + average_res + armor_factor,
  data = normalized_df
)

summary(model.gbm, plotit = FALSE)
plot(model.gbm, i.var = "average_dps_attr")
predictions.gbm.level <- predict.gbm(model.gbm, newdata = normalized_df)

ggplot(normalized_df, aes(x = predictions.gbm.level, y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

####
library(segmented)
plot(model.gam, select = 1)

linear.fit.level <- lm(level ~ average_hdi + average_dps_attr + average_res + fortitude, data = normalized_df)
summary(linear.fit.level)
bptest(linear.fit.level)
segmented.fit.average_hdi <- segmented(linear.fit.level, seg.Z = ~average_hdi, npsi = 2)
summary(segmented.fit.average_hdi)
plot(segmented.fit.average_hdi)
lines.segmented(segmented.fit.average_hdi)
points.segmented(segmented.fit.average_hdi)

plot(model.gam, select = 2)

segmented.fit.average_dps_attr <- segmented(linear.fit.level, seg.Z = ~average_dps_attr, npsi = 1)
summary(segmented.fit.average_dps_attr)
plot(segmented.fit.average_dps_attr)
lines.segmented(segmented.fit.average_dps_attr)
points.segmented(segmented.fit.average_dps_attr)


plot(model.gam, select = 3)

linear.fit.average_res <- lm(level ~ average_hdi + average_dps_attr + average_res + armor_factor, data = normalized_df)
segmented.fit.average_res <- segmented(linear.fit.average_res, seg.Z = ~average_res, npsi = 2)
summary(segmented.fit.average_res)
plot(segmented.fit.average_res)
lines.segmented(segmented.fit.average_res)
points.segmented(segmented.fit.average_res)



plot(model.gam, select = 4)

segmented.fit.armor_factor <- segmented(linear.fit.level, seg.Z = ~armor_factor, npsi = 3)
summary(segmented.fit.armor_factor)
plot(segmented.fit.armor_factor)
lines.segmented(segmented.fit.armor_factor)
points.segmented(segmented.fit.armor_factor)



### Segment analysis
no_armor_df <- normalized_df %>% filter(armor == 0)
armor_df <- normalized_df %>% filter(armor == 1)



# No armor
model.gam.noarmor <- gam(
  level ~ s(average_hdi) +
    s(average_dps_attr) +
    s(average_res) +
    s(fortitude),
  data = no_armor_df
)
summary(model.gam.noarmor)
ggplot(no_armor_df, aes(x = predict(model.gam.noarmor, newdata = no_armor_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")


plot(model.gam.noarmor, select = 1)
plot(model.gam.noarmor, select = 2)
plot(model.gam.noarmor, select = 3)
plot(model.gam.noarmor, select = 4)


linear.fit.level.noarmor <- lm(
  level ~
    average_hdi +
    average_dps_attr +
    average_res +
    fortitude,
  data = no_armor_df
)
summary(linear.fit.level.noarmor)

ggplot(no_armor_df, aes(x = predict(linear.fit.level.noarmor, newdata = no_armor_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")
rs.model.level.noarmor <- resid(linear.fit.level.noarmor)
qqnorm(rs.model.level.noarmor)
qqline(rs.model.level.noarmor)
shapiro.test(rs.model.level.noarmor)
bptest(linear.fit.level.noarmor)


plot(model.gam.noarmor, select = 1)
segmented.fit.average_hdi.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~average_hdi, npsi = 2)
summary(segmented.fit.average_hdi.noarmor)
plot(segmented.fit.average_hdi.noarmor)
lines.segmented(segmented.fit.average_hdi.noarmor)
points.segmented(segmented.fit.average_hdi.noarmor)


plot(model.gam.noarmor, select = 2)

segmented.fit.average_dps_attr.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~average_dps_attr, npsi = 1)
summary(segmented.fit.average_dps_attr.noarmor)
plot(segmented.fit.average_dps_attr.noarmor)
lines.segmented(segmented.fit.average_dps_attr.noarmor)
points.segmented(segmented.fit.average_dps_attr.noarmor)




plot(model.gam.noarmor, select = 3)

segmented.fit.average_res.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~average_res, npsi = 2)
summary(segmented.fit.average_res.noarmor)
plot(segmented.fit.average_res.noarmor)
lines.segmented(segmented.fit.average_res.noarmor)
points.segmented(segmented.fit.average_res.noarmor)




plot(model.gam, select = 4)

segmented.fit.fortitude.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~fortitude, npsi = 1)
summary(segmented.fit.fortitude.noarmor)
plot(segmented.fit.fortitude.noarmor)
lines.segmented(segmented.fit.fortitude.noarmor)
points.segmented(segmented.fit.fortitude.noarmor)

# Armor

model.gam.armor <- gam(
  level ~ s(average_hdi) +
    s(average_dps_attr) +
    s(average_res) +
    s(fortitude),
  data = armor_df
)
summary(model.gam.armor)
plot(model.gam.armor, select = 1)
plot(model.gam.armor, select = 2)
plot(model.gam.armor, select = 3)
plot(model.gam.armor, select = 4)


linear.fit.level.armor <- lm(
  level ~
    average_hdi +
    average_dps_attr +
    average_res +
    fortitude,
  data = armor_df
)
summary(linear.fit.level.armor)

ggplot(armor_df, aes(x = predict(linear.fit.level.armor, newdata = armor_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")
rs.model.level.armor <- resid(linear.fit.level.armor)
qqnorm(rs.model.level.armor)
qqline(rs.model.level.armor)
shapiro.test(rs.model.level.armor)
bptest(linear.fit.level.armor)



plot(model.gam.armor, select = 1)
segmented.fit.average_hdi.armor <- segmented(linear.fit.level.armor, seg.Z = ~average_hdi, npsi = 1)
summary(segmented.fit.average_hdi.armor)
plot(segmented.fit.average_hdi.armor)
lines.segmented(segmented.fit.average_hdi.armor)
points.segmented(segmented.fit.average_hdi.armor)


plot(model.gam.armor, select = 2)

segmented.fit.average_dps_attr.armor <- segmented(linear.fit.level.armor, seg.Z = ~average_dps_attr, npsi = 1)
summary(segmented.fit.average_dps_attr.armor)
plot(segmented.fit.average_dps_attr.armor)
lines.segmented(segmented.fit.average_dps_attr.armor)
points.segmented(segmented.fit.average_dps_attr.armor)




plot(model.gam.armor, select = 3)

segmented.fit.average_res.armor <- segmented(linear.fit.level.armor, seg.Z = ~average_res, npsi = 1)
summary(segmented.fit.average_res.armor)
plot(segmented.fit.average_res.armor)
lines.segmented(segmented.fit.average_res.armor)
points.segmented(segmented.fit.average_res.armor)




plot(model.gam.armor, select = 4)

segmented.fit.armor_factor.armor <- segmented(linear.fit.level.armor, seg.Z = ~armor_factor, npsi = 1)
summary(segmented.fit.armor_factor.armor)
plot(segmented.fit.armor_factor.armor)
lines.segmented(segmented.fit.armor_factor.armor)
points.segmented(segmented.fit.armor_factor.armor)







############ Cluster Analysis
# Why? the armor fit extremely well, and it was all in the same cluster
normalized_df %>%
  filter(cluster == 4) %>%
  dplyr::select(serial, level, average_hdi, average_dps_attr, average_res, armor_factor) %>%
  View()





test_cl_armor <- function(hardiness, fortitude, dexterity, intellect, cleverness, courage, power, kinetic, energy, blast, heat, cold, electricity, acid, stun) {
  ham <- 0.0120 * (hardiness + dexterity + intellect)
  dps <- 0.0126 * (power + cleverness + courage)
  res <- 0.0213 * (kinetic + energy + blast + heat + cold + electricity + acid + stun)
  ar <- 0.0678 * fortitude

  return(-32 + ham + dps + res + ar)
}

test_cl_no_armor <- function(hardiness, fortitude, dexterity, intellect, cleverness, courage, power, kinetic, energy, blast, heat, cold, electricity, acid, stun) {
  avg_ham <- (hardiness + dexterity + intellect) / 3

  ham <- if (avg_ham < 94.584) {
    0.03817 * (hardiness + dexterity + intellect)
  } else if (avg_ham >= 94.584 & avg_ham < 386.844) {
    0.00840333333 * (hardiness + dexterity + intellect)
  } else {
    0.028108 * (hardiness + dexterity + intellect)
  }

  avg_dps <- (power + cleverness + courage) / 3

  dps <- if (avg_dps < 233.667) {
    0.002399 * (power + cleverness + courage)
  } else {
    0.015603 * (power + cleverness + courage)
  }

  avg_res <- (kinetic + energy + blast + heat + cold + electricity + acid + stun) / 8

  res <- if (avg_res <= -25.500) {
    (-0.0083352 / 8) * (kinetic + energy + blast + heat + cold + electricity + acid + stun)
  } else if (avg_res > -25.5 & avg_res < 18.018) {
    (0.2137100 / 8) * (kinetic + energy + blast + heat + cold + electricity + acid + stun)
  } else {
    (0.0680460 / 8) * (kinetic + energy + blast + heat + cold + electricity + acid + stun)
  }

  ar <- if (fortitude < 317) {
    (-0.017705 * fortitude)
  } else {
    (-0.047860 * fortitude)
  }

  return(10 + ham + dps + res + ar)
}

test_cl_no_armor(33, 8, 34, 16, 39, 505, 57, 2, 2, 0, 0, 3, 0, 0, -35)
test_cl_no_armor(611, 442, 352, 105, 50, 519, 137, 58, -64, -2, -35, -34, -99, 40, -99)
test_cl_no_armor(25, 5, 13, 5, 7, 339, 16, 6, 0, 4, 0, -39, -39, -39, -99)

test_cl_armor(33, 8, 34, 16, 39, 505, 57, 2, 2, 0, 0, 3, 0, 0, -35)
test_cl_armor(611, 442, 352, 105, 50, 519, 137, 58, -64, -2, -35, -34, -99, 40, -99)
test_cl_armor(25, 5, 13, 5, 7, 339, 16, 6, 0, 4, 0, -39, -39, -39, -99)
