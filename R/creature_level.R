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
  mutate(nonkinen = ( blast + heat + cold + electricity + acid + stun) / 6)
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
normalized_df <- normalized_df %>%
  mutate(coldac = (cold + acid) / 2)
normalized_df <- normalized_df %>%
  mutate(bhes = (blast + heat + electricity + stun) / 4)
normalized_df <- normalized_df %>%
  mutate(kinen.special = (kinetic.special + energy.special) / 2)
normalized_df <- normalized_df %>%
  mutate(kinen.effective = (kinetic.effective + energy.effective) / 2)
normalized_df <- normalized_df %>%
  mutate(nonkinen.special = (blast.special + cold.special + heat.special + electricity.special + acid.special + stun.special) / 6)
normalized_df <- normalized_df %>%
  mutate(nonkinen.effective = (blast.effective + cold.effective + heat.effective + electricity.effective + acid.effective + stun.effective) / 6)
normalized_df <- normalized_df %>%
  mutate(clepow = (cleverness + power) / 2)
normalized_df <- normalized_df %>%
  mutate(dps = ((damage_high + damage_low) / 2) * to_hit * speed)


##########################
# Clustering experiments #
##########################
# s(hardiness) +
#   s(fortitude) +
#   s(dexterity) +
#   s(intellect) +
#   s(cleverness) +
#   s(power) +
#   s(kinetic) +
#   s(energy) +
#   s(cold)
clustering_data <- normalized_df %>%
  dplyr::select(c(average_hdi, fortitude, cleverness, power, kinen, nonkinen)) %>%
  dplyr::select(where(is.numeric)) %>%
  mutate_all(replace_na, 0)

kmeans_result <- kmeans(clustering_data, centers = 3)
normalized_df$cluster <- as.factor(kmeans_result$cluster)

print(kmeans_result$centers)

# normalized_df %>%
#   dplyr::select(c(serial, skin, level, cluster, hardiness, fortitude, dexterity, intellect, cleverness, power, kinetic, energy, cold)) %>%
#   View()



ggplot(normalized_df, aes(x = average_hdi, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_hdi", y = "level") +
  scale_color_discrete(name = "Cluster")

### Some plots
ggplot(normalized_df, aes(x = fortitude, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "fortitude", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = cleverness, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "cleverness", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = power, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "power", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = kinen, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "kinen", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = nonkinen, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "nonkinen", y = "level") +
  scale_color_discrete(name = "Cluster")


# What even has influence?

model.gam.everything <- gam(
  level ~
    s(hardiness) +
    s(fortitude) +
    s(dexterity) +
    s(endurance) +
    s(intellect) +
    s(cleverness) +
    s(dependability) +
    s(courage) +
    s(fierceness) +
    s(power) +
    s(kinetic) +
    s(energy)  +
    s(blast) +
    s(heat) +
    s(cold) +
    s(electricity) +
    s(acid) +
    s(stun),
  data = normalized_df,
  family = gaussian()
)

summary(model.gam.everything)
plot(model.gam.everything)


model.gbm.everything <- gbm(
  level ~
    hardiness +
    fortitude +
    dexterity +
    endurance +
    intellect +
    cleverness +
    dependability +
    courage +
    fierceness +
    power +
    kinetic +
    energy  +
    blast +
    heat +
    cold +
    electricity +
    acid +
    stun,
  data = normalized_df
)

summary(model.gbm.everything, plotit = FALSE)
plot(model.gbm.everything, i.var = "dexterity")




model.gam.everything.finalstats <- gam(
  level ~
    s(average_ham) +
    s(dps) +
    armor +
    s(kinen) +
    s(nonkinen),
  data = normalized_df,
  family = gaussian()
)

summary(model.gam.everything.finalstats)
plot(model.gam.everything.finalstats)


nonamror <- normalized_df %>% filter(fortitude < 500)
model.gam.everything.nonarmor <- gam(
  level ~
    s(hardiness) +
    s(fortitude) +
    s(dexterity) +
    s(endurance) +
    s(intellect) +
    s(cleverness) +
    s(dependability) +
    s(courage) +
    s(fierceness) +
    s(power) +
    s(kinetic) +
    s(energy)  +
    s(blast) +
    s(heat) +
    s(cold) +
    s(electricity) +
    s(acid) +
    s(stun),
  data = nonamror,
  family = gaussian()
)
summary(model.gam.everything.nonarmor)
plot(model.gam.everything.nonarmor)



armor <- normalized_df %>% filter(fortitude >= 500)
model.gam.everything.armor <- gam(
  level ~
    s(hardiness) +
    s(fortitude) +
    s(dexterity) +
    s(endurance) +
    s(intellect) +
    s(cleverness) +
    s(dependability) +
    s(courage) +
    s(fierceness) +
    s(power) +
    s(kinetic) +
    s(energy)  +
    s(blast) +
    s(heat) +
    s(cold) +
    s(electricity) +
    s(acid) +
    s(stun),
  data = armor,
  family = gaussian()
)
summary(model.gam.everything.armor)
plot(model.gam.everything.armor)


model.lm.everything.armor <- lm(
  level ~
    hardiness +
    fortitude +
    dexterity +
    endurance +
    intellect +
    cleverness +
    dependability +
    courage +
    fierceness +
    power +
    kinetic +
    energy  +
    blast +
    heat +
    cold +
    electricity +
    acid +
    stun,
  data = armor
)
summary(model.lm.everything.armor)


# model.gbm.everything <- gbm(
#   level ~
#     hardiness +
#     fortitude +
#     dexterity +
#     endurance +
#     intellect +
#     cleverness +
#     dependability +
#     courage +
#     power +
#     average_res,
#   data = normalized_df
# )
#
# summary(model.gbm.everything, plotit = FALSE)

model.gam.highest.influence <- gam(
  level ~
    s(hardiness) +
    s(fortitude) +
    s(dexterity) +
    s(intellect) +
    s(cleverness) +
    s(power) +
    s(kinetic) +
    s(energy) +
    s(cold),
  data = normalized_df
)

summary(model.gam.highest.influence)

ggplot(normalized_df, aes(x = predict(model.gam.highest.influence, newdata = normalized_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")



############## Clusters

model_cluster <- function(df) {
  model.gam.highest.influence <- gam(
    level ~
      s(hardiness) +
      s(fortitude) +
      s(dexterity) +
      s(intellect) +
      s(cleverness) +
      s(power) +
      s(kinetic) +
      s(energy) +
      s(cold),
    data = df
  )

  print(summary(model.gam.highest.influence))

  ggplot(df, aes(x = predict(model.gam.highest.influence, newdata = df), y = level)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
    scale_color_discrete(name = "Max Relative Term")

  return(model.gam.highest.influence)
}

model.cluster.1 <- normalized_df %>%
  filter(cluster == 1) %>%
  model_cluster()

ggplot(normalized_df, aes(x = predict(model.cluster.1, newdata = normalized_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

model.cluster.2 <- normalized_df %>%
  filter(cluster == 2) %>%
  model_cluster()

ggplot(normalized_df, aes(x = predict(model.cluster.2, newdata = normalized_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")


model.cluster.3 <- normalized_df %>%
  filter(cluster == 3) %>%
  model_cluster()

ggplot(normalized_df, aes(x = predict(model.cluster.3, newdata = normalized_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")


### GBM
model.gbm <- gbm(
  level ~
    average_hdi +
    fortitude +
    cleverness +
    power +
    kinen +
    nonkinen,
  data = normalized_df
)
summary(model.gbm, plotit = FALSE)
plot(model.gbm, i.var = "average_hdi")
plot(model.gbm, i.var = "fortitude")
plot(model.gbm, i.var = "cleverness")
plot(model.gbm, i.var = "power")
plot(model.gbm, i.var = "kinen")
plot(model.gbm, i.var = "nonkinen")

ggplot(normalized_df, aes(x = predict(model.gbm, newdata = normalized_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")


model.gbm.finalstats <- gbm(
  level ~
    average_ham +
    armor +
    average_dps +
    kinen +
    nonkinen,
  data = normalized_df
)
summary(model.gbm.finalstats, plotit = FALSE)
plot(model.gbm.finalstats, i.var = "average_ham")
plot(model.gbm.finalstats, i.var = "armor")
plot(model.gbm.finalstats, i.var = "average_dps")
plot(model.gbm.finalstats, i.var = "kinen")
plot(model.gbm.finalstats, i.var = "nonkinen")

ggplot(normalized_df, aes(x = predict(model.gbm.finalstats, newdata = normalized_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")




#### GAM

model.gam <- gam(
  level ~
    s(hardiness) +
    s(fortitude) +
    s(dexterity) +
    s(intellect) +
    s(cleverness) +
    s(power) +
    s(kinetic) +
    s(energy) +
    s(cold),
  data = normalized_df
)
summary(model.gam)
plot(model.gam, select = 1)
plot(model.gam, select = 2)
plot(model.gam, select = 3)
plot(model.gam, select = 4)
plot(model.gam, select = 5)
plot(model.gam, select = 6)

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

max.term.1 <- normalized_df %>% filter(max_relative_term == 1)
ggplot(max.term.1, aes(x = predict(model.gam, newdata = max.term.1), y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

max.term.2 <- normalized_df %>% filter(max_relative_term == 2)
ggplot(max.term.2, aes(x = predict(model.gam, newdata = max.term.2), y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

max.term.3 <- normalized_df %>% filter(max_relative_term == 3)
ggplot(max.term.3, aes(x = predict(model.gam, newdata = max.term.3), y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")


max.term.4 <- normalized_df %>% filter(max_relative_term == 4)
ggplot(max.term.4, aes(x = predict(model.gam, newdata = max.term.4), y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

max.term.5 <- normalized_df %>% filter(max_relative_term == 5)
ggplot(max.term.5, aes(x = predict(model.gam, newdata = max.term.5), y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

max.term.6 <- normalized_df %>% filter(max_relative_term == 6)
ggplot(max.term.6, aes(x = predict(model.gam, newdata = max.term.6), y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

max.term.7 <- normalized_df %>% filter(max_relative_term == 7)
ggplot(max.term.6, aes(x = predict(model.gam, newdata = max.term.6), y = level, color = max_relative_term)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
  scale_color_discrete(name = "Max Relative Term")

max.term.8 <- normalized_df %>% filter(max_relative_term == 8)
max.term.9 <- normalized_df %>% filter(max_relative_term == 9)





# My actual suspicions:


model.gam <- gam(
  level ~
    s(average_hdi) +
    s(fortitude) +
    s(cleverness) +
    s(power) +
    s(kinen) +
    s(nonkinen),
  data = normalized_df
)
summary(model.gam)
plot(model.gam, select = 1)
plot(model.gam, select = 2)
plot(model.gam, select = 3)
plot(model.gam, select = 4)
plot(model.gam, select = 5)
plot(model.gam, select = 6)









#### GBM

# model.gbm <- gbm(
#   level ~ average_hdi + average_dps_attr + average_res + armor_factor,
#   data = normalized_df
# )
#
# summary(model.gbm, plotit = FALSE)
# plot(model.gbm, i.var = "average_dps_attr")
# predictions.gbm.level <- predict.gbm(model.gbm, newdata = normalized_df)
#
# ggplot(normalized_df, aes(x = predictions.gbm.level, y = level, color = max_relative_term)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
#   labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level") +
#   scale_color_discrete(name = "Max Relative Term")

####
library(segmented)
plot(model.gam, select = 1)

linear.fit.level <- lm(level ~
                         average_hdi +
                         fortitude +
                         power +
                         cleverness +
                         kinen +
                         nonkinen, data = normalized_df)
summary(linear.fit.level)
bptest(linear.fit.level)

ggplot(normalized_df, aes(x = predict(linear.fit.level, newdata = normalized_df), y = level)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions Colored by Max Relative Term", x = "Predicted Level", y = "Actual Level")


segmented.fit.average_hdi <- segmented(linear.fit.level, seg.Z = ~average_hdi, npsi = 2)
summary(segmented.fit.average_hdi)
plot(segmented.fit.average_hdi)
lines.segmented(segmented.fit.average_hdi)
points.segmented(segmented.fit.average_hdi)

plot(model.gam, select = 2)

segmented.fit.power <- segmented(linear.fit.level, seg.Z = ~fortitude, npsi = 1)
summary(segmented.fit.power)
plot(segmented.fit.power)
lines.segmented(segmented.fit.power)
points.segmented(segmented.fit.power)


plot(model.gam, select = 3)

segmented.fit.cleverness <- segmented(linear.fit.level, seg.Z = ~cleverness, npsi = 1)
summary(segmented.fit.cleverness)
plot(segmented.fit.cleverness)
lines.segmented(segmented.fit.cleverness)
points.segmented(segmented.fit.cleverness)



plot(model.gam, select = 4)

segmented.fit.courage <- segmented(linear.fit.level, seg.Z = ~courage, npsi = 2)
summary(segmented.fit.courage)
plot(segmented.fit.courage)
lines.segmented(segmented.fit.courage)
points.segmented(segmented.fit.courage)























### Segment analysis
no_armor_df <- normalized_df %>% filter(armor == 0)
armor_df <- normalized_df %>% filter(armor == 1 & fortitude >= 500)



##### Influence Analysis Again
model.gam.everything.noarmor <- gam(
  level ~
    s(hardiness) +
    s(fortitude) +
    s(dexterity) +
    s(endurance) +
    s(intellect) +
    s(cleverness) +
    s(dependability) +
    s(courage) +
    s(fierceness) +
    s(power) +
    s(kinetic) +
    s(energy)  +
    s(blast) +
    s(heat) +
    s(cold) +
    s(electricity) +
    s(acid) +
    s(stun),
  data = no_armor_df,
  family = gaussian()
)

summary(model.gam.everything.noarmor)


model.gam.everything.armor <- gam(
  level ~
    s(hardiness) +
    s(fortitude) +
    s(dexterity) +
    s(endurance) +
    s(intellect) +
    s(cleverness) +
    s(dependability) +
    s(courage) +
    s(fierceness) +
    s(power) +
    s(kinetic) +
    s(energy)  +
    s(blast) +
    s(heat) +
    s(cold) +
    s(electricity) +
    s(acid) +
    s(stun),
  data = armor_df,
  family = gaussian()
)

summary(model.gam.everything.armor)
















# No armor
ggplot(no_armor_df, aes(x = average_hdi, y = level, color = cluster)) +
  geom_point() +
  labs(title = "Average HDI vs Level", x = "average_hdi", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(no_armor_df, aes(x = courage, y = level, color = cluster)) +
  geom_point() +
  labs(title = "Courage vs Level", x = "courage", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(no_armor_df, aes(x = power, y = level, color = cluster)) +
  geom_point() +
  labs(title = "power vs Level", x = "power", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(no_armor_df, aes(x = cleverness, y = level, color = cluster)) +
  geom_point() +
  labs(title = "cleverness vs Level", x = "cleverness", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(no_armor_df, aes(x = kinen, y = level, color = cluster)) +
  geom_point() +
  labs(title = "average_res vs Level", x = "average_res", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(no_armor_df, aes(x = nonkinen, y = level, color = cluster)) +
  geom_point() +
  labs(title = "average_res vs Level", x = "average_res", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(no_armor_df, aes(x = fortitude, y = level, color = cluster)) +
  geom_point() +
  labs(title = "fortitude vs Level", x = "fortitude", y = "level") +
  scale_color_discrete(name = "Cluster")



model.gam.noarmor <- gam(
  level ~
    s(average_hdi) +
    s(fortitude) +
    s(cleverness) +
    s(power) +
    s(kinen) +
    s(nonkinen),
  data = no_armor_df
)
summary(model.gam.noarmor)
plot(model.gam.noarmor, select = 1)
plot(model.gam.noarmor, select = 2)
plot(model.gam.noarmor, select = 3)
plot(model.gam.noarmor, select = 4)
plot(model.gam.noarmor, select = 5)
plot(model.gam.noarmor, select = 6)
plot(model.gam.noarmor, select = 7)


ggplot(no_armor_df, aes(x = predict(model.gam.noarmor, newdata = no_armor_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")


model.gbm.noarmor <- gbm(
  level ~
    average_hdi +
    fortitude +
    cleverness +
    power +
    kinen +
    nonkinen,
  data = no_armor_df
)
summary(model.gbm.noarmor, plotit = FALSE)
plot(model.gbm.noarmor, i.var = "average_hdi")
plot(model.gbm.noarmor, i.var = "fortitude")
plot(model.gbm.noarmor, i.var = "cleverness")
plot(model.gbm.noarmor, i.var = "power")
plot(model.gbm.noarmor, i.var = "kinen")
plot(model.gbm.noarmor, i.var = "nonkinen")

residuals <- no_armor_df$level - predict(model.gbm.noarmor, newdata = no_armor_df)
min(residuals)
max(residuals)
mean(residuals)
var(residuals)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)



ggplot(no_armor_df, aes(x = predict(model.gbm.noarmor, newdata = no_armor_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")







#
test_df <- no_armor_df %>%
  filter(kinen <= 25 % kinen > 10)

model.gam.noarmor.negative.nonkinen <- gam(
  level ~
    s(average_hdi) +
    s(fortitude) +
    s(cleverness) +
    s(power) +
    s(kinen) +
    s(nonkinen),
  data = test_df
)
summary(model.gam.noarmor.negative.nonkinen)
plot(model.gam.noarmor.negative.nonkinen, select = 1)
plot(model.gam.noarmor.negative.nonkinen, select = 2)
plot(model.gam.noarmor.negative.nonkinen, select = 3)
plot(model.gam.noarmor.negative.nonkinen, select = 4)
plot(model.gam.noarmor.negative.nonkinen, select = 5)
plot(model.gam.noarmor.negative.nonkinen, select = 6)
plot(model.gam.noarmor.negative.nonkinen, select = 7)



ggplot(test_df, aes(x = predict(model.gam.noarmor.negative.nonkinen, newdata = test_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")


linear.fit.level.noarmor.negative.nonkinen <- lm(
  level ~
    average_hdi +
    fortitude +
    cleverness +
    power +
    kinen +
    nonkinen,
  data = test_df
)
summary(linear.fit.level.noarmor.negative.nonkinen)
rs.model.level.noarmor.negative.nonkinen <- resid(linear.fit.level.noarmor.negative.nonkinen)
qqnorm(rs.model.level.noarmor.negative.nonkinen)
qqline(rs.model.level.noarmor.negative.nonkinen)
shapiro.test(rs.model.level.noarmor.negative.nonkinen)
bptest(linear.fit.level.noarmor.negative.nonkinen)

















linear.fit.level.noarmor <- lm(
  level ~
    average_hdi +
    fortitude +
    cleverness +
    power +
    kinen +
    nonkinen,
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
slope(segmented.fit.average_hdi.noarmor)
intercept(segmented.fit.average_hdi.noarmor)


plot(model.gam.noarmor, select = 2)

segmented.fit.power.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~fortitude, npsi = 2)
summary(segmented.fit.power.noarmor)
slope(segmented.fit.power.noarmor)
plot(segmented.fit.power.noarmor)
lines.segmented(segmented.fit.power.noarmor)
points.segmented(segmented.fit.power.noarmor)




plot(model.gam.noarmor, select = 3)

segmented.fit.cleverness.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~cleverness, npsi = 1)
summary(segmented.fit.cleverness.noarmor)
plot(segmented.fit.cleverness.noarmor)
lines.segmented(segmented.fit.cleverness.noarmor)
points.segmented(segmented.fit.cleverness.noarmor)




plot(model.gam.noarmor, select = 4)

segmented.fit.kinen.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~kinen, npsi = 5)
summary(segmented.fit.kinen.noarmor)
plot(segmented.fit.kinen.noarmor)
lines.segmented(segmented.fit.kinen.noarmor)
points.segmented(segmented.fit.kinen.noarmor)



plot(model.gam.noarmor, select = 5)

segmented.fit.kinen.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~kinen, npsi = 1)
summary(segmented.fit.kinen.noarmor)
plot(segmented.fit.kinen.noarmor)
lines.segmented(segmented.fit.kinen.noarmor)
points.segmented(segmented.fit.kinen.noarmor)
slope(segmented.fit.kinen.noarmor)
intercept(segmented.fit.kinen.noarmor)

plot(model.gam.noarmor, select = 6)

segmented.fit.nonkinen.noarmor <- segmented(linear.fit.level.noarmor, seg.Z = ~nonkinen, npsi = 1)
summary(segmented.fit.nonkinen.noarmor)
plot(segmented.fit.nonkinen.noarmor)
lines.segmented(segmented.fit.nonkinen.noarmor)
points.segmented(segmented.fit.nonkinen.noarmor)
slope(segmented.fit.nonkinen.noarmor)
intercept(segmented.fit.nonkinen.noarmor)


# Armor

model.gam.armor <- gam(
  level ~
    s(average_hdi) +
    s(fortitude) +
    s(cleverness) +
    s(power) +
    s(kinen) +
    s(nonkinen),
  data = armor_df
)
summary(model.gam.armor)
plot(model.gam.armor, select = 1)
plot(model.gam.armor, select = 2)
plot(model.gam.armor, select = 3)
plot(model.gam.armor, select = 4)
plot(model.gam.armor, select = 5)
plot(model.gam.armor, select = 6)


linear.fit.level.armor <- lm(
  level ~
    average_hdi +
    fortitude +
    cleverness +
    power +
    kinen +
    nonkinen,
  data = armor_df
)
summary(linear.fit.level.armor)

ggplot(armor_df, aes(x = predict(linear.fit.level.armor, newdata = armor_df), y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
  scale_color_discrete(name = "Cluster")
ggplot(normalized_df, aes(x = predict(linear.fit.level.armor, newdata = normalized_df), y = level, color = cluster)) +
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
slope(segmented.fit.average_hdi.armor)
intercept(segmented.fit.average_hdi.armor)


plot(model.gam.armor, select = 2)

segmented.fit.power.armor <- segmented(linear.fit.level.armor, seg.Z = ~fortitude, npsi = 1)
summary(segmented.fit.power.armor)
plot(segmented.fit.power.armor)
lines.segmented(segmented.fit.power.armor)
points.segmented(segmented.fit.power.armor)
summary(segmented.fit.power.armor)
slope(segmented.fit.power.armor)
intercept(segmented.fit.power.armor)


plot(model.gam.armor, select = 3)

segmented.fit.cleverness.armor <- segmented(linear.fit.level.armor, seg.Z = ~cleverness, npsi = 1)
summary(segmented.fit.cleverness.armor)
plot(segmented.fit.cleverness.armor)
lines.segmented(segmented.fit.cleverness.armor)
points.segmented(segmented.fit.cleverness.armor)




plot(model.gam.armor, select = 4)

segmented.fit.courage.armor <- segmented(linear.fit.level.armor, seg.Z = ~courage, npsi = 1)
summary(segmented.fit.courage.armor)
plot(segmented.fit.courage.armor)
lines.segmented(segmented.fit.courage.armor)
points.segmented(segmented.fit.courage.armor)


plot(model.gam.armor, select = 5)

segmented.fit.courage.armor <- segmented(linear.fit.level.armor, seg.Z = ~courage, npsi = 1)
summary(segmented.fit.courage.armor)
plot(segmented.fit.courage.armor)
lines.segmented(segmented.fit.courage.armor)
points.segmented(segmented.fit.courage.armor)

plot(model.gam.armor, select = 6)

segmented.fit.courage.armor <- segmented(linear.fit.level.armor, seg.Z = ~nonkinen, npsi = 1)
summary(segmented.fit.courage.armor)
plot(segmented.fit.courage.armor)
lines.segmented(segmented.fit.courage.armor)
points.segmented(segmented.fit.courage.armor)
slope(segmented.fit.courage.armor)
intercept(segmented.fit.courage.armor)






# armor_df$predicted_level <- creature_level_armor(armor_df)
# armor_df$residuals <- armor_df$predicted_level - armor_df$level
# qqnorm(armor_df$residuals)
# qqline(armor_df$residuals)
#
# shapiro.test(armor_df$residuals)
#
#
# ggplot(armor_df, aes(x = predicted_level, y = level, color = cluster)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
#   labs(title = "Predicted Level vs Actual", x = "predicted_level", y = "level") +
#   scale_color_discrete(name = "Cluster")


############ Cluster Analysis
# Why? the armor fit extremely well, and it was all in the same cluster





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
armor_df %>% View()
test_cl_no_armor(33, 8, 34, 16, 39, 505, 57, 2, 2, 0, 0, 3, 0, 0, -35)
test_cl_no_armor(611, 442, 352, 105, 50, 519, 137, 58, -64, -2, -35, -34, -99, 40, -99)
test_cl_no_armor(25, 5, 13, 5, 7, 339, 16, 6, 0, 4, 0, -39, -39, -39, -99)

test_cl_armor(33, 8, 34, 16, 39, 505, 57, 2, 2, 0, 0, 3, 0, 0, -35)
test_cl_armor(611, 442, 352, 105, 50, 519, 137, 58, -64, -2, -35, -34, -99, 40, -99)
test_cl_armor(25, 5, 13, 5, 7, 339, 16, 6, 0, 4, 0, -39, -39, -39, -99)
