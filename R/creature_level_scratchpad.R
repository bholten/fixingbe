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

creatures <- read_csv("data/clean/furrycat/creatures.csv") %>%
  filter(!serial %in% bad_data) %>%
  filter(!(skin == "angler" & level == 2)) %>%
  filter(!(skin == "bearded_jax" & level == 2)) %>%
  filter(!(skin == "boar_wolf" & level == 2)) %>%
  filter(!(skin == "bocatt" & level == 2)) %>%
  filter(!(skin == "choku" & level == 2)) %>%
  filter(!(skin == "durni" & level == 2)) %>%
  filter(!(skin == "eopi" & level == 2)) %>%
  filter(!(skin == "gnort" & level == 2)) %>%
  filter(!(skin == "hermit_spider" & level == 2)) %>%
  filter(!(skin == "huurton" & level == 2)) %>%
  filter(!(skin == "kima" & level == 2)) %>%
  filter(!(skin == "krahbu" & level == 2)) %>%
  filter(!(skin == "kusak" & level == 2)) %>%
  filter(!(skin == "langlatch" & level == 2)) %>%
  filter(!(skin == "mott" & level == 2)) %>%
  filter(!(skin == "roba" & level == 2)) %>%
  filter(!(skin == "shear_mite" & level == 2)) %>%
  filter(!(skin == "slice_hound" & level == 2)) %>%
  filter(!(skin == "squall" & level == 2)) %>%
  filter(!(skin == "swirl_prong" & level == 2)) %>%
  filter(!(skin == "vir_vir" & level == 2)) %>%
  filter(!(skin == "bageraset" & level == 5)) %>%
  filter(!(skin == "bantha" & level == 5)) %>%
  filter(!(skin == "blurrg" & level == 5)) %>%
  filter(!(skin == "bol" & level == 5)) %>%
  filter(!(skin == "bolle_bol" & level == 5)) %>%
  filter(!(skin == "bolma" & level == 5)) %>%
  filter(!(skin == "bordok" & level == 5)) %>%
  filter(!(skin == "brackaset" & level == 5)) %>%
  filter(!(skin == "carrion_spat" & level == 5)) %>%
  filter(!(skin == "cu_pa" & level == 5)) %>%
  filter(!(skin == "dalyrake" & level == 5)) %>%
  filter(!(skin == "dewback" & level == 5)) %>%
  filter(!(skin == "dune_lizard" & level == 5)) %>%
  filter(!(skin == "falumpaset" & level == 5)) %>%
  filter(!(skin == "gualama" & level == 5)) %>%
  filter(!(skin == "guf_drolg" & level == 5)) %>%
  filter(!(skin == "gurnaset" & level == 5)) %>%
  filter(!(skin == "gurrcat" & level == 5)) %>%
  filter(!(skin == "gurreck" & level == 5)) %>%
  filter(!(skin == "ikopi" & level == 5)) %>%
  filter(!(skin == "kaadu" & level == 5)) %>%
  filter(!(skin == "kahmurra" & level == 5)) %>%
  filter(!(skin == "kwi" & level == 5)) %>%
  filter(!(skin == "mawgax" & level == 5)) %>%
  filter(!(skin == "narglatch" & level == 5)) %>%
  filter(!(skin == "pugoriss" & level == 5)) %>%
  filter(!(skin == "verne" & level == 5)) %>%
  filter(!(skin == "zucca_boar" & level == 5)) %>%
  filter(!(skin == "huf_dun" & level == 10)) %>%
  filter(!(skin == "piket" & level == 10)) %>%
  filter(!(skin == "razor_cat" & level == 10)) %>%
  filter(!(skin == "veermok" & level == 10)) %>%
  filter(!(skin == "woolamander" & level == 10)) %>%
  filter(!(skin == "gronda" & level == 15)) %>%
  filter(!(skin == "kliknik" & level == 15)) %>%
  filter(!(skin == "ronto" & level == 15)) %>%
  filter(!(skin == "snorbal" & level == 15)) %>%
  filter(!(skin == "thune" & level == 15)) %>%
  filter(!(skin == "tyblis" & level == 15)) %>%
  filter(!(skin == "vesp" & level == 15)) %>%
  filter(!(skin == "malkloc" & level == 20)) %>%
  filter(!(skin == "torton" & level == 20)) %>%
  filter(!(skin == "graul" & level == 25)) %>%
  filter(!(skin == "merek" & level == 25)) %>%
  filter(!(skin == "sharnaff" & level == 25)) %>%
  filter(!(skin == "fambaa" & level == 30)) %>%
  filter(!(skin == "rancor" & level == 35)) %>%
  filter(!(skin == "kimogila" & level == 40))

# read_csv("data/clean/furrycat/creatures.csv") %>%
#   anti_join(creatures, by = "serial") %>%
#   View()


templates <- read_csv("data/clean/furrycat/templates.csv")
combined_data <- creatures %>% inner_join(templates, by = join_by(template_id == serial), suffix = c("", ".template"))
normalized_df <- combined_data %>%
  mutate_at(
    vars("kinetic",
         "energy",
         "blast",
         "heat",
         "cold",
         "electricity",
         "acid",
         "stun",
         "kinetic.effective",
         "kinetic.special",
         "energy.effective",
         "energy.special",
         "blast.effective",
         "blast.special",
         "heat.effective",
         "heat.special",
         "cold.effective",
         "cold.special",
         "electricity.effective",
         "electricity.special",
         "acid.effective",
         "acid.special",
         "stun.effective",
         "stun.special"),
    ~ ifelse(is.na(.), 0, .)
  )

experiments <- read_csv("data/clean/furrycat/experiments.csv")
samples <- read_csv("data/clean/furrycat/samples.csv")

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
  mutate(average_eff = (kinetic.effective.x + energy.effective.x + blast.effective.x + heat.effective.x + cold.effective.x + electricity.effective.x + acid.effective.x + stun.effective.x) / 8) %>%
  mutate(average_spc = (kinetic.special.x + energy.special.x + blast.special.x + heat.special.x + cold.special.x + electricity.special.x + acid.special.x + stun.special.x) / 8)


normalized_df <- normalized_df %>%
  mutate(average_res = (average_spc + average_eff) / 2)

#normalized_df %>% select(average_res) %>% View()
normalized_df <- normalized_df %>%
  mutate(effective_ham = average_ham * (1 + (armor.x * 1.5)) * (1 + average_res / 100))

### CL CONTRIBUTORS
normalized_df <- normalized_df %>%
  mutate(cl.contributor.ham = (health + action + mind) * 730)

normalized_df <- normalized_df %>%
  mutate(cl.contributor.armor = armor.x * 10)

normalized_df <- normalized_df %>%
  mutate(cl.contributor.dps = 0.99 * (((damage_low + damage_high) / 2) / speed) / 10)


##########################
# Clustering experiments #
##########################
library(cluster)
library(tidyr)
# average_hdi +
#   armor +
#   endurance + dependability + fortitude +
#   average_dps +
#   average_res +
#   quality

clustering_data <- normalized_df %>%
  select(c(average_hdi, armor, average_res, average_dps, armor, quality, endurance, dependability, fortitude)) %>%
  select(where(is.numeric)) %>%
  mutate_all(replace_na, 0)

kmeans_result <- kmeans(clustering_data, centers = 7)
normalized_df$cluster <- as.factor(kmeans_result$cluster)

ggplot(normalized_df, aes(x = average_res, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_res", y = "level") +
  scale_color_discrete(name = "Cluster")

# normalized_df %>%
#   filter(serial == "d5j7caq6" | serial == "3tgq7fdo" | serial == "7p5lkodh") %>%
#   View()



### Some plots
ggplot(normalized_df, aes(x = average_ham, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_ham", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = average_dps, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_dps", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = average_res, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_res", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = cl.contributor.ham, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_eff", y = "level") +
  scale_color_discrete(name = "Cluster")

ggplot(normalized_df, aes(x = cl.contributor.dps, y = level, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "average_eff", y = "level") +
  scale_color_discrete(name = "Cluster")

#####################
# library(FactoMineR)
#
# # Perform PCA
# pcad <- normalized_df %>%
#   select(where(is.numeric))
#
# pca_result <- PCA(pcad, graph = FALSE)
#
# # Create a data frame for plotting
# plot_data <- data.frame(PC1 = pca_result$ind$coord[, 1],
#                         PC2 = pca_result$ind$coord[, 2],
#                         Cluster = as.factor(normalized_df$cluster))
#
# ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
#   geom_point() +
#   labs(title = "PCA of K-means Clusters", x = "Principal Component 1", y = "Principal Component 2") +
#   scale_color_discrete(name = "Cluster")
# ###############################
# library(FNN)
#
# # Assuming you have a function to calculate similarity
# # excluding 'total_vulns' and 'creature_level'
# similar_creatures <- pcad %>%
#   mutate_all(replace_na, 0) %>%
#   get.knn(k = 2)

# normalized_df %>%
#   filter(serial == "00pas7q5") %>%
#   add_vuln_column("electricity") %>%
#   select(
#     kinetic.vuln,
#     energy.vuln,
#     blast.vuln,
#     heat.vuln,
#     cold.vuln,
#     electricity.vuln,
#     acid.vuln,
#     stun.vuln,
#     blast.special.x,
#     blast.special.physique,
#     blast.effective.physique,
#     blast.special.prowess,
#     blast.effective.prowess,
#     blast.special.mental,
#     blast.effective.mental,
#     blast.special.psychology,
#     blast.effective.psychology,
#     blast.special.aggression,
#     blast.effective.aggression
#   ) %>%
#   View()

#
# add_rollup_cl <- function(dataframe) {
#   dataframe$average_ham <- (dataframe$health + dataframe$action + dataframe$mind) / 3
#   dataframe$average_dps <- ((dataframe$damage_high + dataframe$damage_low) / 2) * dataframe$to_hit * dataframe$speed
#   dataframe <- dataframe %>%
#     rowwise() %>%
#     mutate(average_resist = mean(c_across(c(
#       kinetic.special, kinetic.effective,
#       energy.special, energy.effective,
#       blast.special, blast.effective,
#       heat.special, heat.effective,
#       cold.special, cold.effective,
#       electricity.special, electricity.effective,
#       acid.special, acid.effective,
#       stun.special, stun.effective))))
#
#   dataframe$level_factor <- (dataframe$ham_mean / 15000) * (dataframe$average_resist / 100) + (dataframe$average_dps / 1000)
#   return(dataframe)
# }
#
# add_gbm_residuals <- function(dataframe, gbm_model) {
#   dataframe$creature_level_prediction <-predict(gbm_model, newdata = dataframe, n.trees = 500)
#   dataframe$creature_level_residuals <- dataframe$level - dataframe$creature_level_prediction
#   return(dataframe)
# }
#
# plot_model <- function(dataframe) {
#   ggplot(data = dataframe, mapping = aes(x = level, y = creature_level_prediction)) +
#     geom_point() +
#     ggtitle("Actual vs Predicted Creature Level") +
#     xlab("creature_level") +
#     ylab("creature_level_prediction")
# }
#
# plot_prediction_with_residuals <- function(dataframe) {
#   ggplot(dataframe, aes(x = level, y = creature_level_prediction, color = abs(creature_level_residuals))) +
#     geom_point() +
#     scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(abs(dataframe$creature_level_residuals))) +
#     ggtitle("Creature Level vs Average Resist") +
#     xlab("Average Resist") +
#     ylab("Creature Level") +
#     theme_minimal()
# }
#
# plot_avg_resists <- function(dataframe) {
#   ggplot(dataframe, aes(x = average_resist, y = level, color = abs(creature_level_residuals))) +
#     geom_point() +
#     scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = median(abs(dataframe$creature_level_residuals))) +
#     ggtitle("Creature Level vs Average Resist") +
#     xlab("Average Resist") +
#     ylab("Creature Level") +
#     theme_minimal()
# }
#
# plot_avg_ham <- function(dataframe) {
#   ggplot(dataframe, aes(x = ham_mean, y = level, color = abs(creature_level_residuals))) +
#     geom_point() +
#     scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = median(abs(dataframe$creature_level_residuals))) +
#     ggtitle("Average HAM vs Level") +
#     xlab("Average HAM") +
#     ylab("Creature Level") +
#     theme_minimal()
# }
#
# plot_avg_dps <- function(dataframe) {
#   ggplot(dataframe, aes(x = average_dps, y = level, color = abs(creature_level_residuals))) +
#     geom_point() +
#     scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = median(abs(dataframe$creature_level_residuals))) +
#     ggtitle("Average DPS vs Level") +
#     xlab("Average DPS") +
#     ylab("Creature Level") +
#     theme_minimal()
# }
#
# plot_level_factor <- function(dataframe) {
#   ggplot(dataframe, aes(x = level_factor, y = level, color = abs(creature_level_residuals))) +
#     geom_point() +
#     scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = median(abs(dataframe$creature_level_residuals))) +
#     ggtitle("Level Factor vs Level") +
#     xlab("Level Factor") +
#     ylab("Creature Level") +
#     theme_minimal()
# }
#

dmg_fix_data <- normalized_df %>%
  filter(damage_high - damage_low > 10)

gbm_model <- gbm(
  formula = level ~
    hardiness + dexterity + intellect +
    armor +
    endurance + dependability + fortitude +
    power + cleverness + courage +
    average_res +
    quality,
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
plot.gbm(gbm_model, i.var = "dexterity")
plot.gbm(gbm_model, i.var = "endurance")
plot.gbm(gbm_model, i.var = "dependability")
plot.gbm(gbm_model, i.var = "fortitude")
plot.gbm(gbm_model, i.var = "average_dps")
plot.gbm(gbm_model, i.var = "average_res")
plot.gbm(gbm_model, i.var = "quality")

normalized_df$predicted_level <- predict(gbm_model, newdata = normalized_df, type = "response", trees = 500)
normalized_df$residuals <- normalized_df$level - normalized_df$predicted_level

ggplot(normalized_df, aes(x = predicted_level, y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted Level vs Level")


# library(pdp)
# pd <- partial(gbm_model, pred.var = "average_res", n.trees = 500)
# plot(pd)

# library(ALEPlot)
# ice_data <- ALEPlot::ice(gbm_model, normalized_df, "average_res")
# plot(ice_data)
#
# library(lime)
# explainer <- lime(normalized_df, gbm_model)
# explanation <- explain(normalized_df, explainer, n_labels = 1, n_features = 5)
# plot_features(explanation)
#
# pretty.gbm.tree(gbm_model, i.tree = 1)


fort_below_500 <- normalized_df %>%
  filter(fortitude <= 500)

fort_above_500 <- normalized_df %>%
  filter(fortitude > 500)

model <- glm(
  formula = level ~
    average_hdi +
    armor +
    endurance + dependability + fortitude +
    average_dps +
    average_res +
    quality,
  data = normalized_df,
  family = gaussian()
)
summary(model)

normalized_df$predicted_level <- predict(model, newdata = normalized_df, type = "response")
normalized_df$residuals <- normalized_df$level - normalized_df$predicted_level

level_estimate <- function(hardiness, fortitude, dexterity, intellect, power, cleverness, kinetic, energy, blast, heat, cold, electricity, acid, stun, quality) {
  hdi_cl <- (0.015 / 3) * (hardiness + dexterity + intellect)
  power_cl <- 0.015 * power
  cleverness_cl <- 0.05 * cleverness
  res_cl <- (0.13 / 8) * (kinetic + energy + blast + heat + cold + electricity + acid + stun)
  quality_cl <- 0.05 * quality
  armor_cl <- if (fortitude >= 500) { 10 } else { 0 }

  return(2 + hdi_cl + power_cl + cleverness_cl + res_cl + quality_cl + armor_cl)
}

level_fn <- function(data) {
  hardiness <- data$hardiness
  fortitude <- data$fortitude
  dexterity <- data$dexterity
  intellect <- data$intellect
  power <- data$power
  cleverness <- data$cleverness
  kinetic <- data$kinetic
  energy <- data$energy
  blast <- data$blast
  heat <- data$heat
  cold <- data$cold
  electricity <- data$electricity
  acid <- data$acid
  stun <- data$stun
  quality <- data$quality


  hdi_cl <- (0.015 / 3) * (hardiness + dexterity + intellect)
  power_cl <- 0.015 * power
  cleverness_cl <- 0.05 * cleverness
  res_cl <- (0.15 / 8) * (kinetic + energy + blast + heat + cold + electricity + acid + stun)
  quality_cl <- 0.05 * quality
  armor_cl <- if (fortitude >= 500) { 10 } else { 0 }

  return(2 + hdi_cl + power_cl + cleverness_cl + res_cl + quality_cl + armor_cl)
}

ggplot(normalized_df, aes(x = predicted_level, y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted Level vs Level")

rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)

standardized_residuals <- rstandard(model)
cutoff <- 1
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- normalized_df[outlier_indices, ]

outlier_records %>%
  select(serial, skin, level, predicted_level, residuals, health, action, mind, armor, to_hit, speed, damage_low, damage_high,
         kinetic, energy, blast, heat, cold, electricity, acid, stun,
         hardiness, fortitude, dexterity, endurance, intellect, cleverness, courage, dependability, power) %>%
  View()




library(mgcv)
library(broom)
model <- gam(
  formula = level ~
    s(average_hdi) +
    s(average_sec) +
    s(average_dps) +
    s(average_res) +
    armor,
  family = gaussian(),
  data = normalized_df
)
model <- gam(
  formula = level ~
    te(quality, average_hdi) +
    te(quality, fortitude) +
    te(quality, endurance) +
    te(quality, cleverness) +
    te(quality, courage) +
    te(quality, dependability) +
    te(quality, power) +
    te(quality, fierceness) +
    te(quality, average_res),
  family = gaussian(),
  data = normalized_df
)

newdata = normalized_df %>% filter(average_hdi < 200)

model <- gam(
  formula = damage_low ~
    s(power) +
    s(fierceness),
  family = gaussian(),
  data = normalized_df
)
normalized_df$predicted_health <- predict(model, newdata = normalized_df, type = "response")
normalized_df$residuals <- normalized_df$health - normalized_df$predicted_health
ggplot(normalized_df, aes(x = predicted_health, y = health, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted health vs health")

rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)
summary(model)
augment(model) %>% View()
plot(model, select = 2)


model <- gam(
  formula = level ~
    s(average_hdi) +
    s(fortitude) +
    s(endurance) +
    s(cleverness) +
    s(courage) +
    s(dependability) +
    s(power) +
    s(fierceness) +
    s(average_res) +
    armor,
  family = gaussian(),
  data = normalized_df
)
plot(model, selection = 6)
summary(model)
augment(model) %>% View()


## Average Resist breakpoints
model <- gam(
  formula = level ~
    s(average_hdi) +
    s(fortitude) +
    s(endurance) +
    s(cleverness) +
    s(courage) +
    s(dependability) +
    s(power) +
    s(fierceness) +
    s(average_res) +
    armor,
  family = gaussian(),
  data = normalized_df %>% filter(average_res <= -20)
)
plot(model, selection = 6)
summary(model)

model <- lm(
  formula = level ~
    average_hdi +
    fortitude +
    endurance +
    cleverness +
    courage +
    dependability +
    power +
    fierceness +
    average_res +
    armor,
  data = normalized_df %>% filter(average_res <= -20)
)
summary(model)

model <- lm(
  formula = level ~
    average_hdi +
    fortitude +
    endurance +
    cleverness +
    courage +
    dependability +
    power +
    fierceness +
    average_res +
    armor,
  data = normalized_df %>% filter(average_res > -20)
)
summary(model)

damage_model <- gam(
  formula = damage_high ~
    s(power) +
    s(fierceness),
  family = gaussian(),
  data = normalized_df
)
summary(damage_model)

plot(model, select = 8)
plot(damage_model, select = 2)

residuals <- residuals(model)
fitted_values <- fitted(model)

# Plotting residuals
plot(fitted_values, residuals)
abline(h = 0, col = "red")

normalized_df$predicted_level <- predict(model, newdata = normalized_df, type = "response")
normalized_df$residuals <- normalized_df$level - normalized_df$predicted_level
ggplot(normalized_df, aes(x = predicted_level, y = level, color = cluster)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted Level vs Level")

rs <- resid(model)
qqnorm(rs)
qqline(rs)
shapiro.test(rs)

standardized_residuals <- rstandard(model)
cutoff <- 1
outlier_indices <- which(abs(standardized_residuals) > cutoff)
outlier_records <- normalized_df[outlier_indices, ]

normalized_df %>%
  select(serial, quality, skin, level, predicted_level, residuals, health, action, mind, armor, to_hit, speed, damage_low, damage_high,
         kinetic, energy, blast, heat, cold, electricity, acid, stun,
         hardiness, fortitude, dexterity, endurance, intellect, cleverness, courage, dependability, power) %>%
  View()


#
# normalized_df <- add_gbm_residuals(normalized_df, gbm_model)
# normalized_df <- add_rollup_cl(normalized_df)
#
# plot_model(normalized_df)
# plot_avg_ham(normalized_df)
# plot_avg_dps(normalized_df)
# plot_avg_resists(normalized_df)
# plot_level_factor(normalized_df)
#
#
# gbm_model_averages <- gbm(
#   formula = level ~ average_ham + average_dps + average_resist,
#   data = normalized_df,
#   distribution = "gaussian",
#   n.trees = 500,
#   interaction.depth = 6,
#   n.minobsinnode = 5,
#   bag.fraction = 0.5,
#   cv.folds = 10
#   #shrinkage = 0.01
# )
#
# summary(gbm_model_averages)
# plot.gbm(gbm_model_averages, i.var = "average_ham")
# plot.gbm(gbm_model_averages, i.var = "average_dps")
# plot.gbm(gbm_model_averages, i.var = "average_resist")
#
# normalized_df <- add_gbm_residuals(normalized_df, gbm_model_averages)
# normalized_df <- add_rollup_cl(normalized_df)
#
# plot_model(normalized_df)
# plot_avg_ham(normalized_df)
# plot_avg_dps(normalized_df)
# plot_avg_resists(normalized_df)
# plot_level_factor(normalized_df)
#

library(glue)
HAM   = 0.73 / 1000
ARMOR = 10
DPS   = 0.99 / 10

KIN_POS   = 0.12
ENG_POS   = 0.1
BLAST_POS = 0.02
HEAT_POS  = 0.02
COLD_POS  = 0.02
ELEC_POS  = 0.02
ACID_POS  = 0.02
STUN_POS  = 0.02

KIN_NEG   = 0.2
ENG_NEG   = 0.12
BLAST_NEG = 0.04
HEAT_NEG  = 0.1
COLD_NEG  = 0.12
ELEC_NEG  = 0.04
ACID_NEG  = 0.02
STUN_NEG  = 0.02

resist_contribution <- function(res, pos_multiplier, neg_multiplier) {
  if (res > 0) {
    return(res * pos_multiplier)
  }
  else if (res < -50) {
    return(-50 * neg_multiplier)
  }
  else {
    return(res * neg_multiplier)
  }
}


creature_level <- function(data) {
  health      = data$health
  action      = data$action
  mind        = data$mind
  armor       = data$armor
  damage_low  = data$damage_low
  damage_high = data$damage_high
  to_hit      = data$to_hit
  speed       = data$speed
  kin         = data$kinetic
  eng         = data$energy
  blast       = data$blast
  heat        = data$heat
  cold        = data$cold
  electricty  = data$electricity
  acid        = data$acid
  stun        = data$stun


  # CL contributions
  ham_cl = (health + action + mind) * HAM
  armor_cl = armor * ARMOR
  dps_cl = (((damage_low * to_hit) / speed) / 10) * 0.99
  kin_cl = resist_contribution(kin, KIN_POS, KIN_NEG)
  eng_cl = resist_contribution(eng, ENG_POS, ENG_NEG)
  blast_cl = resist_contribution(blast, BLAST_POS, BLAST_NEG)
  heat_cl = resist_contribution(heat, HEAT_POS, HEAT_NEG)
  cold_cl = resist_contribution(cold, COLD_POS, COLD_NEG)
  electricity_cl = resist_contribution(electricty, ELEC_POS, ELEC_NEG)
  acid_cl = resist_contribution(acid, ACID_POS, ACID_NEG)
  stun_cl = resist_contribution(stun, STUN_POS, STUN_NEG)

  print(glue('HAM CL is {ham_cl}'))
  print(glue('Armor CL is {armor_cl}'))
  print(glue('DPS CL is {dps_cl} = (({damage_low} * {to_hit} / {speed}) / 10) * 0.99'))
  print(glue('Kin CL is {kin_cl}'))
  print(glue('Eng CL is {eng_cl}'))
  print(glue('Blast CL is {blast_cl}'))
  print(glue('Heat CL is {heat_cl}'))
  print(glue('Cold CL is {cold_cl}'))
  print(glue('Elec CL is {electricity_cl}'))
  print(glue('Acid CL is {acid_cl}'))
  print(glue('Stun CL is {stun_cl}'))

  return(
    ham_cl + armor_cl + dps_cl +
      kin_cl + eng_cl + blast_cl +
      heat_cl + cold_cl + electricity_cl +
      acid_cl + stun_cl
  )
}

creature_cl_individual <- function(health, action, mind, armor, damage_low, damage_high, to_hit, speed, kin, eng, blast, heat, cold, elec, acid, stun) {
  df <- data.frame(
    health = health,
    action = action,
    mind = mind,
    armor = armor,
    damage_low = damage_low,
    damage_high = damage_high,
    to_hit = to_hit,
    speed = speed,
    kinetic = kin,
    energy = eng,
    blast = blast,
    heat = heat,
    cold = cold,
    electricity = elec,
    acid = acid,
    stun = stun
  )

  return(creature_level(df))
}


creature_cl_individual(7521, 5508, 5835, 0, 140, 140, 0.28, 2.14, 30.5, -69.3, -17.7, -69.3, -17.7, -47.4, -69.3, -69.3)
creature_cl_individual(9222, 7674, 4002, 0, 120, 120, 0.22, 2.14, 59.425, -88.605, -6.195, -24.255, -6.195, -80.94, -24.255, -88.605)


creature_cl_individual(6709, 3245, 2957, 0, 80, 90, 0.24, 1.99, 28, 5, 9, -99, -70, -70, -6, -99)


##
normalized_df %>%
  filter(serial == "m4ai12ck") %>%
  creature_level()

normalized_df %>%
  filter(serial == "ke5nbvgo") %>%
  select(predicted_level)

normalized_df <- normalized_df %>%
  rowwise() %>%
  mutate(predicted_level = level_fn(cur_data())) %>%
  ungroup()

ggplot(normalized_df, aes(x = predicted_level, y = level, color = cluster)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  ggtitle("Actual vs Predicted Creature Levels")


