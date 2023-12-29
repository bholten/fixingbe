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
  filter(!serial %in% bad_data)
templates <- read_csv("data/clean/furrycat/templates.csv")
combined_data <- creatures %>% inner_join(templates, by = join_by(template_id == serial))
normalized_df <- combined_data %>%
  mutate_at(
    vars("kinetic.effective.x",
         "kinetic.special.x",
         "energy.effective.x",
         "energy.special.x",
         "blast.effective.x",
         "blast.special.x",
         "heat.effective.x",
         "heat.special.x",
         "cold.effective.x",
         "cold.special.x",
         "electricity.effective.x",
         "electricity.special.x",
         "acid.effective.x",
         "acid.special.x",
         "stun.effective.x",
         "stun.special.x"),
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
gbm_model <- gbm(
  formula = level ~
    health + action + mind + speed + to_hit + damage_low + damage_high + total_vulns,
    # hardiness +
    # fortitude +
    # dexterity +
    # endurance +
    # intellect +
    # cleverness +
    # courage +
    # dependability +
    # fierceness +
    # power +
    # kinetic.special.x +
    # kinetic.effective.x +
    # energy.special.x +
    # energy.effective.x +
    # blast.special.x +
    # blast.effective.x +
    # heat.special.x +
    # heat.effective.x +
    # cold.special.x +
    # cold.effective.x +
    # electricity.special.x +
    # electricity.effective.x +
    # acid.special.x +
    # acid.effective.x +
    # stun.special.x +
    # stun.effective.x +
    # kinetic.vuln +
    # energy.vuln +
    # blast.vuln +
    # heat.vuln +
    # cold.vuln +
    # electricity.vuln +
    # acid.vuln +
    # stun.vuln +
    # total_vulns,
  data = normalized_df,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 6,
  n.minobsinnode = 5,
  bag.fraction = 0.5,
  cv.folds = 10
  #shrinkage = 0.01
)
#
summary(gbm_model, plotit = FALSE)
#
plot.gbm(gbm_model, i.var = "total_vulns")
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
