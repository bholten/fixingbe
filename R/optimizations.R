library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)

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


library(optim)

# Constraints:
# - Hardiness, Dexterity, Intellect all equal weights
# - Dependability, Endurance equal
# - All resists equal
objective_function <- function(coef, data) {
  predictions <- coef[1] * data$hardiness +
                 coef[1] * data$dexterity +
                 coef[1] * data$intellect +
                 coef[2] * data$fortitude
                 coef[3] * data$dependability +
                 coef[3] * data$endurance +
                 coef[4] * data$cleverness +
                 coef[5] * data$courage +
                 coef[6] * data$power +
                 coef[7] * data$fierceness +
                 coef[8] * data$kinetic +
                 coef[8] * data$energy +
                 coef[8] * data$blast +
                 coef[8] * data$cold +
                 coef[8] * data$heat +
                 coef[8] * data$electricity +
                 coef[8] * data$acid +
                 coef[8] * data$stun +
                 coef[9] * data$quality

  return(sum((data$level - predictions)^2))
}

coefs <- 9
lower_bounds <- rep(0, coefs)
upper_bounds <- rep(Inf, coefs)
initial_coef <- rep(0.000-15, coefs)

optim_results <- optim(initial_coef, objective_function,
                       data = normalized_df,
                       method = "L-BFGS-B",
                       lower = lower_bounds,
                       upper = upper_bounds)

predicted_level <- with(normalized_df,
                        optim_results$par[1] * hardiness +
                        optim_results$par[1] * dexterity +
                        optim_results$par[1] * intellect +
                        optim_results$par[2] * fortitude +
                        optim_results$par[3] * dependability +
                        optim_results$par[3] * endurance +
                        optim_results$par[4] * cleverness +
                        optim_results$par[5] * courage +
                        optim_results$par[6] * power +
                        optim_results$par[7] * fierceness +
                        optim_results$par[8] * kinetic +
                        optim_results$par[8] * energy +
                        optim_results$par[8] * blast +
                        optim_results$par[8] * cold +
                        optim_results$par[8] * heat +
                        optim_results$par[8] * electricity +
                        optim_results$par[8] * acid +
                        optim_results$par[8] * stun +
                        optim_results$par[9] * quality)

normalized_df$predicted_level <- predicted_level
normalized_df$residuals <- normalized_df$level - normalized_df$predicted_level

ggplot(normalized_df, aes(x = predicted_level, y = level)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Linear Model Predicted Level vs Level")

ggplot(normalized_df, aes(x = predicted_level, y = residuals)) +
  geom_point() +
  ggtitle("Linear Model Predicted Level vs Residuals")

