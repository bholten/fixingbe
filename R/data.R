###############################################################################
# data.R - Data Loading and Filtering for Creature Level Analysis
#
# This script loads the Furrycat database and applies data quality filters.
# After filtering: 370 creatures (79 armored, 291 unarmored)
#
# For full documentation of filters, see: docs/data_quality_filters.md
# For analysis results, see: docs/creature_level_analysis.md
###############################################################################

library(readr)
library(magrittr)
library(dplyr)

###############################################################################
# EXPLICIT BAD DATA EXCLUSIONS
# These specific serials are excluded due to obvious data errors or anomalies
###############################################################################
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
              "mm8gn9sj",                          # kimogila
              "64v2qh2v", "78mq8sed", "lnk4d7fn",  # health ~ hardiness + dexterity outliers I don't like
              # Data quality issues identified during residual analysis
              "jgh9tjrs",                          # bantha level 9 with health=139 (health/level=15, expected ~420)
              "q4glknvv",                          # kima level 20 with health=965 (health/level=48)
              "5cubijof"                           # dewback level 12 with health=803 (health/level=67)
)

###############################################################################
# LOAD DATA AND APPLY FILTERS
###############################################################################
creatures <- read_csv("data/clean/furrycat/creatures.csv") %>%
  filter(!serial %in% bad_data) %>%
  # -------------------------------------------------------------------------
  # MINIMUM LEVEL FILTERS
  # Creatures at their skin's minimum CL are excluded because they may be
  # artificially capped by the game engine, not the formula.
  # See docs/historical_guide.md for minimum CLs by skin.
  # -------------------------------------------------------------------------
  # CL 2 minimum skins
  filter(!(skin == "angler" & level == 2)) %>%
  filter(!(skin == "bearded_jax" & level == 2)) %>%
  filter(!(skin == "bearded_jax" & level == 3)) %>%
  filter(!(skin == "bearded_jax" & level == 4)) %>%
  filter(!(skin == "boar_wolf" & level == 2)) %>%
  filter(!(skin == "bocatt" & level == 2)) %>%
  filter(!(skin == "choku" & level == 2)) %>%
  filter(!(skin == "durni" & level == 2)) %>%
  filter(!(skin == "durni" & level == 3)) %>%
  filter(!(skin == "durni" & level == 4)) %>%
  filter(!(skin == "eopi" & level == 2)) %>%
  filter(!(skin == "gnort" & level == 2)) %>%
  filter(!(skin == "gnort" & level == 3)) %>%
  filter(!(skin == "gnort" & level == 4)) %>%
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
  # CL 5 minimum skins
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
  # Additional skin minimums discovered during creature level analysis
  filter(!(skin == "bageraset" & level == 10)) %>%
  filter(!(skin == "bantha" & level == 6)) %>%
  filter(!(skin == "bol" & level == 10)) %>%
  filter(!(skin == "bolle_bol" & level == 10)) %>%
  filter(!(skin == "brackaset" & level == 8)) %>%
  filter(!(skin == "carrion_spat" & level == 10)) %>%
  filter(!(skin == "cu_pa" & level == 9)) %>%
  filter(!(skin == "dewback" & level == 6)) %>%
  filter(!(skin == "dune_lizard" & level == 6)) %>%
  filter(!(skin == "durni" & level == 5)) %>%
  filter(!(skin == "durni" & level == 6)) %>%
  filter(!(skin == "falumpaset" & level == 8)) %>%
  filter(!(skin == "gurrcat" & level == 6)) %>%
  filter(!(skin == "gurreck" & level == 7)) %>%
  filter(!(skin == "hermit_spider" & level == 9)) %>%
  filter(!(skin == "huurton" & level == 10)) %>%
  filter(!(skin == "kaadu" & level == 8)) %>%
  filter(!(skin == "kima" & level == 9)) %>%
  filter(!(skin == "slice_hound" & level == 10)) %>%
  filter(!(skin == "squall" & level == 9)) %>%
  filter(!(skin == "vir_vir" & level == 7)) %>%
  # CL 10 minimum skins
  filter(!(skin == "huf_dun" & level == 10)) %>%
  filter(!(skin == "piket" & level == 10)) %>%
  filter(!(skin == "razor_cat" & level == 10)) %>%
  filter(!(skin == "veermok" & level == 10)) %>%
  filter(!(skin == "woolamander" & level == 10)) %>%
  # CL 15 minimum skins
  filter(!(skin == "gronda" & level == 15)) %>%
  filter(!(skin == "kliknik" & level == 15)) %>%
  filter(!(skin == "ronto" & level == 15)) %>%
  filter(!(skin == "snorbal" & level == 15)) %>%
  filter(!(skin == "thune" & level == 15)) %>%
  filter(!(skin == "tyblis" & level == 15)) %>%
  filter(!(skin == "vesp" & level == 15)) %>%
  # CL 20 minimum skins
  filter(!(skin == "malkloc" & level == 20)) %>%
  filter(!(skin == "torton" & level == 20)) %>%
  # CL 25 minimum skins
  filter(!(skin == "graul" & level == 25)) %>%
  filter(!(skin == "merek" & level == 25)) %>%
  filter(!(skin == "sharnaff" & level == 25)) %>%
  # CL 30+ minimum skins
  filter(!(skin == "fambaa" & level == 30)) %>%
  filter(!(skin == "rancor" & level == 35)) %>%
  filter(!(skin == "kimogila" & level == 40))

###############################################################################
# JOIN TEMPLATES AND NORMALIZE
###############################################################################
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
  resist_special <- sym(paste0(resist, ".special"))
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

###############################################################################
# FEATURE ENGINEERING
# Derived variables used in creature level analysis
###############################################################################
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
