source("R/data.R")

library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(gbm)
library(mgcv)
library(broom)
library(cluster)
library(tidyr)
library(segmented)
library(dplyr)


normalized_df %>% dplyr::select(serial, level, skin, average_res) %>% View()

model <- lm(level ~ average_res, data = normalized_df)
summary(model)


segmented.model <- segmented(model, seg.Z = ~average_res, psi=-50)
summary(segmented.model)
