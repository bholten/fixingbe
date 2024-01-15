required_packages <- c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "rmarkdown",
  "readr",
  "magrittr",
  "gbm",
  "tidyr",
  "mgcv",
  "broom",
  "segmented",
  "lmtest"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
