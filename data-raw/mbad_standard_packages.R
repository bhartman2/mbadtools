## code to prepare `mbad_standard_packages` dataset goes here

mbad_standard_packages = c(
  "tidyverse",
  "ggfortify",
  "GGally",
  "skimr",
  "broom",
  "gt",
  "patchwork",
  "car",
  "yardstick",
  "ggResidpanel"
  )

## create dataset in /data folder
usethis::use_data(mbad_standard_packages, overwrite = TRUE)
