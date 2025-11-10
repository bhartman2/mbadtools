## code to prepare `myresids` dataset goes here

## This script creates a dataset named `myresids` that contains legal residual plots for `ggResidpanel::resid_panel`

myresids = c("resid", "qq", "ls","hist")

## create dataset in /data folder
usethis::use_data(myresids, overwrite = TRUE)
