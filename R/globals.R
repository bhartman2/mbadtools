# globals.R
#' @import pdp
#' @import pre
#' @import vip
utils::globalVariables("r.squared")
utils::globalVariables(c(
  "Variable", 
  "Importance", 
  "skim_variable", 
  "numeric.mean", 
  "numeric.sd"
))