#' mbadtools: Tools for ...
#'
#' Description of the package.
#'
#' See the package README:
#' \url{https://github.com/bhartman2/mbadtools#readme}
#' 
#' @importFrom car residualPlots
#' @importFrom ggfortify ggfreqplot
#' @importFrom ggh4x geom_pointpath
#' @importFrom gt gt
#' @importFrom skimr skim
#' @importFrom tidymodels tidymodels_packages
#' @importFrom tidyverse tidyverse_packages
#' @importFrom workflowsets workflow_set
#' @importFrom bvartools bvar
#' @importFrom vars VAR
#'
#' @keywords internal
"_PACKAGE"
# Declare global variables to prevent R CMD check NOTEs
utils::globalVariables(c(".cooksd", ".fitted", ".hat", ".lower", ".pred", 
                         ".resid", ".std.resid", ".upper", "Cumulative", 
                         "Percentage", "Period", "impulse", "p.value", "vard"))