#' lib_packages
#' 
#' enables packages listed in a character vector to an R script 
#'
#' @param pkgtxtvector character vector of package names
#' @param quiet logical suppress startup messages; default = TRUE
#'
#' @returns same as \code{\link{library}}
#' @export
#'
#' @seealso \code{\link{library}}, \code{\link{suppressPackageStartupMessages}}
#' @examples
#' pkgs = c("tidyverse","MASS")
#' lib_packages(pkgs)
#' 
lib_packages = function(pkgtxtvector, quiet=FALSE) {
  cat("Loading MBAD packages:\n",pkgtxtvector)
  for (p in 1:length(pkgtxtvector)) {
    x = pkgtxtvector[p]
    if (quiet) suppressPackageStartupMessages(library(x, character.only=T)) else
      library(x, character.only=T)
  }  
}