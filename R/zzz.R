# List the packages you want to attach attached when your meta-package loads
core_packages <- c(
  "tidyverse", "ggfortify", "GGally", "skimr",
  "broom", "gt", "patchwork", "car",
  "yardstick", "gglm", "ggh4x", "ggpubr"
)

# Function to check which packages are already loaded
#' core_unloaded
#'
#' @returns a list of packages that have not been loaded yet
#' @export
#'
#' @examples
#' \dontrun{
#' }
core_unloaded <- function() {
  search <- search()
  ins <- paste0("package:", core_packages)
  core_packages[!ins %in% search]
}

# This function runs automatically whenever the package is loaded via library()
#' .onAttach
#'
#' @param libname character, name of library
#' @param pkgname character, name of package
#'
#' @returns package startup message, list of newly loaded packages in printable grid
#' @export
#'
#' @examples
#' \dontrun{
#' }
.onAttach <- function(libname, pkgname) {
  needed <- core_unloaded()

  if (length(needed) == 0) return()

  # Create a clean header message
  packageStartupMessage(paste0("Loading mbadtools packages: "))

  # Gently load each package silently
  for (pkg in needed) {
    library(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
  }

  # Format a nice printable grid of the loaded packages
  # (Simulating the tidyverse startup look)
  packageStartupMessage(paste(needed, collapse = "  "))
}
