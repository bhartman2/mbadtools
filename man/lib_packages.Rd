% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lib_packages.R
\name{lib_packages}
\alias{lib_packages}
\title{lib_packages}
\usage{
lib_packages(pkgtxtvector, quiet = FALSE)
}
\arguments{
\item{pkgtxtvector}{character vector of package names}

\item{quiet}{logical suppress startup messages; default = TRUE}
}
\value{
same as \code{\link{library}}
}
\description{
enables packages listed in a character vector to an R script
}
\examples{
pkgs = c("tidyverse","MASS")
lib_packages(pkgs)

}
\seealso{
\code{\link{library}}, \code{\link{suppressPackageStartupMessages}}
}
