#' Partial Residual Plots 
#' 
#' Draw partial residual plots with ggplot methods. Automatically plots all driver 
#' partial plots in the model.
#'
#' @param .data an lm object
#'
#' @return a patchwork plot layout containing the partial residual plots.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom broom augment
#'
#' @examples
#' \dontrun{
#' library(ISLR2);
#' fit = lm(mpg ~ horsepower + weight + acceleration, data=Auto);
#' fit %>%  gg_partial_residual_plots();
#' }
#' 
gg_partial_residual_plots = function (.data) {
  
  # require(tidyverse)
  # require(ggplot2)
  
  afit = .data %>% broom::augment()
  terms=rlang::parse_exprs(attr(.data$terms, "term.labels"))
  len = length(terms)
  PP = vector("list", length = len)
  for (i in 1:len) {
    PP[[i]] = afit %>% 
      ggplot2::ggplot(
        ggplot2::aes(x=!!terms[[i]], y=afit$.std.resid)) +
      geom_point() +
      geom_hline(aes(yintercept=0), linetype="dashed") +
      geom_smooth(se=F, method="loess",formula='y~x', span=2/3) +
      labs(title=paste0("Partial Residuals - ",terms[[i]]),
           y = "Standard Residuals")
  }
  
  return(patchwork::wrap_plots(list(PP), ncol=2))
}
