#' Partial Residual Plots 
#' 
#' Draw partial residual plots with ggplot methods. Automatically plots all driver partial plots in the model.
#'
#' @param .data an lm object
#'
#' @return a patchwork plot layout containing the partial residual plots.
#' @export
#'
#' @examples
#' fit = lm(mpg ~ horsepower + weight + acceleration, data=Auto)
#' fit %>%  gg_partial_residual_plots()
#' 
gg_partial_residual_plots = function (.data) {
  
  require(tidyverse)
  require(broom)
  require(rlang)
  require(patchwork)
  
  afit = .data %>% augment()
  terms=rlang::parse_exprs(attr(.data$terms, "term.labels"))
  len = length(terms)
  PP = vector("list", length = len)
  for (i in 1:len) {
    # ufit = update(.data, .~.-!!terms[[i]])
    PP[[i]] = afit %>% ggplot(aes(x=!!terms[[i]], y=.std.resid)) +
      geom_point() +
      geom_hline(aes(yintercept=0), linetype="dashed") +
      geom_smooth(se=F, method="loess",formula='y~x', span=2/3) +
      labs(title=paste0("Partial Residuals - ",terms[[i]]),
           y = "Standard Residuals")
  }
  
  return(wrap_plots(PP, ncol=2))
}