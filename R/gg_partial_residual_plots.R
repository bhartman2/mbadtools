#' Partial Residual Plots 
#' 
#' Draw partial residual plots with ggplot methods. Automatically plots all driver 
#' partial plots in the model.
#'
#' @param .data an lm object
#'
#' @return a patchwork plot layout containing the partial residual plots.
#' @export
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOtzFq9LnQE23CVAUg?e=52crMv}{Residual Plot Examples},
#' \code{\link[ggfortify]{autoplot.lm}}, 
#' \code{\link[ggResidpanel]{resid_panel}} 
#' 
#' @importFrom magrittr %>%
#' @importFrom broom augment
#'
#' @examples
#' data(freeny, package="datasets")
#' fit = lm(y ~ ., data = freeny)
#' gg_residual_plots(fit)
#' gg_partial_residual_plots(fit)
#' 
gg_partial_residual_plots = function (.data) {
  
  # require(tidyverse)
  # require(ggplot2)
  
  afit = .data %>% broom::augment()
  terms=rlang::parse_exprs(attr(.data$terms, "term.labels"))
  len = length(terms)
  PP = vector("list", length = len)
  for (i in 1:len) {
    PP[[i]] = ggplot2::ggplot(afit,
        ggplot2::aes(x=!!terms[[i]], y=.std.resid)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed") +
      ggplot2::geom_smooth(se=F, method="loess",formula='y~x', span=2/3) +
      labs(title=paste0("Partial Residuals - ",terms[[i]]),
           y = "Standard Residuals")
  }
  
  return( 
    PP %>% patchwork::wrap_plots(ncol=2)
         )
}
