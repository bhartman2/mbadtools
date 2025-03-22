#' gg_residual_plots
#' 
#' ggplot versions of common residual plots meant to mimic the plot.lm() and ggfortify::autoplot.lm() 
#' residual plots.
#'
#' @param .data an lm() regression object
#' @param items a vector of integers from 0 to 7; which of 7 plots to select; default 1:3
#' @return a patchwork array of ggplot objects
#' 
#' @importFrom ggplot2 geom_point geom_hline geom_smooth labs geom_col 
#'             geom_qq aes geom_abline geom_histogram geom_function
#'             geom_qq_line
#'
#' @importFrom magrittr %>%
#' @importFrom broom augment
#' @importFrom stats dnorm density sd filter
#' 
#' @export
#' @examples
#' data(cars)
#' fit = lm(dist~speed, data=cars)
#' gg_residual_plots(fit, items=4:6)

gg_residual_plots = function(.data, items=1:3) {
  
  fit = .data
  afit = fit %>% broom::augment()
  
  # item 1
  gRF = ggplot2::ggplot(afit,
                    ggplot2::aes(x = afit$.fitted, y = afit$.resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed")+
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Residuals vs Fitted",
         x = "Fitted value", y = "Residual")
  
  # item 2
  gQQ = ggplot2::ggplot(afit, 
                        ggplot2::aes(sample = afit$.std.resid)) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line(linetype="dashed") +
    ggplot2::labs(title = "Normal Q-Q",
         x = "Theoretical quantiles", 
         y = "Observed quantiles")
  
  # item 3 
  gSL = ggplot2::ggplot(afit, ggplot2::aes(x = afit$.fitted, 
                                           y = sqrt(abs(afit$.std.resid)))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Scale-Location",
         x = "Fitted value", 
         y = expression(sqrt(abs("Standard Residual")))
    )
  # item 4
  gCD = ggplot2::ggplot(afit,
                        ggplot2::aes(x = seq_along(afit$.cooksd), y = afit$.cooksd)) +
    ggplot2::geom_col() +
    ggplot2::labs(title="Cook's Distance",
         x = "Obs number", 
         y = "Cook's distance")
  
  # item 5
  gRH = ggplot2::ggplot(afit, 
                        ggplot2::aes(x=afit$.hat, y = afit$.std.resid))+
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed")+
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Residuals vs Leverage",
         x = "Leverage", 
         y = "Standardized Residuals")
  
  # item 6
  gCH = ggplot2::ggplot(afit, aes(x = afit$.hat/(1-afit$.hat), y = afit$.cooksd)) +
    ggplot2::geom_point() +
    ggplot2::labs(title="Cook's Distance vs Leverage/(1-Leverage)",
         x = "Leverage/(1-Leverage)", 
         y = "Cook's distance")
  
  lapply(0:7, 
         function(i) gCH <<- gCH + 
           ggplot2::geom_abline(aes(slope=i, intercept=0), linetype="dashed") )
  
  # item 7
  gHI =  ggplot2::ggplot(afit) +
    ggplot2::geom_histogram(
      ggplot2::aes(x=afit$.resid,
                   y=ggplot2::after_stat(density)), 
      fill="lightgray", color="black") +
    ggplot2::geom_function(fun = dnorm, 
                  args = list(mean = mean(afit$.resid), 
                              sd = sd(afit$.resid)),
                  linetype="dashed") +
    ggplot2::labs(title="Histogram of Residuals",
         x="Residuals",
         y="Density")
  
  # item 0
  gB = GGally::ggally_blank()
  
  # list
  P = list(gRF, gQQ, gSL, gCD, gRH, gCH, gHI)
  
  # layout 
  return(
    P[items] %>% patchwork::wrap_plots(ncol=2)
  )
  
}