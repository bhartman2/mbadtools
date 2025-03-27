#' gg_residual_plots
#' 
#' ggplot versions of common residual plots meant to mimic the plot.lm() and ggfortify::autoplot.lm() 
#' residual plots.
#'
#' @param .data an lm() regression object
#' @param items a vector of integers from 0 to 7; which of 7 plots to select; default 1:3
#' @return a patchwork array of ggplot objects
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOtzFq9LnQE23CVAUg?e=52crMv}{Residual Plot Examples},
#' \code{\link[ggfortify]{autoplot.lm}}, 
#' \code{\link[ggResidpanel]{resid_panel}} 
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
#' data(freeny, package="datasets")
#' fit = lm(y ~ ., data=freeny)
#' gg_residual_plots(fit)
#' gg_residual_plots(fit, items=4:6)
#'
gg_residual_plots = function(.data, items=1:3) {
  
  fit = .data
  afit = fit %>% broom::augment()
  
  # item 1
  gRF = ggplot2::ggplot(afit,
                    ggplot2::aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed")+
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Residuals vs Fitted",
         x = "Fitted value", y = "Residual")
  
  # item 2
  gQQ = ggplot2::ggplot(afit, 
                        ggplot2::aes(sample = .std.resid)) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line(linetype="dashed") +
    ggplot2::labs(title = "Normal Q-Q",
         x = "Theoretical quantiles", 
         y = "Observed quantiles")
  
  # item 3 
  gSL = ggplot2::ggplot(afit, ggplot2::aes(x = .fitted, 
                                           y = sqrt(abs(.std.resid)))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Scale-Location",
         x = "Fitted value", 
         y = expression(sqrt(abs("Standard Residual")))
    )
  # item 4
  gCD = ggplot2::ggplot(afit,
                        ggplot2::aes(x = seq_along(.cooksd), y = .cooksd)) +
    ggplot2::geom_col() +
    ggplot2::labs(title="Cook's Distance",
         x = "Obs number", 
         y = "Cook's distance")
  
  # item 5
  gRH = ggplot2::ggplot(afit, 
                        ggplot2::aes(x=.hat, y = .std.resid))+
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed")+
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Residuals vs Leverage",
         x = "Leverage", 
         y = "Standardized Residuals")
  
  # item 6
  gCH = ggplot2::ggplot(afit, ggplot2::aes(x = .hat/(1-.hat), y = .cooksd)) +
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
      ggplot2::aes(x=.resid,
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