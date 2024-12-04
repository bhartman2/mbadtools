#' gg_residual_plots
#' 
#' ggplot versions of common residual plots meant to mimic the plot.lm() and ggfortify::autoplot.lm() 
#' residual plots.
#'
#' @param .data an lm() regression object
#' @param items a vector of integers from 1 to 7; which of 7 plots to select; default 1:3
#' @return a patchwork array of ggplot objects
#' @export
#' @examples
#' require(tidyverse)
#' data(cars)
#' fit = lm(dist~speed, data=cars)
#' fit %>% gg_residual_plots()
#' fit %>% gg_residual_plots(4:6)

gg_residual_plots = function(.data, items=1:3) {
  
  require(tidyverse)
  require(broom)
  require(patchwork)
  require(GGally)
  
  fit = .data
  afit = fit %>% augment()
  
  # Currently you cannot change which items are plotted.
  
  # item 1
  gRF = afit %>% 
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_smooth(method="loess", se = FALSE, formula='y~x') +
    labs(title="Residuals vs Fitted",
         x = "Fitted value", y = "Residual")
  # item 2
  gQQ = afit %>% 
    ggplot(aes(sample = .std.resid)) +
    geom_qq() +
    geom_qq_line(linetype="dashed") +
    labs(title = "Normal Q-Q",
         x = "Theoretical quantiles", 
         y = "Observed quantiles")
  # item 3 
  gSL = afit %>% 
    ggplot(aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
    geom_point() +
    geom_smooth(method="loess", se = FALSE, formula='y~x') +
    labs(title="Scale-Location",
         x = "Fitted value", 
         y = expression(sqrt(abs("Standard Residual")))
    )
  # item 4
  gCD = afit %>% 
    ggplot(aes(x = seq_along(.cooksd), y = .cooksd)) +
    geom_col() +
    labs(title="Cook's Distance",
         x = "Obs number", 
         y = "Cook's distance")
  # item 5
  gRH = afit %>% 
    ggplot(aes(x=.hat, y = .std.resid))+
    geom_point() +
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_smooth(method="loess", se = FALSE, formula='y~x') +
    labs(title="Residuals vs Leverage",
         x = "Leverage", 
         y = "Standardized Residuals")
  
  # item 6
  gCH = afit %>% 
    ggplot(aes(x = .hat/(1-.hat), y = .cooksd)) +
    geom_point() +
    labs(title="Cook's Distance vs Leverage/(1-Leverage)",
         x = "Leverage/(1-Leverage)", 
         y = "Cook's distance")
  lapply(0:7, 
         function(i) gCH <<- gCH + 
           geom_abline(aes(slope=i, intercept=0), linetype="dashed") )
  
  # item 7
  gHI = afit %>% ggplot() +
    geom_histogram(aes(x=.resid,y=after_stat(density)), fill="lightgray", color="black") +
    geom_function(fun = dnorm, 
                  args = list(mean = mean(afit$.resid), 
                              sd = sd(afit$.resid)),
                  linetype="dashed") +
    labs(title="Histogram of Residuals",
         x="Residuals",
         y="Density")
  
  # item 0
  gB = GGally::ggally_blank()
  
  # list
  P = list(gRF, gQQ, gSL, gCD, gRH, gCH, gHI)
  
  # layout 
  return(
    P[items] %>% wrap_plots(ncol=2, nrow=3)
  )
  
}