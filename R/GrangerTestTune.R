#' GrangerTestTune
#' 
#' Run a Granger Causality test in both directions for one time series versus another, 
#' over a range of different orders
#'
#' @param ts1 time series 
#' @param ts2 time series
#' @param max.lags maximum number of lags to try, default 10
#'
#' @return a list containing two ggplot objects, showing variation in p-value of 
#' Granger causality test in each  direction with number of lags; 
#' can be plotted by new('ggmultiplot') or other means.
#' @export
#' @importFrom rlang enquo ensym
#' @importFrom stats is.ts
#' @importFrom lmtest grangertest
#' @importFrom lemon geom_pointline
#'
#' @examples
#' \dontrun{
#' u = GrangerTestTune(moody$ICS, moody$Wgrowth, max.lags=10)
#' new('ggmultiplot', nrow=2, plots=u)
#' }
#' 
GrangerTestTune = function(ts1, ts2, max.lags=10) {
  
  # require(dplyr)
  # require(ggplot2)
  
  f1=rlang::enquo(ts1)
  ff1=rlang::ensym(ts1) #for where it might be displayed
  f2=rlang::enquo(ts2)
  ff2=rlang::ensym(ts2) #for where it might be displayed
  
  # check class of the arguments
  if(!stats::is.ts(ts1) | !stats::is.ts(ts2))
    stop('Parameter is not of class ts time series')
  if(length(ts1) != length(ts2))
    stop('Time series are not of the same length')
  
  g.pvalue=c()
  for (i in 1:max.lags) {
    g.pvalue[i] =  lmtest::grangertest(ts1~ts2, order=i)[2,4]
  }
  gpd1 = ggplot2::ggplot() +
    lemon::geom_pointline(aes(1:max.lags, g.pvalue, color="p-value"), linetype="dashed", linesize=2) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept=.05, color="Significance level 95%"), 
      size=1.5, linetype="dotted") +
    ggplot2::scale_x_continuous(breaks=c(1:max.lags), labels=as.character(c(1:max.lags))) +
    ggplot2::labs(title=paste0(ff2, " =G=> ", ff1), 
         x="Order of Granger test", y="Significance level (p-value)") +
    ggplot2::geom_smooth(aes(1:max.lags, g.pvalue, color="loess fit"), method="loess", formula='y~x', linetype="dotted") +
    ggplot2::scale_color_manual(values=c("blue","black", "red"), name="Legend")+
    ggplot2::theme(legend.position="none")
  
  g.pvalue2=c()
  for (i in 1:max.lags) {
    g.pvalue2[i] =  lmtest::grangertest(ts2~ts1, order=i)[2,4]
  }
  gpd2 = ggplot2::ggplot() +
    lemon::geom_pointline(aes(1:max.lags, g.pvalue2, color="P-values"), linetype="dashed", linesize=2) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept=.05, color="Significance level 95%"), 
      size=1.5, linetype="dotted") +
    ggplot2::scale_x_continuous(breaks=c(1:max.lags), labels=as.character(c(1:max.lags))) +
    ggplot2::labs(title=paste0(ff1, " =G=> ",ff2), 
         x="Order of Granger test", y="Significance level (p-value)") +
    ggplot2::geom_smooth(
      aes(1:max.lags, g.pvalue2, color="loess fit"), 
      method="loess", formula='y~x', linetype="dotted") +
    ggplot2::scale_color_manual(values=c("blue","black", "red"), name="Legend") +
    ggplot2::theme(legend.position = "bottom")
  
  plots=list(gpd1, gpd2)
  return(plots)
}

#' GrangerTestPvals
#' 
#' Computes p.values for Granger cause test of two time series, in both directions.
#'
#' @param ts1 time series
#' @param ts2 time series
#' @param max.lags integer, maximum number of lags to consider, default =10.
#'
#' @returns data frame
#' @importFrom rlang enquo ensym
#' @importFrom stats is.ts
#' @export
#'
#' @examples
#' \dontrun{
#' GrangerTestPvals(moody$ICS, moody$Wgrowth, max.lags=10)
#' }
GrangerTestPvals = function(ts1, ts2, max.lags=10) {
  
  # require(dplyr)

  # f1=rlang::enquo(ts1)
  # ff1=rlang::ensym(ts1) #for where it might be displayed
  # f2=rlang::enquo(ts2)
  # ff2=rlang::ensym(ts2) #for where it might be displayed

    # check class of the arguments
  if(!stats::is.ts(ts1) | !stats::is.ts(ts2))
    stop('Parameter is not of class ts time series')
  if(length(ts1) != length(ts2))
    stop('Time series are not of the same length')
  
  g.pvalue=c()
  for (i in 1:max.lags) {
    g.pvalue[i] =  lmtest::grangertest(ts1~ts2, order=i)[2,4]
  }
  g.pvalue2=c()
  for (i in 1:max.lags) {
    g.pvalue2[i] =  lmtest::grangertest(ts2~ts1, order=i)[2,4]
  }
  q = data.frame(lag=1:max.lags, p12 = g.pvalue, p21 = g.pvalue2)
  return(q)
}