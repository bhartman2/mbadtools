#' GrangerTestTune
#' 
#' Run a Granger Causality test in both directions for one time series versus another, over a range of different orders
#'
#' @description Use this function two time series that were taken at the same times.
#' 
#' @param ts1 time series 
#' @param ts2 time series
#' @param max.lags maximum number of lags to try, default 10
#'
#' @return a list containing two ggplot objects, showing variation in p-value of Granger causality test in each  directionwith number of lags; can be plotted by new('ggmultiplot') or other means.
#' @export
#'
#' @examples
#' \dontrun{
#' data(EuStockMarkets, package="datasets")
#' # extract two time series
#' DA = EuStockMarkets[,"DAX"] 
#' FT = EuStockMarkets[,"FTSE"]
#' # plot the two plots with patchwork
#' GrangerTestTune(DA, FT) %>% patchwork::wrap_plots(nrow=2) 
#' }
#' 
GrangerTestTune = function(ts1, ts2, max.lags=10) {
  
  f1=rlang::enquo(ts1)
  ff1=rlang::ensym(ts1) #for where it might be displayed
  f2=rlang::enquo(ts2)
  ff2=rlang::ensym(ts2) #for where it might be displayed
  
  # check class of the arguments
  if( !stats::is.ts(ts1) | !stats::is.ts(ts2) != "ts")
    stop('Parameter is not of class ts time series')
  if(length(ts1) != length(ts2))
    stop('Time series are not of the same length')
  
  g.pvalue=c()
  for (i in 1:max.lags) {
    g.pvalue[i] =  lmtest::grangertest(ts1~ts2, order=i)[2,4]
  }
  gpd1 =ggplot() +
    ggh4x::geom_pointpath(aes(1:max.lags, g.pvalue, color="p-value"), 
                          linewidth=1.2) +
    geom_hline(aes(yintercept=.05, color="Significance level 95%"), size=1.5, 
               linetype="dotted") +
    scale_x_continuous(breaks=c(1:max.lags), labels=as.character(c(1:max.lags))) +
    labs(title=paste0(ff2, " =G=> ", ff1), 
         x="Order of Granger test", y="Significance level (p-value)") +
    geom_smooth(aes(1:max.lags, g.pvalue, color="loess fit"), 
                method="loess", formula='y~x', linewidth=.7) +
    scale_color_manual(values=c("blue","black", "red"), name="Legend")+
    theme(legend.position="none")
  
  g.pvalue2=c()
  for (i in 1:max.lags) {
    g.pvalue2[i] =  grangertest(ts2~ts1, order=i)[2,4]
  }
  gpd2 =ggplot() +
    ggh4x::geom_pointpath(aes(1:max.lags, g.pvalue2, color="P-values"), 
                          linewidth=1.2) +
    geom_hline(aes(yintercept=.05, color="Significance level 95%"), size=1.5, linetype="dotted") +
    scale_x_continuous(breaks=c(1:max.lags), labels=as.character(c(1:max.lags))) +
    labs(title=paste0(ff1, " =G=> ",ff2), 
         x="Order of Granger test", y="Significance level (p-value)") +
    geom_smooth(aes(1:max.lags, g.pvalue2, color="loess fit"), 
                method="loess", formula='y~x', linewidth=.7) +
    scale_color_manual(values=c("blue","black", "red"), name="Legend") +
    theme(legend.position = "bottom")
  
  plots=list(gpd1, gpd2)
  return(plots)
}

#' GrangerTestPvals
#' 
#' Creates a data frame of p-values for bi-directional Granger Causality test for 
#'   multiple lags, using `lmtest::grangertest`. 
#' 
#' @description Use this function for two time series that were taken at the same times.
#'
#' @param ts1 time series of type ts
#' @param ts2 time series of type ts
#' @param max.lags number of lags to consider, default = 10
#'
#' @returns data frame with max.lags rows and three columns: 
#'   lag, the lag number, p12, the p-value for ts1->ts2, and p21, the reverse. 
#' @export
#'
#' @examples
#' data(EuStockMarkets, package="datasets")
#' # extract two time series
#' DAX = EuStockMarkets[,"DAX"] 
#' FTSE = EuStockMarkets[,"FTSE"]
#' # create the p-values table for the two directions 
#' GrangerTestPvals(DAX, FTSE)
#' 
GrangerTestPvals = function(ts1, ts2, max.lags=10) {
  
  f1=rlang::enquo(ts1)
  ff1=rlang::ensym(ts1) #for where it might be displayed
  f2=rlang::enquo(ts2)
  ff2=rlang::ensym(ts2) #for where it might be displayed
  
  # check class of the arguments
  if( !stats::is.ts(ts1)| !stats::is.ts(ts2) ) 
    print('Parameter is not of class ts time series; trying to make them.')
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