#' GrangerPlot
#' 
#' Creates list of plots of Granger test p-values for multiple lags from dataframe 
#' created by `GrangerTestPvals`.
#'
#' @param .data a dataframe object created by `GrangerTestPvals`.
#'
#' @returns a list of two plots for each direction of Granger Causality test
#' @export
#'
#' @examples
#' data(moody, package="datasets")
#' gpv = GrangerTestPvals(moody, ICS, Wgrowth, max.lags=10)
#' GrangerPlot(gpv)
GrangerPlot = function (.data) {
  
  d = .data
  mx = max(d$lag)
  nm = names(d)
  
  # plot 1
  gpd1 = ggplot2::ggplot(d) +
    ggplot2::geom_point(ggplot2::aes(lag, d[,2], color="P-values")) +
    ggplot2::geom_line(ggplot2::aes(lag, d[,2]), linetype="dotted") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept=.05, color="Significance level 95%")) +
    ggplot2::scale_x_continuous(breaks=c(1:mx), 
                                labels=as.character(c(1:mx))) +
    ggplot2::labs(title=nm[2], 
                  x="Lag Order of Granger Test", 
                  y="Significance (p-value)") +
    ggplot2::geom_smooth(ggplot2::aes(lag, d[,2], color="loess fit"),
                         method="loess", formula='y~x', linetype="dotted") +
    ggplot2::scale_color_manual(values=c("blue","black", "red"))+
    ggplot2::theme(legend.position="none")
  
  # plot 2 
  gpd2 = ggplot2::ggplot(.data) +
    ggplot2::geom_point(ggplot2::aes(lag, d[,3], color="P-values")) +
    ggplot2::geom_line(ggplot2::aes(lag, d[,3]), linetype="dotted") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept=.05, color="Significance level 95%")) +
    ggplot2::scale_x_continuous(breaks=c(1:mx), 
                                labels=as.character(c(1:mx))) +
    ggplot2::labs(title=nm[3], 
                  x="Lag Order of Granger test", 
                  y="Significance (p-value)") +
    ggplot2::geom_smooth(ggplot2::aes(lag, d[,3], color="loess fit"),
                         method="loess", formula='y~x', linetype="dotted") +
    ggplot2::scale_color_manual(values=c("blue","black", "red")) +
    ggplot2::theme(legend.position = "bottom")
  
  plots=list(gpd1, gpd2)
  return(plots)
}

#' GrangerTestPvals
#' 
#' Computes p.values for Granger cause test of two time series, in both directions.
#'
#' @param .data dataframe containing two series columns
#' @param s1 series 1 name
#' @param s2 series 2 name
#' @param max.lags integer, maximum number of lags to consider, default =10.
#'
#' @returns data frame with lags and p.values of two granger tests
#' @export
#'
#' @examples
#' data(moody, package="mbadtools")
#' GrangerTestPvals(moody, ICS, Wgrowth)
#' 
GrangerTestPvals = function(.data, s1, s2, max.lags=10) {
  
  df=.data %>% 
    dplyr::select({{s1}}, {{s2}} )

  # s2 causes s1
  g.pvalue=c()
  for (i in 1:max.lags) {
    g.pvalue[i] =  lmtest::grangertest(df, order=i)[2,4]
  }
  
  # s1 causes s2
  g.pvalue2=c()
  for (i in 1:max.lags) {
    g.pvalue2[i] =  lmtest::grangertest(df %>% 
                      dplyr::relocate({{s2}}, .before=1), order=i)[2,4]
  }
  
  n1 = paste0(rlang::enquo(s1), "=G=>", rlang::enquo(s2))
  n2 = paste0(rlang::enquo(s2), "=G=>", rlang::enquo(s1))
  pvnames = c(n1[2],n2[2])
  
  pv = data.frame(lag=1:max.lags, 
                 g.pvalue, 
                 g.pvalue2)
  names(pv)[2] = pvnames[1]
  names(pv)[3] = pvnames[2]
  
  return(pv)
}