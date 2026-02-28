#' GrangerPlot
#' 
#' Creates list of plots of Granger test p-values for multiple lags from dataframe 
#' created by `GrangerTable`.  Use this function on a data frame if the data column observations 
#' were taken at the same times
#'    
#' @param .data a dataframe object created by `GrangerTable`.
#'
#' @returns a list of two plots for each direction of Granger Causality test
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOtyhT7YLcuCOMiy1A?e=LMf1Xa}{Examples},
#' \code{\link[lmtest]{grangertest}}
#' 
#' @examples
#' \dontrun{
#' data(moody, package="mbadtools")
#' moody %>% GrangerTable(ICS, Wgrowth) %>% GrangerPlot() %>% patchwork::wrap_plots(nrow=2)
#' }
#' @export
#' 
GrangerPlot = function (.data) {
  
  d = .data
  mx = max(d$lag)
  nm = names(d)
  
  # plot 1
  gpd1 = ggplot2::ggplot(d) +
    ggh4x::geom_pointpath(ggplot2::aes(lag, d[,2], color="P-values"),
                          linewidth=1.2) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept=.05, 
                   color="Significance level 95%"), 
      linewidth=1.5, linetype="dotted") +
    ggplot2::scale_x_continuous(breaks=c(1:mx), 
                                labels=as.character(c(1:mx))) +
    ggplot2::labs(title=nm[2], 
                  x="Lag Order of Granger Test", 
                  y="Significance (p-value)") +
    ggplot2::geom_smooth(ggplot2::aes(lag, d[,2], color="loess fit"),
                         method="loess", formula='y~x', linewidth=.7) +
    ggplot2::scale_color_manual(values=c("blue","black", "red"))+
    ggplot2::theme(legend.position="none")
  
  # plot 2 
  gpd2 = ggplot2::ggplot(.data) +
    ggh4x::geom_pointpath(ggplot2::aes(lag, d[,3], color="P-values"),
                          linewidth=1.2) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept=.05, 
                   color="Significance level 95%"),
      linewidth=1.5, linetype="dotted") +
    ggplot2::scale_x_continuous(breaks=c(1:mx), 
                                labels=as.character(c(1:mx))) +
    ggplot2::labs(title=nm[3], 
                  x="Lag Order of Granger test", 
                  y="Significance (p-value)") +
    ggplot2::geom_smooth(ggplot2::aes(lag, d[,3], color="loess fit"),
                         method="loess", formula='y~x', linewidth=.7) +
    ggplot2::scale_color_manual(values=c("blue","black", "red")) +
    ggplot2::theme(legend.position = "bottom")
  
  plots=list(gpd1, gpd2)
  return(plots)
}

#' GrangerTable
#' 
#' Computes p.values for Granger cause test of two time series, in both directions. Can
#'   be supplied as input to `mbadtools::GrangerPlot()`. Use this if the data is in 
#'   a dataframe and you are sure the two series observations are taken at the same times.
#'
#' @param .data dataframe containing two numeric columns
#' @param s1 series 1 numeric column name
#' @param s2 series 2 numeric column name
#' @param max.lags integer, maximum number of lags to consider, default =10.
#'
#' @returns data frame with lags and p.values of two granger tests
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOtyhT7YLcuCOMiy1A?e=LMf1Xa}{Examples},
#' \code{\link[lmtest]{grangertest}}
#' @examples
#' data(moody, package="mbadtools")
#' moody %>% GrangerTable(ICS, Wgrowth)
#' @export
#' 
GrangerTable = function(.data, s1, s2, max.lags=10) {
  
  df=.data %>% 
    dplyr::select( {{s1}}, {{s2}} )

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