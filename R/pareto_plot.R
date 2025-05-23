#' pareto_arrange_data
#' 
#' Arrange compute data for a pareto plot
#'
#' @param .data data frame with a category variable and a count column,
#'    often produced with `dplyr::summarize`
#' @param countcol name of the column with the counts, no quotes required
#'
#' @returns a dataframe with the category and count columns plus the Cumulative and Percentage columns.
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOt0X0A8jVrshpaYQg?e=01d2ZT}{Pareto Plot Examples},
#' \code{\link[qcc]{pareto.chart}}
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate summarize group_by
#' @examples
#' data(chickwts, package="datasets")
#' K = chickwts %>% group_by(feed) %>% summarize(avwt = mean(weight))
#' K1 = pareto_arrange_data(K, avwt)
#' print(K1)
#' 
pareto_arrange_data = function (.data, countcol) {
  
  # require(tidyverse)
  
  df = .data
  df1 <- df %>%
    dplyr::arrange(dplyr::desc({{countcol}})) 
  df2 = df1 %>%
    mutate(Cumulative = cumsum({{countcol}}), # Cumulative sum
           Percentage = Cumulative / sum({{countcol}}) * 100) # Cumulative percentage
  return(df2)
}

#' pareto_plot
#' 
#' Make a pareto plot from data arranged with `pareto_arrange`
#'
#' @param .data a pareto_arranged data frame
#' @param category name of the category column
#' @param count name of the count column
#'
#' @returns a ggplot object
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOt0X0A8jVrshpaYQg?e=01d2ZT}{Pareto Plot Examples},
#' \code{\link[qcc]{pareto.chart}}
#' @export
#' @examples
#' data(chickwts, package="datasets")
#' K = chickwts %>% group_by(feed) %>% summarize(avwt = mean(weight))
#' K1 = pareto_arrange_data(K, avwt)
#' pareto_plot(K1, feed, avwt)
#' 
pareto_plot = function (.data, category, count) {
  
  # require(tidyverse)

  df = .data
  # Create the Pareto chart
  P = ggplot2::ggplot(df, 
                      ggplot2::aes(
                        x = stats::reorder({{category}}, -{{count}}), 
                        y = {{count}})) +
    # Bar chart for frequencies
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    # Points for cum%
    ggplot2::geom_point(ggplot2::aes(y = Percentage*max({{count}})/100), 
               color = "red", size = 2) + 
    # Line for cum%
    ggplot2::geom_line(ggplot2::aes(y = Percentage*max({{count}})/100, group = 1), 
              color = "red", linewidth = 1)
  
  return(P)
}

#' pareto_sec_axis
#'
#' @param maxcount numeric, maximum value of the count column
#'
#' @returns a ggplot layer 
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOt0X0A8jVrshpaYQg?e=01d2ZT}{Pareto Plot Examples},
#' \code{\link[qcc]{pareto.chart}}
#'
#' @export
#' @examples
#' data(chickwts, package="datasets")
#' K = chickwts %>% group_by(feed) %>% summarize(avwt = mean(weight))
#' K1 = pareto_arrange_data(K, avwt)
#' pareto_plot(K1, feed, avwt) +
#'   pareto_sec_axis(max(K1$avwt))
#' 
pareto_sec_axis = function(maxcount) {
  
  # require(tidyverse)
  
  list(
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(~ ., 
                          name = "Cumulative Pct (%)",
                          breaks = maxcount*c(20,40,60,80,100)/100,
                          labels=sapply(as.character(c(20,40,60,80,100)),
                                        FUN = function (x) paste0(x,"%")
                          )
      )
    ) 
    
  )
  
}

#' pareto_pct_labels
#' 
#' Add percent labels on cumulative percent line. Use additional parameters
#' to size and position the labels on the plot. Data object must have a Percentage column.
#'
#' @param maxcount maximum count
#' @param ... additional parameters to `geom_text`
#'
#' @returns added ggplot layer
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOt0X0A8jVrshpaYQg?e=01d2ZT}{Pareto Plot Examples},
#' \code{\link[qcc]{pareto.chart}}
#' @export
#'
#' @examples
#' data(chickwts, package="datasets")
#' K = chickwts %>% group_by(feed) %>% summarize(avwt = mean(weight))
#' K1 = pareto_arrange_data(K, avwt)
#' pareto_plot(K1, feed, avwt) +
#'   pareto_pct_labels(max(K1$avwt), nudge_y=1, size=3)
#'   
pareto_pct_labels = function(maxcount,...) {
  
  # require(tidyverse)
  
  list (
    # add percentage labels to line
    ggplot2::geom_text(ggplot2::aes(y=Percentage*maxcount/100, 
                  label = paste0(round(Percentage,1),"%")),
                  ...)
  )
    
}

#' pareto_cutoff_line
#' 
#' Puts a cutoff line on a pareto_plot
#'
#' @param maxcount maximum count
#' @param percent cutoff percent, default=80
#'
#' @returns added ggplot layer with the Pareto percentage line
#' @seealso \href{https://1drv.ms/u/s!AvpG0vuEDBBQoOt0X0A8jVrshpaYQg?e=01d2ZT}{Pareto Plot Examples},
#' \code{\link[qcc]{pareto.chart}}
#' @export
#'
#' @examples
#' data(chickwts, package="datasets")
#' K = chickwts %>% group_by(feed) %>% summarize(avwt = mean(weight))
#' K1 = pareto_arrange_data(K, avwt)
#' pareto_plot(K1, feed, avwt) +
#'   pareto_cutoff_line(max(K1$avwt))
#'
pareto_cutoff_line = function (maxcount, percent=80) {
  
  # require(tidyverse)
  
  list (
    
    ggplot2::geom_hline(yintercept = percent*maxcount/100, linetype="dotted")
    
  )

}