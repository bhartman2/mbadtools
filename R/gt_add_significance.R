#' gt_add_significance
#' 
#' Makes a gt table, and adds colored significance notation, to a data frame containing p_value column. 
#' Useful for glance and tidy output from lm models.
#'
#' @param .data A tidy or glance tibble resulting from an lm model.
#' @param level The significance level selected. default .05
#' @return a gt table with the pvalue column colored where result is significant.
#' @export
#' @examples
#' require(tidyverse);
#' fit = lm(dist~speed, data=cars);
#' fit %>% broom::tidy() %>% add_significance();
#' 
gt_add_significance = function (.data, level=.05, digits=4) {
  
  levels_supported = c(.001, .01, .05, .1)
  colors_supported = c("cyan","lightcyan","lightgreen", "greenyellow")
  
  
  has.p.value = "p.value" %in% names(.data)

  out = .data %>% gt %>% opt_interactive %>% fmt_number(decimals=digits) 
  
  if (!has.p.value) {return(out)}
  else if ( nrow(.data %>% filter(p.value<= level))>0 ) {
    out = out %>% data_color(alpha=0.5,
                   columns=p.value,
                   rows = p.value <= level,
                   palette=colors_supported[match(level, levels_supported)])  
    return(out)
  } 
  else {return (out)}
  
}