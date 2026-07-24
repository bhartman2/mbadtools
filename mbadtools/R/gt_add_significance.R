#' gt_add_significance
#' 
#' Makes a gt table, and adds colored significance notation, to a data frame containing `p.value` column. 
#' Useful for glance and tidy output from lm models.
#'
#' @param .data A tidy or glance tibble resulting from an lm model.
#' @param level The significance level selected. default = .05
#' @param digits number of digits to retain in numeric columns; default = 4
#' @return a gt table with the pvalue column colored where result is significant.
#' @export
#' @examples
#' \dontrun{
#' data(cars)
#' fit = lm(dist~speed, data=cars);
#' gt_add_significance(broom::tidy(fit))
#' }
#' 
gt_add_significance = function (.data, level=.05, digits=4) {
  
  D = .data

  levels_supported = c(.001, .01, .05, .1)
  colors_supported = c("cyan","lightcyan","lightgreen", "greenyellow")
  
  
  has.p.value = "p.value" %in% names(D)

  out = D %>% gt::gt() %>% gt::fmt_number(decimals=digits) 
  
  if (!has.p.value) { return(out) }
  else if ( nrow(D %>% dplyr::filter(p.value<= level)) > 0 ) {
    out = out %>% gt::data_color(alpha=0.5,
                   columns=p.value,
                   rows = p.value <= level,
                   palette=colors_supported[match(level, levels_supported)])  
    return(out)
  } 
  else {return (out)}
  
}