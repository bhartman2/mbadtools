#' Partial Residual Plots 
#' 
#' Draw partial residual plots with ggplot methods. Automatically plots all driver 
#' partial plots in the model.
#'
#' @param .data an lm object
#' @param new_data data set model was trained on if class is `_lm`; default NULL
#' @param ... parameters to be passed to `patchwork::wrap_plots()`
#' @return a patchwork plot layout containing the partial residual plots.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom broom augment
#' @importFrom ggplot2 labs
#' @importFrom parsnip augment
#' @examples
#' data(freeny, package="datasets")
#' fit = lm(y ~ ., data = freeny)
#' gg_partial_residual_plots(fit)
#' 
gg_partial_residual_plots = function (.data, new_data = NULL, ...) {
  
  fit = .data
  
  if (inherits(fit, "lm")) {
    
    afit = fit %>% broom::augment()
    terms=rlang::parse_exprs(attr(.data$terms, "term.labels"))
    
  } else if (inherits(fit, "_lm")) { # doesn't work without new_data argument
    
      if(is.null(new_data)) stop("parsnip model must have new_data.")
    
      afit = fit %>% parsnip::augment(new_data = new_data) %>%
      relocate(starts_with("."), .after=last_col()) %>% 
      rename(.fitted = .pred) %>% 
      mutate(.std.resid = scale(.resid))
      terms = colnames(new_data)
      
  } else{
    stop("Unknown class.")
  }
  
  len = length(terms)
  
  PP = vector("list", length = len)
  for (i in 1:len) {
    PP[[i]] = ggplot2::ggplot(afit,
        ggplot2::aes(x=!!terms[[i]], y=.resid)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed") +
      ggplot2::geom_smooth(se=F, method="loess",formula='y~x', span=2/3) +
      labs(title=paste0("Residual vs ",terms[[i]]),
           y = "Residual")
  }
  
  return( 
    PP %>% patchwork::wrap_plots(ncol=2, ...)
         )
}
