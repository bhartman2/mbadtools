#' gg_residual_plots
#' 
#' ggplot versions of common residual plots meant to mimic the `plot.lm` and `ggfortify::autoplot.lm` 
#' residual plots. The 10 plots are:
#' 
#' 1. residual vs fitted (RF) "rf"
#' 2. Actual vs Theoretical Quantile plot (QQ) "qq"
#' 3. Location vs scale (sqrt(abs(standardized residuals)) plot (LS) "ls"
#' 4. Observation vs Cook's Distance (CD) "cookd"
#' 5. Standardized Residuals vs Leverage (RH) "lev"
#' 6. Cook's Distance vs Leverage/(1-Leverage) (CH) "cookl"
#' 7. fitted values histogram (HI)
#' 8. y vs predicted or fitted (YP) "yvp"
#' 9. Tukey test of curvature (TT) "Tukey test"
#' 10. Index vs leverage (.hat values) (IH)
#' 0. Blank (B) "bl"
#' 
#' 
#'
#' @param .data an lm() regression object
#' @param items a vector of integers from 0 to 10; which of 10 plots to select;
#'  default c(1:3,7)
#' @param new_data data set model was trained on if class is `_lm`; default NULL
#' @param ... parameters to be passed to `patchwork::wrap_plots()`
#' @return a patchwork array of ggplot objects
#' 
#' @importFrom ggplot2 geom_point geom_hline geom_smooth labs geom_col geom_qq aes geom_abline geom_histogram geom_function geom_qq_line
#' @importFrom magrittr %>% 
#' @importFrom broom augment
#' @importFrom parsnip augment
#' @importFrom stats dnorm density sd filter
#' @importFrom ggplot2 aes
#' @importFrom patchwork wrap_plots
#' @importFrom GGally ggally_blank
#' @importFrom car residualPlots
#' 
#' @export
#' @examples
#' data(freeny, package="datasets")
#' fit = lm(y ~ ., data=freeny)
#' gg_residual_plots(fit)
#'
gg_residual_plots = function(.data, items=c(1:3,7), new_data=NULL, ...) {
  
  fit = .data
  
  if (inherits(fit, "lm")) {
    afit = fit %>% broom::augment()
  } else if (inherits(fit, "_lm")) { # doesnt work without new_data argument
    if(is.null(new_data)) stop("parsnip model must have new_data.")
    afit = fit %>% parsnip::augment(new_data = new_data) %>%
      relocate(starts_with("."), .after=last_col()) %>% 
      rename(.fitted = .pred) %>% 
      mutate(.std.resid = scale(.resid))
  } else{
    stop("Unknown class.")
  }
  
  # item 1
  gRF = ggplot2::ggplot(afit, ggplot2::aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed")+
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
  # gRF = ggplot2::ggplot(afit,
  #                   ggplot2::aes(x = .fitted, y = .resid)) +
  #   ggplot2::geom_point() +
  #   ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed")+
  #   ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
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
  
  # item 3 "ls"
  gSL = ggplot2::ggplot(afit, 
                        ggplot2::aes(x = .fitted, 
                                     y = sqrt(abs(.std.resid)))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Scale-Location",
         x = "Fitted value", 
         y = expression(sqrt(abs("Standard Residual")))
    )
  # item 4 "cookd"
  gCD = ggplot2::ggplot(afit,
                        ggplot2::aes(x = seq_along(.cooksd), y = .cooksd)) +
    ggplot2::geom_col() +
    ggplot2::labs(title="Cook's Distance",
         x = "Obs number", 
         y = "Cook's distance")
  
  # item 5 "lev"
  gRH = ggplot2::ggplot(afit, 
                        ggplot2::aes(x=.hat, y = .std.resid))+
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype="dashed")+
    ggplot2::geom_smooth(method="loess", se = FALSE, formula='y~x') +
    ggplot2::labs(title="Residuals vs Leverage",
         x = "Leverage", 
         y = "Standardized Residuals")
  
  # item 6 "cookl"
  gCH = ggplot2::ggplot(afit, ggplot2::aes(x = .hat/(1-.hat), y = .cooksd)) +
    ggplot2::geom_point() +
    ggplot2::labs(title="Cook's Distance vs Leverage/(1-Leverage)",
         x = "Leverage/(1-Leverage)", 
         y = "Cook's distance")
  
  lapply(0:7, 
         function(i) gCH <<- gCH + 
           ggplot2::geom_abline(aes(slope=i, intercept=0), linetype="dashed") )
  
  # item 7 "hist"
  gHI =  ggplot2::ggplot(afit) +
    ggplot2::geom_histogram(
      ggplot2::aes(x=.std.resid,
                   y=ggplot2::after_stat(density)), 
      fill="lightgray", color="black", bins=30) +
    ggplot2::geom_function(fun = dnorm, 
                  args = list(mean = 0, 
                              sd = 1),
                  linetype="dashed") +
    ggplot2::labs(title="Histogram of Residuals",
         x="Residuals",
         y="Density")
  
  # item 0 "bl"
  gB = GGally::ggally_blank()
  
  # item 8 "yvp"
  nm = ifelse(colnames(afit)[1] == ".rownames", 
              colnames(afit)[2], 
              colnames(afit)[1])
  response = dplyr::pull(afit,nm)
  fitted = dplyr::pull(afit, .fitted)
  gYP =  ggplot2::ggplot(afit) +
    ggplot2::geom_point(
      ggplot2::aes(x=response, y=.fitted) ) +
    ggplot2::geom_abline(ggplot2::aes(slope=1, intercept=0),
                         color="red") +
    ggplot2::labs(title=paste0(nm," vs Fitted"),
                  x=nm,
                  y="Fitted")
  
  
  # item 9 "Tukey test"
  gTT = GGally::ggally_blank()
  if(9 %in% items) {
    J = Tukey_test(.data)
    gTT = patchwork::wrap_table(J)
  }

  # item 10 ind index vs hat "hat"
  afiti = dplyr::mutate(afit, ind = dplyr::row_number())
  maxhats = dplyr::slice(dplyr::arrange(afiti, dplyr::desc(.hat)), 5)
  gIH = ggplot2::ggplot(afiti, ggplot2::aes(x = ind, y = .hat)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(data=maxhats, 
      aes(x=index, y=.hat, label = ind)) +
    ggplot2::labs(title="Index vs Leverage",
         x = "Index", 
         y = "Leverage (.hat)")
   
  # list
  P = list(gRF, gQQ, gSL, gCD, gRH, gCH, gHI, gYP, gTT, gIH)
  
  # layout 
  return(
    patchwork::wrap_plots(plots=P[items], ...)
  )
  
}

#' Tukey_test
#' 
#' Tukey test of curvature in linear regression models
#'
#' @param .data an lm() regression object
#' @param fmt character c("gt", "summary"), default "gt"
#' @return a list;
#'      summary a data frame with the variables, statistics and p-values
#'      gt a gt table with the same
#' 
#' @importFrom car residualPlots
#' @importFrom rlang .data
#'  
#' @export
#' @examples
#' fit = lm(mpg~hp, data=mtcars)
#' Tukey_test(fit, fmt = "summary")
#' Tukey_test(fit)
#'
Tukey_test = function (.data, fmt = c("gt", "summary")) {

  fmt = match.arg(fmt)

  # Force the model call to contain the actual dataframe, not just the name
  # This prevents stats::update() from searching the environment for the data
  if (!is.null(.data$model)) {
    .data$call$data <- .data$model 
  } else {
    # Fallback if the user ran lm(..., model = FALSE)
    .data$call$data <- stats::model.frame(.data)
  }
  
  TT = tryCatch({
    temp = data.frame(car::residualPlots(.data, plot=FALSE, tests=FALSE))
    temp = tibble::rownames_to_column(temp, var="Variable")
    dplyr::rename(temp, p.value = .data$`Pr...Test.stat..`)
  },
   error = function (e) {
    data.frame(Message = "Model not supported")
   }
  )
 
  # return for model not supported
  if ( "Message" %in% names(TT) ){
    if (fmt == "summary") { return(TT) }
    if (fmt =="gt") {
      return(gt::gt(TT) |>
        gt::tab_options(column_labels.hidden = TRUE))
    }
  }
   
  # Normal return for successfully computed Tukey tests
  if (fmt == "summary") {
    return(TT)
  } else if (fmt =="gt") {
    TTgt = gt_add_significance(TT) |> 
      gt::tab_header(title="Tukey test of curvature")
    return(TTgt)
  }
}
  
