#' ggvar_irf
#' 
#' Plots impulse response functions from a `vars::varirf` object
#'
#' @param varirf a `varirf` object created using the `vars` package
#'
#' @returns a ggplot
#' @export
#' @importFrom stringr str_c
#' 
#' @examples
#' data("Canada", package="vars")
#' var.2c <- vars::VAR(Canada, p = 2, type = "const")
#' IRF = vars::irf(var.2c, impulse = "e", response = "prod")
#' ggvar_irf(IRF)
#' 
ggvar_irf = function (varirf) {
  
  # construct data frame
  imp = varirf$impulse
  irf = varirf$irf[[imp]]
  hi = varirf$Upper[[imp]]
  lo = varirf$Lower[[imp]]
  tau = 1:length(irf)
  
  irf_df = data.frame(tau, irf)
  irf_df = dplyr::case_when (varirf$boot ~  irf_df %>% 
                               dplyr::mutate(hi=hi, lo=lo),
                       .default = irf_df)
  nm = names(irf_df)

  # graph labels
  above = stringr::str_c(
    dplyr::case_when(varirf$ortho ~ "Orthogonal", .default=""),
    "Impulse Response from",
    varirf$impulse,
    dplyr::case_when(varirf$cumulative ~ '(cumulative)', .default=""),
    sep=" "
  )
  below = dplyr::case_when( varirf$boot ~ 
                     stringr::str_c(
                            as.character(100*(1-varirf$ci)),
                            "% Bootstrap CI,",
                            as.character(varirf$runs),
                            "runs",
                            sep=" "
                          ), 
                     .default=""
    )
  res = varirf$response
  
  nres = length(res)
  
  # ggplots
  P = ggplot2::ggplot(irf_df, ggplot2::aes(x=.data[[nm[1]]])) +
    ggh4x::geom_pointpath(ggplot2::aes(y=.data[[nm[2]]])) +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), color="red") +
    ggplot2::scale_x_continuous(breaks=1:nrow(irf_df)) 
  if (varirf$boot) 
    P = P + ggplot2::geom_ribbon(ggplot2::aes(ymin=.data[[nm[3]]], 
                            ymax=.data[[nm[4]]]), alpha=.1)
  P = P + ggplot2::labs(title=above, x = below, y = res) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(P)
}

#' ggvar_plot_stability
#' 
#' Plots the list of empirical fluctuation processes created by the 
#'   \code{\link[vars]{stability}} test; 
#'   one is created for each equation. Use \code{\link[patchwork]{wrap_plots}} to display, 
#'   where n is the number of plots. It wraps \code{\link{ggvar_plot_stability_break}} and
#'   and \code{\link{ggvar_plot_stability_lines}}
#'   
#' If you want one or just some plots, make them into a list first
#'
#' @param varstabil a `varstabil` list object from \code{\link[vars]{stability}} or 
#'   a list of \code{\link[strucchange]{efp}} objects
#' @param lines logical, plot the boundary lines? default TRUE
#' @param breaks logical, plot vertical line at break and value, default TRUE
#' @param a numeric, limit for boundary lines, default 1.2516; 
#'   see \code{\link{ggvar_plot_stability_break}} for rationale
#'
#' @returns a list of ggplots one for each variable
#' @export
#' @importFrom stringr str_c
#'
#' @examples
#' data("Canada", package="vars")
#' var.2c <- vars::VAR(Canada, p = 2, type = "const")
#' stab = vars::stability(var.2c)
#' ggvar_plot_stability(stab, lines=FALSE, breaks=FALSE) # override defaults for generic plot
#' 
ggvar_plot_stability = function(varstabil, lines=TRUE, breaks=TRUE, a=1.2516) {
  
  if (a != 1.2516) {
    warning("Don't change this parameter in most circumstances; 
            know what you are doing.", call. = FALSE)
    response = readline("Enter a new value (or press Enter to use 1.2516): ")
    if (response == "") {
      a <- 1.2516
    } else {
      x <- as.numeric(response)
    }
  }
  
  # which type of list?
  if ( is(varstabil, 'varstabil') ) {
    V = varstabil  
    A = V$stability
    nv = length(A)
    nm = V$names
  } else 
  if (is(varstabil, 'list')) {
    A = varstabil
    nm = names(A)
    nv= length(A)
  }

  Plist = list()
  
  for ( i in 1:nv) {
    
    process = A[[i]]$process
    nobs = A[[i]]$nobs
    tau = 0:nobs
    D = data.frame(tau=tau, process=process)
    J = ggplot2::ggplot(D, ggplot2::aes(x=tau) ) +
      ggplot2::geom_line(ggplot2::aes(y=process)) +
      ggplot2::labs(title=stringr::str_c(A[[i]]$type, "process", 
                                         nm[i], sep=" "), 
                    y = "Empirical fluctuation process",
                    x= "Time period") +
      ggplot2::scale_x_continuous(breaks = seq(0, nobs, by=10)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
    
    if (lines) J = J + ggvar_plot_stability_lines()
    
    if (breaks) {
      pi = which.max(abs(D$process))
      bv = round(D$process[pi],3)
      J = J + ggvar_plot_stability_break(pi, bv)
    }
    
    Plist[[i]] = J
  }
  
  
  return(Plist)
}

#' ggvar_plot_stability_break
#' 
#' Internal function that adds layers to a \code{\link{ggvar_plot_stability}} plot; 
#'   a vertical line and a text 
#'   notation of the max value, at the max of the stability index. 
#'   Called by \code{\link{ggvar_plot_stability}} optionally.
#'
#' @param pi breakpoint index; obtain from process maximum point
#' @param bv breakpoint value; use pi to obtain from process
#'
#' @returns ggplot layers to add to a `ggvar_plot_stability()` plot
#' @export
#'
#' @examples
#' data("Canada", package="vars")
#' var.2c <- vars::VAR(Canada, p = 2, type = "const")
#' stab = vars::stability(var.2c)
#' ggvar_plot_stability(stab, lines=FALSE) # override default
#' 

ggvar_plot_stability_break = function(pi, bv) {
  
  vj = -1; if(bv <=0 ) vj=-vj
  
  list(
    # layer 1
    ggplot2::geom_vline(ggplot2::aes(xintercept= pi - 1), linetype="dotted"),
    # layer 2
    ggplot2::annotate('text',
                      x=pi,
                      y=bv,
                      label=bv,
                      vjust=vj, fontface="italic"),
    ggplot2::annotate('text',
                      x=pi,
                      y=-1.2516,
                      label=pi,
                      vjust=vj, fontface="italic")
  )
  
}

#' ggvar_plot_stability_lines
#' 
#' Internal function that adds layers to a \code{\link{ggvar_plot_stability}} plot; 
#'   adds horizontal boundary lines. Called by
#'   \code{\link{ggvar_plot_stability}} optionally.
#'
#' @param a limit for upper and lower bounds; default 1.2516, taken from 
#'   Gemini discussion of limits for \code{\link[strucchange]{efp}}.
#'
#' @returns a ggplot
#' @export
#'
#' @examples
#' data("Canada", package="vars")
#' var.2c <- vars::VAR(Canada, p = 2, type = "const")
#' stab = vars::stability(var.2c)
#' ggvar_plot_stability(stab, breaks=FALSE) # lines=T is default
#' 
ggvar_plot_stability_lines = function(a=1.2516) {
  
  list(
    
    # layer 1
    ggplot2::geom_hline(ggplot2::aes(yintercept=0)),
    # layer 2  
    ggplot2::geom_hline(ggplot2::aes(yintercept=a), color="red", linewidth=.2),
    #layer 3
    ggplot2::geom_hline(ggplot2::aes(yintercept=-a), color="red", linewidth=.2)
      
  )
  
}

#' bvarirf_to_varirf
#' 
#' Transforms a `bvarirf` object from\code{\link[bvartools]{irf}} into 
#'  a `varirf` skeleton object from\code{\link[vars]{irf}}, 
#'  for \code{\link{ggvar_irf}} to plot. The `bvarirf` object only contains one response at a time.
#'
#' @param bvarirf a `bvarirf` object made by \code{\link{bvartools}}
#' @param impulse character, the impulse column name
#' @param response character, the response column name
#' @param cumulative logical, default TRUE
#' @param ortho logical, default TRUE
#' @param boot logical, default TRUE
#' @param ci numeric p-value for confidence interval; in (0,1), default .05
#' @param runs integer, number of runs, default 100
#'
#' @returns a `varirf` skeleton object good enough for \code{\link{ggvar_irf}} to plot.
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' # Load data
#' data("e1", package="bvartools")
#' e1 <- diff(log(e1)) * 100
#' 
#' # Generate model data
#' model <- bvartools::gen_var(e1, p = 2, deterministic = 2,
#'                  iterations = 100, burnin = 10)
#' # Number of iterations and burnin should be much higher.
#' 
#' # Add prior specifications
#' model <- bvartools::add_priors(model)
#' 
#' # Obtain posterior draws
#' object <- bvartools::draw_posterior(model)
#' 
#' # Calculate IR
#' ir <- bvartools::irf(object, impulse = "invest", response = "cons")
#' 
#' # Plot IR
#' X = bvarirf_to_varirf(ir, impulse="invest",  response="cons")
#' ggvar_irf(X)
#' 
bvarirf_to_varirf = function(bvarirf, impulse, response,
                             cumulative=TRUE,
                             ortho=TRUE,
                             boot=TRUE,
                             ci=.05,
                             runs=100) {
  BV = bvarirf
  
  bvar_irdf = list(
    Lower=stats::setNames(list(BV[,1]), impulse),
    Upper=stats::setNames(list(BV[,3]), impulse),
    irf  =stats::setNames(list(BV[,2]), impulse),
    impulse = impulse,
    response = response,
    ortho = ortho,
    cumulative = cumulative,
    boot = boot,
    ci=ci,
    runs=runs
  )
  return(bvar_irdf)
}

#' ggvar_forecastplot
#' 
#' plot multiple timeseries and forecasts made with \code{\link{predict}}; 
#'  use design and \code{\link[patchwork]{wrap_plots}} to plot from list.
#'
#' @param bvar_pred predictions of class `bvarprd` from\code{\link[bvartools]{predict.bvar}} or
#'    `varprd` from \code{\link[vars]{predict}}.
#' @param trun integer, start index of data to plot for all predictions; default 1
#'
#' @returns a list of ggplots
#' @importFrom methods is
#' @importFrom stats lag
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' # Load data
#' library(bvartools)
#' data("e1")
#' e1 <- diff(log(e1)) * 100
#' # Generate model data
#' model <- gen_var(e1, p = 2, deterministic = 2,
#'                  iterations = 100, burnin = 10)
# Add prior specifications
#' model <- add_priors(model)
#' # Obtain posterior draws
#' object <- draw_posterior(model)
#' # Calculate forecasts
#' pred <- predict(object, new_d = rep(1, 10))
#' # Plot forecasts
#' ggvar_forecastplot(pred)
#' 
ggvar_forecastplot = function (bvar_pred, trun=1) {
  
  P = bvar_pred
  
  if( is(P, "bvarprd") ) Phistory = P$y 
  else if (is(P,"varprd") ) Phistory = P$model$y
  
  Phistory = Phistory %>% data.frame
  Pnames = names(Phistory)
  ng = nrow(Phistory)
  
  if ( trun!=1 ) Phistory = Phistory[trun:ng,]
  
  # forecasts data frame
  Pdf = data.frame()
  for (i in 1:length(Pnames)) {
    if (is(P,"bvarprd")) {
      lo = P$fcst[[i]][,1]
      up = P$fcst[[i]][,3]
      fc = P$fcst[[i]][,2]      
    } else
    if (is(P,"varprd")) {
      lo = P$fcst[[i]][,2]
      up = P$fcst[[i]][,3]
      fc = P$fcst[[i]][,1]      
    }

    row = data.frame(impulse=Pnames[[i]],
                     .fitted=fc, .lower = lo, .upper=up)
    Pdf = bind_rows(Pdf, row)
  }

  Plotlist = list()
  for (i in 1:length(Pnames)) {
    X = Pdf %>% 
      dplyr::filter(impulse==Pnames[[i]]) %>% 
      dplyr::select(-impulse)
    nx = nrow(X)
    
    Plotlist[[Pnames[i]]] = 
      ggplot2::ggplot(Phistory) +
      ggplot2::geom_point(ggplot2::aes(trun:ng, Phistory[,i])) +
      ggplot2::geom_point(data=X, 
                          ggplot2::aes(x = ng + seq_len(nx),
                                       y = .fitted),
                          color="blue") +
      ggplot2::geom_ribbon(data=X,
                           ggplot2::aes(x = ng + seq_len(nx), 
                                        ymin = .lower, ymax = .upper), 
                           alpha=.2, fill="lightgreen") +
      ggplot2::geom_hline(ggplot2::aes(yintercept=0), color="red") +
      ggplot2::labs(y = Pnames[i], x="Period") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
    
  }
  return(Plotlist)
}

#' ggvar_fevdplot
#' 
#' Forecast Error Variance Decomposition plots for `bvartools::bvar` objects
#'   from \code{\link[bvartools]{bvartools}}; plots all responses or any one 
#'  
#' @param bvarobject a `bvarest` object from \code{\link[bvartools]{bvartools}}
#' @param type character, type of fevd to create, no default, allowed types are
#'    c("oir","gir","sir", "sgir"); only "oir" has sum to unity.
#' @param ... other parameters for \code{\link[bvartools]{fevd.bvar}}; see documentation for details.
#'
#' @returns a named list of ggplots, named by response variables of model. 
#'   Display with patchwork or just plot one list member.
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' #' # Load data
#' library(bvartools)
#' data("e1")
#' e1 <- diff(log(e1)) * 100
#' 
#' # Generate model data
#' model <- gen_var(e1, p = 2, deterministic = 2,
#'                  iterations = 100, burnin = 10)
#' 
# Add prior specifications
#' model <- add_priors(model)
#' 
#' # Obtain posterior draws
#' object <- draw_posterior(model)
#' 
#' ggvar_fevdplot(object, type="oir", n.ahead=7)
#' 
ggvar_fevdplot = function(bvarobject, type="oir", ...) {
  
  nm = attr(bvarobject$y, "dimnames")[[2]]
  nn = length(nm)
  
  FPplots = list()
  for (i in 1:nn) {
    vd = bvartools::fevd(bvarobject, response=nm[i], type=type, )
    nx=nrow(vd)
    X1 = tibble::rownames_to_column(data.frame(vd), var="Period")
    X1[,1] = as.numeric(X1[,1])-1
    X1 = tidyr::pivot_longer(X1, cols=2:ncol(X1), 
                             names_to = "impulse", values_to = "vard")
    
    P = ggplot2::ggplot(X1, 
                        ggplot2::aes(x=Period, y = vard*100, fill=impulse)
    ) +
      ggplot2::geom_col(color="black") +
      ggplot2::labs(y="Percentage", x="",
                    title=paste("FEVD type", type, ":", nm[i], sep=" ")
      ) +
      ggplot2::scale_x_continuous(breaks=0:nx)+
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))     
    
    FPplots[[i]] = P
    
  }
  
  return( FPplots %>% stats::setNames(nm))
  
}