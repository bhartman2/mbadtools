#' ggvar_irf
#' 
#' Plots impulse response functions from a `vars::varirf` object
#'
#' @param varirf a `varirf` object created using the `vars` package
#'
#' @returns a ggplot
#' @export
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
#' Plots the empirical fluctuation processes created by the `vars::stability()` test; 
#'   one is created for each equation. Use `patchwork::wrap_plots(nrow=n)` to display, 
#'   where n is the number of equations.
#'
#' @param varstabil a `varstabil` object created from `vars::stability()`
#'
#' @returns a list of ggplots one for each equation
#' @export
#'
#' @examples
#' data("Canada", package="vars")
#' var.2c <- vars::VAR(Canada, p = 2, type = "const")
#' stab = vars::stability(var.2c)
#' ggvar_plot_stability(stab)
#' 
ggvar_plot_stability = function(varstabil) {
  
  V = varstabil  
  A = V$stability
  nv = A %>% length
  
  Plist = list()
  
  for ( i in 1:nv) {
    
    process = A[[i]]$process
    nobs = A[[i]]$nobs
    tau = 0:nobs
    D = data.frame(tau=tau, process=process)
    Plist[[i]] = ggplot2::ggplot(D, ggplot2::aes(x=tau) ) +
      ggplot2::geom_line(ggplot2::aes(y=process)) +
      ggplot2::labs(title=stringr::str_c(A[[i]]$type, "of equation", 
                                         V$names[i], sep=" "), 
                    y = "Empirical fluctuation process",
                    x= "Time period") +
      ggplot2::scale_x_continuous(breaks = seq(0, nobs, by=10)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept=0)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept=1.2516), 
                          color="red", linewidth=.2)+
      ggplot2::geom_hline(ggplot2::aes(yintercept=-1.2516), 
                          color="red", linewidth=.2)+
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
    
  }
  
  return(Plist)
}

#' ggvar_plot_stability_break
#' 
#' Adds layers to a `ggvar_plot_stability()` plot; a vertical line and a text 
#'   notation of the max value, at the max of the stability index.
#'
#' @param pi breakpoint index; obtain from 
#' @param bv breakpoint value; use pi to obtain from 
#'
#' @returns ggplot layers to add to a `ggvar_plot_stability()` plot
#' @export
#'
#' @examples
ggvar_plot_stability_break = function(pi, bv) {
  
  vj = -1; if(bv<=0) vj=-vj
  
  list(
    # layer 1
    ggplot2::geom_vline(ggplot2::aes(xintercept=pi - 1), linetype="dotted"),
    # layer 2
    ggplot2::annotate('text',
                      x=pi,
                      y=bv,
                      label=bv,
                      vjust=vj, fontface="italic")
  )
  
}

#' bvarirf_to_varirf
#' 
#' Transforms a `bvartools::bvarirf` object into a `vars::varirf` skeleton object
#'  for `ggvar_irf()` to plot. `bvartools` only allows one response at a time 
#'  in its `bvarirf` object.
#'
#' @param bvarirf a `bvarirf` object made by `bvartools`
#' @param impulse character, the impulse column name
#' @param response character, the response column name
#' @param cumulative logical, default TRUE
#' @param ortho logical, default TRUE
#' @param boot logical, default TRUE
#'
#' @returns a `varirf` skeleton object good enough for `mbadtools::ggvar_irf()` to plot.
#' @export
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
#' plot multiple timeseries and forecasts made with `predict()`; 
#'  use design and `patchwork::wrap_plots()` to plot from list 
#'
#' @param bvar_pred predictions of class `bvarprd` or `varprd`
#'
#' @returns a list of ggplots
#' @export
#'
#' @examples
#' # Load data
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
#' # Calculate forecasts
#' pred <- predict(object, new_d = rep(1, 10))
#' 
#' # Plot forecasts
#' ggvar_forecastplot(pred)
#' 
ggvar_forecastplot = function (bvar_pred) {
  
  P = bvar_pred
  
  if( class(P) == "bvarprd") Phistory = P$y 
  else if (class(P) == "varprd") Phistory = P$model$y
  
  Phistory = Phistory %>% data.frame
  Pnames = names(Phistory)
  
  # forecasts data frame
  Pdf = data.frame()
  for (i in 1:length(Pnames)) {
    if (class(P) == "bvarprd") {
      lo = P$fcst[[i]][,1]
      up = P$fcst[[i]][,3]
      fc = P$fcst[[i]][,2]      
    } else
    if (class(P)=="varprd") {
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
      ggplot2::geom_point(ggplot2::aes(1:nrow(Phistory), Phistory[,i])) +
      ggplot2::geom_point(data=X, 
                          ggplot2::aes(x = nrow(Phistory)+seq_len(nx),
                                       y = .fitted),
                          color="blue") +
      ggplot2::geom_ribbon(data=X,
                           ggplot2::aes(x = nrow(Phistory)+seq_len(nx), 
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
#' Forecast Error Variance Decomposition plots for `bvartools::bvar` objects; plots all
#'  responses or any one 
#'
#' @param bvarobject a bvarest object 
#' @param type character, type of fevd to create, no default, allowed types are
#'    c("oir","gir","sir", "sgir"); only "oir" has sum to unity.
#' @param ... other parameters for `fevd.bvar`; see documentation for details.
#'
#' @returns a named list of ggplots, named by response variables of model. 
#'   Display with patchwork or just plot one list member.
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
    vd = fevd(bvarobject, response=nm[i], type=type, )
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