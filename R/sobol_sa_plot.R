correct_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  x[x < 0] <- 0
  x[x > 1] <- 1
  cbind(var = row.names(x), x)
}

#'Plot First-Order Effects from a Sobol Sensitivity Analysis of a Simulation
#'Model
#'
#'\code{plot_sobol_fo} plots first-order effects.
#'
#'This is function of the \strong{eat} package. \code{sobol_sa} conducts a
#'global variance decomposition, and then \code{plot_sobol_fo} can be used to
#'plot it.
#'
#'
#'@param x An object created by \code{sobol_sa}. \code{input_values}
#'@param outcome_var Optional character vector for labeling the outcome variable
#'  in the plot. Default is "Outcome".
#'  
#'@return Returns a ggplot2 plot.
#'  
#' @examples
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' s <- sobol_sa(fake_abm, inputs, "sq")
#' # plot_sobol_fo(s)
#' 
#'@export

plot_sobol_fo <- function(x, outcome_var = "Outcome"){
  ss <- correct_bias(x$S)
  ggplot2::ggplot(ss, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() +
    ggplot2::xlab("Variable") + ggplot2::ylab("Estimated Effect") +
    ggplot2::ggtitle(paste("First Order Effects of Variables on", outcome_var)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) +
    ggplot2::ylim(c(0,1))
}

#'Plot Total Effects from a Sobol Sensitivity Analysis of a Simulation
#'Model
#'
#'\code{plot_sobol_total} plots total effects.
#'
#'This is function of the \strong{eat} package. \code{sobol_sa} conducts a
#'global variance decomposition, and then \code{plot_sobol_total} can be used to
#'plot it.
#'
#'
#'@param x An object created by \code{sobol_sa}. \code{input_values}
#'@param outcome_var Optional character vector for labeling the outcome variable
#'  in the plot. Default is "Outcome".
#'  
#'@return Returns a ggplot2 plot.
#'  
#' @examples
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' s <- sobol_sa(fake_abm, inputs, "sq")
#' # plot_sobol_total(s)
#' 
#'@export

plot_sobol_total <- function(x, outcome_var = "Outcome"){
  tt <- correct_bias(x$T)
  ggplot2::ggplot(tt, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() + 
    ggplot2::xlab("Variable") + ggplot2::ylab("Estimated Effect") +
    ggplot2::ggtitle(paste("Total Sensitivity Effects of Variables on", outcome_var)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) + 
    ggplot2::ylim(c(0,1))
}
