correct_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  cbind(var = row.names(x), x)
}

#'Plot Partial Correlation Analysis of a Simulation Model
#'
#'\code{plot_pc} 
#'
#'This is function of the \strong{eat} package. \code{pc_sa} conducts a
#'a partial correlation analysis.
#'
#'
#'@param x An object created by \code{pc_sa}. 
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
#' s <- pc_sa(fake_abm, inputs, "sq")
#' # plot_pc(s)
#' 
#'@export

plot_pc <- function(x, outcome_var = "Outcome"){
  ss <- correct_bias(x[[7]]) # $SRRC, $SRC, $PRRC, $PRC
  
  ggplot2::ggplot(ss, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() +
    ggplot2::xlab("Variable") + ggplot2::ylab(
      paste("Estimated Effect on", outcome_var)) +
    ggplot2::ggtitle(paste("Estimated Effects of Variables on", outcome_var)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) +
    ggplot2::ylim(c(-1,1))
}
