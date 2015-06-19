# Calculation of R^2. this function is adapted from:
# J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter Estimation and Sensitivity Analysis of Agent-Based Models: 
# A Cookbook Using NetLogo and R. Journal of Artificial Societies and Social Simulation. 17, 11 (2014).
get_rsquare <- function(x, y, on.ranks) {
  data <- data.frame(Y = y, x)
  if (on.ranks) {
    for (i in seq(ncol(data))) {
      data[ ,i] <- rank(data[ ,i])
    }
  }
  i <- seq(nrow(data))
  d <- data[i, ]
  lm.Y <- stats::lm(formula(paste(colnames(d)[1], "~", paste(colnames(d)[-1], collapse = "+"))), 
                    data = d)
  summary(lm.Y)$r.squared
}

#'Partial Correlation Analysis of a Simulation Model
#'
#'\code{pc_sa} conducts a partial correlation analysis.
#'
#'This is function of the \strong{eat} package. It takes an abm in function form
#'and a list of input values. Helper function for extracting R-sqaured is from 
#'Thiele et al. (2014).
#'
#'@param abm A function that takes as input values for each of the 
#'  \code{input_values}
#'@param input_values List
#'@param out Character vector length one to be passed an argument to the 
#'  \code{abm} function to specify what outcome measure to use.
#'@param sample_count  Optional Numeric vector length one. Default is 100.
#'@param constraints Optional Character vector that is either "none" or is using
#'  only variable names that are specified in the input_values List argument. 
#'  This character vector is evaluated in an environment created for the sampled
#'  data on the variables, and its evaluation results in a Logical vector that 
#'  that subsets sampled.
#'@param nboot Optional Numeric vector length one. Default is 1000.
#'@param iterations Optional numeric vector length one.
#'@param parallel Optional logical vector length one. Default is FALSE.
#'@param cores Optional Numeric vector length one. Default is 
#'  parallel::detectCores().
#'@param verbose Optional logical vector.
#'@param rank Optional logical vector.
#'@param method Optional character vector that is either "src" (Standardized 
#'  Regression Coefficient) or "pcc" (Partial Correlation Coefficient)
#'  
#'@return Returns a sensitivity object that can be plotted by functions.
#'  
#' @examples
#' # Unconstrained Analysis
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' pc_sa(fake_abm, inputs, "sq")
#' pc_sa(fake_abm, inputs, "sq", method = "pcc", rank = FALSE)
#' pc_sa(fake_abm, inputs, "sq", method = "src", rank = FALSE)
#' 
#' # Constrained Analysis
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' pc_sa(fake_abm, inputs, "sq", constraints = "param1 > 0.1 & param2 < 0.9")
#' 
#'@references G. Pujol et al., Sensitivity: Sensitivity Analysis (2014), 
#'  (available at
#'  http://cran.r-project.org/web/packages/sensitivity/index.html).
#'  
#'  A. Saltelli, K. Chan, E. M. Scott, Sensitivity Analysis (Wiley, Chichester, 
#'  2009).
#'  
#'  J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter Estimation and 
#'  Sensitivity Analysis of Agent-Based Models: A Cookbook Using NetLogo and R. 
#'  Journal of Artificial Societies and Social Simulation. 17, 11 (2014).
#'  
#'@export

pc_sa <- function(abm, 
                  input_values,
                  out, 
                  sample_count = 100,
                  constraints = "none",
                  nboot = 1000, 
                  iterations = NULL,
                  parallel = FALSE,
                  cores = NULL,
                  verbose = TRUE,
                  rank = TRUE,
                  method = c("src", "pcc")){
  
  method <- match.arg(method)
  start_time <- as.numeric(proc.time()[[1]])
  call <- match.call()
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  if(parallel & missing(cores)) cores <- parallel::detectCores() - 1
  
  # Create samples, removing samples violating constraints, until you have enough:
  input.set <- create_set(input_values, input_names, sample_count, constraints)
  if(verbose) cat("Done with input set creation.\n")
  
  ##################################################
  # Simulation runs with generated input factor sets:
  if(verbose) cat("Starting simulations.\n")
  # simulation results for input factor sets (as matrix)
  if (parallel) {
    doParallel::registerDoParallel(cores = cores)
  } # without registering the backend the %dopar% should just run sequentially as %do%
  if (missing(iterations)){
    pc_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input.set)), .combine='c'), {
      abm(as.numeric(input.set[i, ]), out = out)
    })
  } else {
    pc_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input.set)), .combine='c'), {
      abm(as.numeric(input.set[i, ]), out = out, iterations = iterations)
    })
  }
  if(verbose) cat("Done with simulations.\n")
  
  if (method == "src"){
    result <- sensitivity::src(X = input.set, y = pc_sim, nboot = nboot, rank = rank)
    r_squared <- get_rsquare(x = input.set, y = pc_sim, 
                             on.ranks = rank)
  }
  
  if (method == "pcc"){
    result <- sensitivity::pcc(X = input.set, y = pc_sim, nboot = nboot, rank = rank)
    r_squared <- "Not relevant to this method. Only relevant to the 'src' method."
  }
  
  new("pcSA",
      call = call,
      result = result, 
      r_squared = r_squared,
      timing = as.numeric(proc.time()[[1]]) - start_time,
      session = sessionInfo())
}
