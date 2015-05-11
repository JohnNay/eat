#'Partial Correlation Analysis of a Simulation Model
#'
#'\code{pc_sa} conducts a partial correlation analysis.
#'
#'This is function of the \strong{eat} package. It takes an abm in function form
#'and a list of input values.
#'
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
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  if(parallel & missing(cores)) cores <- parallel::detectCores() - 1
  
  # Create two samples, removing samples violating constraints, until you have enough:
  input.set <- create_set(input_values, input_names, sample_count, constraints)
  if(verbose) cat("Done with input set creation \n")
  
  ##################################################
  # Simulation runs with generated input factor sets:
  if(verbose) cat("Starting simulations \n")
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
  if(verbose) cat("Done with simulations \n")
  
  if (method == "src"){
    result <- sensitivity::src(X = input.set, y = pc_sim, nboot = nboot, rank = rank)
  }
  
  if (method == "pcc"){
    result <- sensitivity::pcc(X = input.set, y = pc_sim, nboot = nboot, rank = rank)
  }
  
  result
}
