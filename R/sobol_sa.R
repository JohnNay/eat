#'Conduct a Sobol Sensitivity Analysis of a Simulation Model
#'
#'\code{sobol_sa} conducts a global variance decomposition.
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
#'@param sobol_nboot Optional Numeric vector length one. Default is 1000.
#'@param iterations Optional numeric vector length one.
#'@param parallel Optional logical vector length one. Default is FALSE.
#'  
#'@return Returns a sobol objects that can be plotted by functions 
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
#' sobol_sa(fake_abm, inputs, "sq")
#' 
#'@export

sobol_sa <- function(abm, 
                     input_values,
                     out, 
                     sample_count = 100, 
                     sobol_nboot = 1000, 
                     iterations = NULL,
                     parallel = FALSE){
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  # Create two samples, removing samples violating constraints, until you have enough:
  input.sets.1 <- create_set(input_values, input_names, sample_count)
  input.sets.2 <- create_set(input_values, input_names, sample_count)
  
  # Make sets the same size:
  rows <- min(nrow(input.sets.1), nrow(input.sets.2))
  input.sets.1  <- input.sets.1[seq(rows), ]
  input.sets.2  <- input.sets.2[seq(rows), ]
  stopifnot(nrow(input.sets.1) == nrow(input.sets.2) & nrow(input.sets.2) > 0)
  
  ##################################################
  # Simulation runs with generated input factor sets:
  # Create instance of sobol class:
  sobol_aggregate <- sensitivity::sobol2007(model = NULL, 
                                            X1 = input.sets.1, X2 = input.sets.2, 
                                            nboot = sobol_nboot)
  
  # simulation results for input factor sets (as matrix)
  if (parallel) {
    doParallel::registerDoParallel(cores = parallel::detectCores())
  } # without registering the backend the %dopar% should just run as %do%
  if (missing(iterations)){
    sobol_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c'), {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out)
    })
  } else {
    sobol_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c'), {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out, iterations = iterations)
    })
  }
  # add simulation results (as vector) to sobol object
  sensitivity::tell(sobol_aggregate, sobol_sim)
  sobol_aggregate
}

