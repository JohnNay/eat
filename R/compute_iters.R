# Coefficient of variation
coef_var <- function(x){
  
}

#'Determine Number of Iterations to Simulate a Stochastic Model
#'
#'\code{compute_iters} conducts estimates a sufficient number of iterations for
#'subsequent analysis of a simulation model.
#'
#'This is function of the \strong{eat} package. It takes an abm in function form
#'and a list of input values.
#'
#'@param abm A function that takes as input values for each of the 
#'  \code{input_values}
#'@param input_values List
#'@param out Character vector length one to be passed an argument to the 
#'  \code{abm} function to specify what outcome measure to use.
#'@param constraints Optional Character vector that is either "none" or is using
#'  only variable names that are specified in the input_values List argument. 
#'  This character vector is evaluated in an environment created for the sampled
#'  data on the variables, and its evaluation results in a Logical vector that 
#'  that subsets sampled.
#'@param parallel Optional logical vector length one. Default is FALSE.
#'@param cores Optional Numeric vector length one. Default is 
#'  parallel::detectCores().
#'@param verbose Optional logical vector.
#'@param method Optional character vector that is either ...
#'  
#'@return Returns a sensitivity object that can be plotted by functions.
#'
#' @examples
#' compute_iters()
#' 
#'@references Lorscheid, I., Heine, B.O., & Meyer, M. (2012). Opening the "black
#'box" of simulations: increased transparency and effective communication 
#'through the systematic design of experiments. Computational and Mathematical 
#'Organization Theory, 18 (1), 22–62.
#'
#'Hendricks W, Robey K (1936) The sampling distribution of the coefficient of
#'variation. Ann Math Stat 7:129–132
#'
#'@export

compute_iters <- function(abm, 
                  input_values,
                  out, 
                  constraints = "none",
                  parallel = FALSE,
                  cores = NULL,
                  verbose = TRUE,
                  method = c("coef_var")){
  
  method <- match.arg(method)
  start_time <- as.numeric(proc.time()[[1]])
  call <- match.call()
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  if(parallel & missing(cores)) cores <- parallel::detectCores() - 1
  
  # Create sample, removing samples violating constraints, until you have enough:
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
  if(verbose) cat("Done with simulations.\n")
  
  if (method == "coef_var"){
    result <- 1
  }
  
  list(call = call,
      result = result, 
      timing = as.numeric(proc.time()[[1]]) - start_time,
      session = sessionInfo())
}
