create_sample <- function(input_values, input_names, sample_count) {
  # will create values from 0 to 1 and must be transformed afterwards, if need be.
  
  # create a random sample of input factor sets with Latin Hypercube Sampling
  lhs_design <- lhs::improvedLHS(sample_count, length(input_values))
  
  # transform the standardized random values to the real input value range (if need be)
  # and apply the desired random distribution
  lhs_design <- lapply(seq(1,length(input_values)), function(i) {
    input_values[[i]]$ARGS$p <- as.vector(lhs_design[ ,i])
    do.call(input_values[[i]]$random_function, input_values[[i]]$ARGS) # input_values[[i]]$min, input_values[[i]]$max
  })
  names(lhs_design) <- input_names
  data.frame(lhs_design)
}

keep_satisfied <- function(sampled){
  # TODO: add any constraints
  constraints <- sampled
  data.frame(sampled[constraints, , drop=FALSE])
}

create_set <- function(input_values, input_names, sample_count){
  input.sets <- create_sample(input_values, input_names, sample_count)
  input.sets <- keep_satisfied(input.sets)
  while(nrow(input.sets) < sample_count) { 
    # Create input factor sets by latin hypercube sampling:
    input.sets <- rbind(input.sets,
                        create_sample(input_values, input_names, sample_count))  
    # Discard input factor sets that violate constraints:
    input.sets <- keep_satisfied(input.sets)
  }
  input.sets
}

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
#'@return Returns something
#'  
#' @examples
#' # In scripts, it can makes sense to set parallel to
#' # 'as.logical(Sys.info()['sysname'] != 'Windows')'.
#' 
#' #create a list to put in "input_values" arg of sobol_sa() func 
#'  # that looks like this: (just change param1 to the name of the actual param)
#'  # input_values <- lapply(list(param1 = NA, param2 = NA), 
#'  #                        function(x) list(random_function = "qunif",
#'                                           ARGS = list(min = 0, max = 1)))
#' # if there are any params that are binary valued give them a binom prior distribution:
#' #input_values[["param2"]] <- list(random_function = "qbinom",
#'                                   ARGS = list(size = 1, prob = 0.5))
#' 
#' 
#'
#'@export

sobol_sa <- function(abm, 
                     input_values,
                     out, 
                     sample_count = 100, 
                     sobol_nboot = 1000, 
                     iterations = NULL,
                     parallel = FALSE){
  
  out <- match.arg(out)
  
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
    sobol_sim <- foreach::foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c') %dopar% {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out) # out = c("avg", "firstlast")
    }
  } else {
    sobol_sim <- foreach::foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c') %dopar% {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out, iterations = iterations)
    }
  }
  # add simulation results (as vector) to sobol object
  sensitivity::tell(sobol_aggregate, sobol_sim)
  sobol_aggregate
}

correct_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  x[x < 0] <- 0
  x[x > 1] <- 1
  cbind(var = row.names(x), x)
}

plot_sobol_fo <- function(x, outcome_var = "Outcome"){
  ss <- correct_bias(x$S)
  ggplot2::ggplot(ss, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() +
    ggplot2::xlab("Variable") + ggplot2::ylab("Estimated Effect") +
    ggplot2::ggtitle(paste("First Order Effects of Variables on", outcome_var)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) +
    ggplot2::ylim(c(0,1))
}

plot_sobol_total <- function(x, outcome_var = "Outcome"){
  tt <- correct_bias(x$T)
  ggplot2::ggplot(tt, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() + 
    ggplot2::xlab("Variable") + ggplot2::ylab("Estimated Effect") +
    ggplot2::ggtitle(paste("Total Sensitivity Effects of Variables on", outcome_var)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) + 
    ggplot2::ylim(c(0,1))
}

