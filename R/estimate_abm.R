#'Estimate an ABM
#'
#'Using \code{\link{estimate_abm}} one can use their data and their abm function
#'they are using for \code{\link{cv_abm}} to estimate an ABM via optimization of
#'its global abm parameters or their specification. Then this can be used for 
#'the \code{\link{compute_iters}}, \code{\link{sobol_sa}}, and 
#'\code{\link{pc_sa}}.
#'
#'@inheritParams cv_abm
#'  
#'@return Returns a function that has three arguments: \code{parameters, out, 
#'  iterations}. If \code{out=="action_avg"} for the returned function, the
#'  average of all the actions is returned by this function; otherwise, the
#'  vector of the average for each time is returned by this function. This
#'  returns a wrapper function around their abm simulation function to be used
#'  for \code{\link{compute_iters}}, \code{\link{sobol_sa}}, and 
#'  \code{\link{pc_sa}}.
#'  
#'@export

estimate_abm <- function(data, features, Formula, agg_patterns,
                         abm_simulate,
                         abm_vars,
                         iters,
                         tseries_len,
                         verbose = TRUE,
                         tp = rep(tseries_len, nrow(agg_patterns)),
                         package = c("caretglm", "caretglmnet", "glm", "caretnnet", "caretdnn"),
                         sampling = FALSE, sampling_size = 1000,
                         STAT = c("mean", "median"),
                         abm_optim = c("GA", "DE"), 
                         optimize_abm_par = FALSE,
                         parallel_training = FALSE){
  
  # Extract the desired function object while avoiding undesired matching to objects of other types:
  abm_simulate <- match.fun(abm_simulate, descend = FALSE)
  
  if(!(identical(length(features), length(Formula))))
    stop("identical(length(features), length(Formula)) should be TRUE, but it's FALSE.")
  
  #################################################################
  # Mandatory individual-level model training:
  model <- training(data, features, Formula,
                    sampling = sampling, sampling_size = sampling_size, outcome_var_name = outcome_var_name,
                    package = package,
                    parallel = parallel_training) # TRAINING
  
  #################################################################
  # Optional ABM optimization:
  if (is.null(abm_vars$value)) {
    if (is.null(abm_vars$lower) | is.null(abm_vars$upper)) 
      stop("'abm_vars$lower' and/or 'abm_vars$upper' was NULL (missing) and 'abm_vars$value' was also missing. Either provide lower and upper bounds, or provide values.")
    
    if (verbose) cat("Starting to do ABM optimization with training data.\n") # TRAINING
    
    fitness <- function(parameter){
      abm_predicted <- rep(NA, nrow(agg_patterns))
      abm_dynamics <- matrix(NA, nrow=nrow(agg_patterns), ncol= tseries_len)
      for (u in unique(data$group)){ # make prediction for each of the groups 
        abm_results <- abm_simulate(model= model, features = features, 
                                    parameters = as.numeric(agg_patterns[u, ]),
                                    tuning_parameters = parameter,
                                    time_len = tp[u],
                                    iterations = iters,
                                    STAT = STAT) 
        if(is.na(abm_results$action_avg)) 
          stop(paste("Fitness function tried to return an NA value for avg action of group", u, "with param values as", parameter))
        if(any(is.na(abm_results$dynamics))) 
          stop(paste("Fitness function tried to return an NA value for dynamic action of group", u, 
                     "at time", which(is.na(abm_results$dynamics)) , "with param values as", parameter))
        abm_predicted[u] <- abm_results$action_avg
        abm_dynamics[u, seq(tp[u])] <- abm_results$dynamics
      }
      # leaving out groups in vec unique(test$group) in this comparison because it not used for the training, its the test.
      avg_action_error <- compute_rmse(abm_predicted[-unique(test$group)], 
                                       agg_patterns[-unique(test$group), which(names(agg_patterns) %in% "action")]) 
      if(is.na(avg_action_error)) 
        stop(paste("Fitness function tried to return an NA value for avg action error with param values as", parameter))
      dynamic_action_error <- rep(NA, nrow(agg_patterns))
      
      for (l in unique(data$group)){
        dynamic_action_error[l] <- compute_rmse(abm_dynamics[l, seq(tp[l])], 
                                                as.numeric(agg_patterns[l, which(names(agg_patterns) %in% paste(seq(tp[l])))]))
      }
      
      if(any(is.na(dynamic_action_error[-unique(test$group)])))
        stop(paste("Fitness function tried to return an NA value for dynamic action error with param values as", 
                   paste(parameter, collapse = ", ")))
      result <- -(avg_action_error + mean(dynamic_action_error[-unique(test$group)], na.rm=TRUE)) # MSE of the avg action + mean of the MSE's of the time series of all training data groups
      if(is.na(result)) stop(paste("Fitness function tried to return an NA value with param values as",
                                   paste(parameter, collapse = ", ")))
      if(length(result) != 1) stop(paste("Fitness function returned a result that is not of length one with param values as", 
                                         paste(parameter, collapse = ", ")))
      result
    }
    
    num_cores <- parallel::detectCores() - 1
    popSize <- ifelse(num_cores > 20, num_cores, 21)
    
    if (optimize_abm_par){
      doParallel::registerDoParallel(cores = num_cores)
    }
    
    if (abm_optim == "GA"){
      ga_solution <- GA::ga(type = "real-valued", fitness = fitness,
                            min = abm_vars$lower, max = abm_vars$upper,
                            popSize = popSize, run = 3, maxfitness = 0,
                            names = names(abm_vars$lower),
                            parallel = optimize_abm_par)
      solution <- ga_solution@solution[1, ]
    }
    if (abm_optim == "DE"){
      fitness_min <- function(parameter) -fitness(parameter) # this optimization routine minimizes objective function
      de_solution <- DEoptim::DEoptim(fitness_min, lower = abm_vars$lower, upper = abm_vars$upper, 
                                      control = DEoptim::DEoptim.control(parallelType = 2,
                                                                         foreachArgs = list(.packages = c("caret", "dplyr", "DEoptim"),
                                                                                            .export = c("agg_patterns", "data",
                                                                                                        "abm_simulate", "model", "features",
                                                                                                        "compute_rmse", "fitness",
                                                                                                        "fitness_min"), .verbose = TRUE),
                                                                         NP = popSize, itermax = 20,
                                                                         steptol = 3))
      solution <- de_solution$optim$bestmem
    }
    
    abm_vars$value <- solution
    if (verbose) cat("\nOptimal values of parameters are ", paste(solution, collapse = " ,"), ".\n", sep="")
  }
  #################################################################
  
  # Build abm with predictive models trained and used as agent models.
  function(parameters, out, iterations = iters){
    if(out=="action_avg"){
      abm_simulate(parameters,
                   model = model, features = features,
                   tuning_parameters = abm_vars$value, 
                   time_len = tseries_len,
                   iterations,
                   STAT = "mean")$action_avg
    } else {
      abm_simulate(parameters,
                   model = model, features = features,
                   tuning_parameters = abm_vars$value, 
                   time_len = tseries_len,
                   iterations,
                   STAT = "mean")$dynamics
    }
  }
  
}
