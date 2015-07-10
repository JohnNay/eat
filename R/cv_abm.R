#'Estimate and Test an ABM
#'
#'\code{cv_abm} uses cross-validation to test an ABM's predictive power.
#'
#'The function returns an S4 object. See \linkS4class{cv_abm} for the details of
#'the \code{slots} (objects) that this type of object will have.
#'
#'@param data \code{data.frame} with each row (obervational unit) being an 
#'  individual decision. With a column named "group" specifying which group of 
#'  \code{agg_patterns} each obseravtion is in, and a column named "period" 
#'  specifying at what time period each behavior was taken.
#'@param features \code{list} of the variables (columns in \code{data}) to be 
#'  used in the prediction \code{Formula}. As many elements in the \code{list} 
#'  as we want discrete models for different times. Each element of the 
#'  \code{list} is a \code{character vector}, with each element of the 
#'  \code{character vector} being a feature to use for training an 
#'  individual-level model.
#'@param Formula \code{list} where each element is a length one character vector
#'  that specifies a formula, e.g. \code{y ~ x}. The character vector makes 
#'  sense in the context of the \code{features} and \code{data}. There are as 
#'  many elements in the list as there are discrete models for different times.
#'@param k numeric vector length one specifying the number of models for 
#'  different times.
#'@param agg_patterns data.frame with rows (observational unit) being the group 
#'  and columns: (a.) those aggregate level variables needed for the prediction 
#'  with the specified \code{formula} (with same names as the variables in the 
#'  formula); (b.) a column named "action" with the proportion of the relevant 
#'  outcome action taken in that group; (c.) columns named 
#'  \code{paste(seq(tseries_len))} with the mean/median levels (\code{STAT}) of 
#'  the action for each time period.
#'@param abm_simulate function with these arguments: \code{model, features, 
#'  parameters, tuning_parameters, iterations, time_len, STAT = c("mean", 
#'  "median")}. Where \code{model} is the output of \code{\link{training}}. 
#'  Output of the function is a list with three named elements: \code{dynamics, 
#'  action_avg, simdata}. Where \code{dynamics} is a numeric vector length 
#'  \code{tseries_len}, \code{action_avg} is a numeric vector length one, and 
#'  \code{simdata} is a \code{data.frame} with the numeric results of the 
#'  simulation.
#'@param abm_vars a list with either (1.) a numeric vector named "lower" AND a 
#'  numeric vector named "upper" each the length of the number of tuning_params 
#'  of ABM (the names of the elements of these vecs should be the names of the 
#'  variables and they should be in the same order that the \code{abm_simulate} 
#'  function uses them); or (2.) a numeric vector named "value" the length of 
#'  the number of tuning_params of the ABM (variables should be in the same 
#'  order that the \code{abm_simulate} function uses them). Either provide lower
#'  and upper elements of the list or provide a value element of the list.
#'@param iters numeric vector length one specifying number of iterations to 
#'  simulate ABM for.
#'@param tseries_len numeric vector length one specifying maximum number of time
#'  periods to use for model training and testing. If some groups have less than
#'  the maximum then you need to provide a vector to the \code{tp} argument.
#'@param tp optional numeric vector length number of rows of \code{agg_patterns}
#'  specifying how long the time series for each group should be. Default is 
#'  \code{rep(tseries_len, nrow(agg_patterns))}.
#'@param package optional character vector length one, default is 
#'  \code{"caretglm", "caretglmnet", "glm", "caretnnet", "caretdnn"}.
#'@param sampling optional logical vector length one, default is \code{FALSE}. 
#'  If \code{sampling == TRUE}, we sample equal numbers of observations from 
#'  each 'group' to reduce potential problems with the final estimated model 
#'  being too affected by groups with more observations.
#'@param sampling_size optional numeric vector length one specifying how many 
#'  observations from each group that \code{\link{training}} should sample to 
#'  train the model, default is 1000. Only applicable when \code{sampling} 
#'  argument is set to \code{TRUE}.
#'@param outcome_var_name optional character vector length one, default is 
#'  \code{"action"}. \code{\link{training}} uses it to sample to train the model
#'  with a balanced sampling based on \code{outcome_var_name}. Only applicable
#'  when \code{sampling} argument is set to \code{TRUE}.
#'@param STAT optional character vector length one, default is \code{c("mean", 
#'  "median")}.
#'@param saving optional logical vector length one, default is \code{FALSE}.
#'@param filename optional character vector length one, default is \code{NULL}.
#'@param abm_optim optional character vector length one, default is 
#'  \code{c("GA", "DE")}.
#'@param validate optional character vector length one, default is 
#'  \code{c("lgocv", "cv")}.
#'@param folds optional numeric vector length one, default is 
#'  \code{ifelse(validate == "lgocv", max(data$group), 10)}.
#'@param drop_nzv optional logical vector length one, default is \code{FALSE}.
#'@param verbose optional logical vector length one, default is \code{TRUE}.
#'@param predict_test_par optional logical vector length one, default is 
#'  \code{FALSE}. If you are getting any errors with this function, make sure 
#'  you set args like this to FALSE because debugging in parallel is much 
#'  harder.
#'@param optimize_abm_par optional logical vector length one, default is 
#'  \code{FALSE}. This is passed to the optimization algorithm.
#'@param parallel_training optional logical vector length one, default is 
#'  \code{FALSE}. This is passed to \code{\link{training}}.
#'  
#'@return Returns an S4 object of class \linkS4class{cv_abm}. With slots for 
#'  \code{call = "language", predicted_patterns = "list", timing = "numeric", 
#'  and diagnostics = "character"}.
#'  
#' @examples
#'# Create data:
#'cdata <- data.frame(period = rep(seq(10), 1000),
#'                    action = rep(0:1, 5000),
#'                  my.decision1 = sample(1:0, 10000, TRUE),
#'                  other.decision1 = sample(1:0, 10000, TRUE),
#'                  group = c(rep(1, 5000), rep(2, 5000)))
#'time_len <- 2
#'agg_patterns <- data.frame(group = c(1,2),
#'                         action = c( mean(as.numeric(cdata[cdata$group==1, "action"])),
#'                                     mean(as.numeric(cdata[cdata$group==2, "action"]))),
#'                         c(eat::period_vec_create(cdata[cdata$group==1, ], time_len)[1],
#'                           eat::period_vec_create(cdata[cdata$group==2, ], time_len)[1]),
#'                         c(eat::period_vec_create(cdata[cdata$group==1, ], time_len)[2],
#'                           eat::period_vec_create(cdata[cdata$group==2, ], time_len)[2]))
#'names(agg_patterns)[3:4] <- c("1", "2")
#'
#'# Create ABM:
#'simulate_abm <- function(model, features, parameters, time_len, 
#'                         tuning_parameters,
#'                       iterations = 1250, STAT = "mean"){
#'matrixOut <- data.frame(period = rep(1:10, 1000),
#'                        action = rep(0:1, 5000),
#'                        my.decision1 = sample(1:0, 10000, TRUE),
#'                        other.decision1 = sample(1:0, 10000, TRUE))
#'action_avg <- mean(matrixOut$action, na.rm=TRUE) 
#'dynamics <- period_vec_create(matrixOut, time_len)
#'list(dynamics = dynamics, action_avg = action_avg, simdata = matrixOut)
#'} 
#'# Create features and formula lists:
#'k <- 1
#'features <- as.list(rep(NA, k)) # create list to fill
#'features[[1]] <- c("my.decision1", "other.decision1")
#'Formula <- as.list(rep(NA, k)) # create list to fill
#'Formula[[1]] <- "action ~ my.decision1 + other.decision1"
#'# Call cv_abm():
#'res <- cv_abm(cdata, features, Formula, k, agg_patterns,
#'              outcome_var_name = "action",
#'              abm_simulate = simulate_abm,
#'              abm_vars = list(values = c(0.3, 0.5)),
#'              iters = 1000,
#'              tseries_len = time_len,
#'              tp = c(1, 2),
#'              package = "caretglm",
#'              STAT = "mean",
#'              saving = FALSE, filename = NULL,
#'              validate = "lgocv", 
#'              drop_nzv = FALSE, 
#'              predict_test_par = FALSE)
#'              
#'summary(res)
#'#plot(res)
#'#performance(res, "cor_pval")
#'
#'@export

cv_abm <- function(data, features, Formula, k, agg_patterns,
                   abm_simulate,
                   abm_vars,
                   iters,
                   tseries_len,
                   tp = rep(tseries_len, nrow(agg_patterns)),
                   package = c("caretglm", "caretglmnet", "glm", "caretnnet", "caretdnn"),
                   sampling = FALSE, sampling_size = 1000, outcome_var_name = "action",
                   STAT = c("mean", "median"),
                   saving = FALSE, filename = NULL,
                   abm_optim = c("GA", "DE"), 
                   validate = c("lgocv", "cv"), 
                   folds = ifelse(validate == "lgocv", max(data$group), 10), 
                   drop_nzv = FALSE, 
                   verbose = TRUE,
                   predict_test_par = FALSE,
                   optimize_abm_par = FALSE,
                   parallel_training = FALSE){
  # iterations= parameter[1]*15000 inside the fitness() for GA optimization
  # because more noise (parameter[1]), more iterations are needed to determine how good that param set is
  
  # Extract the desired function object while avoiding undesired matching to objects of other types:
  abm_simulate <- match.fun(abm_simulate, descend = FALSE)
  
  if(saving & missing(filename)) 
    stop("If saving the file, supply a 'filename'. Every fold, this will save the cv_results and the counter for what fold you are on.")
  if(length(tp) != nrow(agg_patterns)) 
    stop("The length of the 'tp' vector supplied is not the same as the number of rows of the 'agg_patterns' supplied.")
  
  msg <- "\n\n"
  start_time <- as.numeric(proc.time()[[3]])
  call <- match.call()
  
  # Make sure the 'data' has the features that the formula needs:
  if (!all(features[[1]] %in% colnames(data))) stop("Not all of the features specified are in the data provided.")
  
  abm_optim <- match.arg(abm_optim)
  validate <- match.arg(validate)
  package <- match.arg(package)
  STAT <- match.arg(STAT)
  
  predicted_patterns <- lapply(as.list(rep(NA, max(data$group))), 
                               function(x) list(predicted=NA, actual=NA,
                                                dynamics=rep(NA, tseries_len), simdata=data.frame()))
  
  # The non-caret version of creating fold assignments:
  #     group_folds <- sample(seq(folds), length(unique(data$group)), replace=TRUE) # try to allocate each obs to one of k folds
  #     while(length(unique(group_folds)) != folds){
  #       group_folds <- sample(seq(folds), length(unique(data$group)), replace=TRUE) # keep trying to allocate each obs to one of k folds
  #     }
  group_folds <- caret::createFolds(y = unique(data$group), k = folds, list = FALSE)
  
  if(length(group_folds) != length(unique(data$group))) stop("Assignment of groups to folds didnt work.")
  if(length(unique(group_folds)) != folds) stop("Assignment of groups to folds didnt work.")
  if(verbose) cat("Group folds:", group_folds ,"\n")
  msg <- paste0(msg, "Group folds: ", paste(group_folds, collapse = ", "), "\n")
  
  # create a vector same length as data with assignments of each row to a fold:
  fold_ass <- rep(NA, nrow(data))
  for (s in seq(nrow(data))) fold_ass[s] <- group_folds[data[s, "group"]]
  if (length(fold_ass) != nrow(data)) stop("Creating a vector same length as data with assignments of each row to a fold didnt work.")
  data <- cbind(data, fold_ass = fold_ass)
  
  # In the ith fold, the elements of folds that equal i are in the test set, and the remainder are in the training set.
  for(i in seq(folds)){
    if(verbose) cat("\n--------- + Starting with training data (all data but fold ", i, "). + ---------\n", sep="")
    msg <- paste0(msg, "\n--------- + Starting with training data (all data but fold ", i, "). + ---------\n")
    training_data <- data[data$fold_ass!=i, ] # use all data but i for TRAINING
    
    training_features <- features
    training_Formula <- Formula
    
    nzvs <- caret::nearZeroVar(training_data[ , which(names(training_data) %in% features[[k]])], freqCut = 95/5, uniqueCut=10) 
    if (length(nzvs) > 0){
      to_drop <- colnames(training_data)[which(names(training_data) %in% features[[k]])[nzvs]]
      if(verbose) cat("We should be dropping", length(to_drop), "feature(s), which is (are):", to_drop, "\n")
      msg <- paste0(msg, "We should be dropping ", length(to_drop), " feature(s), which is (are): ", paste(to_drop, collapse = ", "), "\n")
      
      if (drop_nzv){
        # just names in features[[k]] so we dont drop group, folds and training vars
        if(verbose) cat("Dropping", length(to_drop), "feature(s), which is (are):", paste(to_drop, collapse = ", "), ".\n")
        msg <- paste0(msg, "Dropping ", length(to_drop), " feature(s), which is (are): ", paste(to_drop, collapse = ", "), ".\n")
        
        # TODO: check that the features to be droppped are ACTUALLY IN the formula. If they arent then skip this next expr.
        for (m in seq(k)) {
          if (length(features[[m]][!(features[[m]] %in% to_drop)]) > 0){
            # this conditional makes sure that there is some features in there
            # this is important for any "constant" only feature sets.
            training_features[[m]] <- features[[m]][!(features[[m]] %in% to_drop)]
            for(f in seq(length(to_drop))){
              # remove all to_drop features from the character vec: 
              dropping <- to_drop[f]
              # find where 'dropping' occurs in the char vec and take it out:
              fterms <- attr(terms(as.formula(training_Formula[[m]])), "term.labels")
              frhs <- paste(fterms[-grep(dropping, fterms)], collapse=" + ")
              flhs <- formula.tools::lhs.vars(as.formula(training_Formula[[m]]))
              training_Formula[[m]] <- paste(c(flhs, frhs), collapse=" ~ ")
            }
            
            cat("New training formula:", training_Formula[[m]])
            msg <- paste0(msg, "New training formula: ", training_Formula[[m]], ".\n")
          }
        }
        
      }
    }
    
    if (verbose) cat("Training data has ", nrow(training_data), " rows. And has groups ", paste(sort(unique(training_data$group)), collapse = ", "), ".\n", sep="")
    msg <- paste0(msg, "Training data has ", nrow(training_data), " rows. And has groups ", paste(sort(unique(training_data$group)), collapse = ", "), ".\n")
    
    model <- training(training_data, features, training_Formula, k, 
                      sampling = sampling, sampling_size = sampling_size, outcome_var_name = outcome_var_name,
                      package = package,
                      parallel = parallel_training) # TRAINING
    
    if(verbose) cat("\nFinished training agent-level model on training data (all data but fold ", i, ").\n", sep="")
    msg <- paste0(msg,"\nFinished training agent-level model on training data (all data but fold ", i, ").\n")
    
    test <- data[data$fold_ass==i, ] # use just i for TESTING
    
    predict_test <- function(tuning_params, par){
      if (par){
        num_cores <- parallel::detectCores() - 1
        doParallel::registerDoParallel(cores = num_cores)
        
        foreach::`%dopar%`(foreach::foreach(x = sort(unique(data$group))), {
          if (x %in% sort(unique(test$group))){
            abm_simulate(model=model, features=features, 
                         parameters=as.numeric(agg_patterns[x, ]), # have to convert df row to numeric vector
                         time_len = tp[x],
                         tuning_parameters = tuning_params, 
                         iterations = iters,
                         STAT = STAT)
          } else {
            NA # foreaching through all unique(data$group) so result of predict_test is a list length of number of groups
          } # with results for each test$group stored in the element of the list corresponding to that test group
        }) # and everything else is an NA
      } else {
        foreach::`%do%`(foreach::foreach(x = sort(unique(data$group))), {
          if (x %in% sort(unique(test$group))){
            abm_simulate(model=model, features=features, 
                         parameters=as.numeric(agg_patterns[x, ]), # have to convert df row to numeric vector
                         time_len = tp[x],
                         tuning_parameters = tuning_params,  
                         iterations = iters,
                         STAT = STAT)
          } else {
            NA # foreaching through all unique(data$group) so result of predict_test is a list length of number of groups
          } # with results for each test$group stored in the element of the list corresponding to that test group
        }) # and everything else is an NA
      }
    }
    
    if(verbose) cat("Test data has ", nrow(test), " rows. And has groups ", paste(sort(unique(test$group)), collapse = ", "), ".\n", sep="")
    msg <- paste0(msg, "Test data has ", nrow(test), " rows. And has groups ", paste(sort(unique(test$group)), collapse = ", "), ".\n")
    
    if ((nrow(test) + nrow(training_data)) != nrow(data))
      stop("(nrow(test) + nrow(training_data)) != nrow(data). So, the function did not divide up the data into test and training right.")
    
    if (is.null(abm_vars$value)) {
      if (is.null(abm_vars$lower) | is.null(abm_vars$upper)) 
        stop("'abm_vars$lower' and/or 'abm_vars$upper' was NULL (missing) and 'abm_vars$value' was also missing. Either provide lower and upper or provide value.")
      
      if (verbose) cat("Starting to do ABM optimization with training data.\n") # TRAINING
      msg <- paste0(msg, "Starting to do ABM optimization with training data.\n")
      
      fitness <- function(parameter){
        abm_predicted <- rep(NA, nrow(agg_patterns))
        abm_dynamics <- matrix(NA, nrow=nrow(agg_patterns), ncol= tseries_len)
        for (u in unique(training_data$group)){ # make prediction for each of the groups 
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
        
        for (l in unique(training_data$group)){
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
                                                                                              .export = c("agg_patterns", "training_data",
                                                                                                          "abm_simulate", "model", "features",
                                                                                                          "compute_rmse", "fitness",
                                                                                                          "fitness_min"), .verbose = TRUE),
                                                                           NP = popSize, itermax = 20,
                                                                           steptol = 3))
        solution <- de_solution$optim$bestmem
      }
      
      if (verbose) cat("\nOptimal values of parameters are ", paste(solution, collapse = " ,"), ".\n", sep="")
      msg <- paste0(msg, "\nOptimal values of parameters are ", paste(solution, collapse = " ,"), ".\n")
      
      # build abm with predictive models trained on all data but i then predict on i data
      if (verbose) cat("Starting to do ABM simulations to predict test data.\n") # TESTING
      msg <- paste0(msg, "Starting to do ABM simulations to predict test data.\n")
      
      predicted_test <- predict_test(tuning_params = solution,
                                     par = predict_test_par)
      
      for (x in sort(unique(test$group))) {
        predicted_patterns[[x]]$predicted <- predicted_test[[x]]$action_avg
        predicted_patterns[[x]]$dynamics <- predicted_test[[x]]$dynamics
        predicted_patterns[[x]]$simdata <- predicted_test[[x]]$simdata
      }
      
    } else {
      
      predicted_test <- predict_test(tuning_params = abm_vars$value,
                                     par = predict_test_par)
      
      for(x in sort(unique(test$group))){ 
        predicted_patterns[[x]]$predicted <- predicted_test[[x]]$action_avg
        predicted_patterns[[x]]$dynamics <- predicted_test[[x]]$dynamics
        predicted_patterns[[x]]$simdata <- predicted_test[[x]]$simdata
      }
      
    }
    
    if (verbose) cat("Finished ABM simulations.\n")
    msg <- paste0(msg, "Finished ABM simulations.\n")
    
    for(x in sort(unique(test$group))){
      predicted_patterns[[x]]$actual <- agg_patterns[x, "action"]
      
      if(verbose) cat("Predicted: ", predicted_patterns[[x]]$predicted, ". Actual: ", predicted_patterns[[x]]$actual, ".\n", sep="")
      if(verbose) cat("Predicted Dynamics: ", paste(predicted_patterns[[x]]$dynamics[1:tp[x]], collapse = ", "), ".\n Actual Dynamics: ", 
                      paste(as.numeric(agg_patterns[x, which(names(agg_patterns) %in% paste(1:(tp[x])))]), collapse = ", "), ".\n", sep="")
      msg <- paste0(msg, "Predicted: ", predicted_patterns[[x]]$predicted, ". Actual: ", predicted_patterns[[x]]$actual, ".\n",
                    "Predicted Dynamics: ", paste(predicted_patterns[[x]]$dynamics[1:tp[x]], collapse = ", "), ".\n Actual Dynamics: ", 
                    paste(as.numeric(agg_patterns[x, which(names(agg_patterns) %in% paste(1:(tp[x])))]), collapse = ", "), ".\n")
    }
    
    if (saving) {
      if (verbose) cat("Saving file.\n")
      msg <- paste0(msg, "Saving file.\n")
      save(predicted_patterns, i, file = filename)
    }
  }
  
  new("cv_abm",
      call = call,
      predicted_patterns = predicted_patterns,
      timing = as.numeric(proc.time()[[3]]) - start_time,
      diagnostics = msg,
      session = sessionInfo())
}
