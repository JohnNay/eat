#' Estimate and Test an ABM
#'
#'\code{cv_abm} uses cross-validation to test an ABM's predictive power.
#'
#'The function returns an S4 object.
#' See \linkS4class{cv_abm} for the details of the slots
#'(objects) that this type of object will have.
#'
#'@param data data.frame full dataset with all individual decisions with 
# a column called "group" specifying which group of agg_patterns each obseravtion is in.
#' @param features list
#' @param Formula list
#' @param k numeric vector length one
#' @param agg_patterns data.frame
#' @param abm_simulate function with model, features, and parameters args
#' @param tseries_len optional numeric vector length one
#' @param package optional charac vector length one: "caretglm", "caretglmnet", "glm", "caretnnet", "caretdnn"
#' @param sampling optional logical vector, FALSE
#' @param STAT optional charac vector length one: "mean", "median"
#' @param saving = FALSE
#' @param filename = NULL
#' @param abm_vars a list if not null
#' @param abm_optim = c("GA", "DE")
#' @param validate = c("lgocv", "cv")
#' @param folds = ifelse(validate == "lgocv", max(data$group), 10)
#' @param repeat_cv = 1
#' @param drop_nzv = TRUE
#' @param verbose = TRUE
#' @param predict_test_par = FALSE
#'
#'@return Returns an S4 object of class cv_abm.
#'
#' @examples
#' # Create data:
#'cdata <- data.frame(period = rep(1:10, 1000),
#'                    outcome = rep(1:2, 5000),
#'                    my.decision1 = sample(1:0, 10000, TRUE),
#'                    other.decision1 = sample(1:0, 10000, TRUE))
#' # Create ABM
#' simulate_abm <- function(model, features, parameters, tseries_len, noise, threshold = 0.5, iterations = 1250, STAT = "mean"){
#'  matrixOut <- data.frame()
#'  list(dynamics = dynamics, action_avg = action_avg, simdata = matrixOut)
#'  } 
#'
#'
#'@export

cv_abm <- function(data, features, Formula, k, agg_patterns,
                   abm_simulate, # function with model, features, and parameters args
                   tseries_len = 8,
                   package = c("caretglm", "caretglmnet", "glm", "caretnnet", "caretdnn"),
                   sampling = FALSE,
                   STAT = c("mean", "median"),
                   saving = FALSE, filename = NULL,
                   abm_vars  = NULL, # a list if not null
                   abm_optim = c("GA", "DE"), 
                   validate = c("lgocv", "cv"), 
                   folds = ifelse(validate == "lgocv", max(data$group), 10), 
                   repeat_cv = 1, # TODO: add repeat_cv functionality
                   drop_nzv = TRUE, 
                   verbose = TRUE,
                   predict_test_par = FALSE){
  # sampling == TRUE samples equal numbers of observations from each game structure
  # @agg_patterns data.frame with the global game params for each of the 16 games
  
  # iterations= parameter[1]*15000 inside the fitness() for GA optimization
  # because more noise (parameter[1]), more iterations are needed to determine how good that param set is
  if(saving & missing(filename)) stop("If you are saving the file you need to supply a 'filename'.")
  
  msg <- ""
  start_time <- as.numeric(proc.time()[[1]])
  call <- match.call()
  
  
  abm_optim <- match.arg(abm_optim)
  validate <- match.arg(validate)
  package <- match.arg(package)
  STAT <- match.arg(STAT)
  
  predicted_patterns <- lapply(as.list(rep(NA, max(data$group))), 
                           function(x) list(predicted=NA, actual=NA, null_model=NA,
                                            dynamics=rep(NA, tseries_len), simdata=data.frame()))
  
  #     group_folds <- sample(seq(folds), length(unique(data$group)), replace=TRUE) # try to allocate each obs to one of k folds
  #     while(length(unique(group_folds)) != folds){
  #       group_folds <- sample(seq(folds), length(unique(data$group)), replace=TRUE) # keep trying to allocate each obs to one of k folds
  #     }
  group_folds <- caret::createFolds(y = unique(data$group), k = folds, list = FALSE)
  
  if(length(group_folds) != length(unique(data$group))) stop("Assignment of groups to folds didnt work.")
  if(length(unique(group_folds)) != folds) stop("Assignment of groups to folds didnt work.")
  if(verbose) cat("Group folds:", group_folds ,"\n")
  msg <- paste0(msg, "Group folds:", group_folds ,"\n")
  
  # create a vector same length as data with assignments of each row to a fold:
  fold_ass <- rep(NA, nrow(data))
  for (s in seq(nrow(data))) fold_ass[s] <- group_folds[data[s, "group"]]
  if(length(fold_ass) != nrow(data)) stop("Creating a vector same length as data with assignments of each row to a fold didnt work.")
  data <- cbind(data, fold_ass = fold_ass)
  
  # In the ith fold, the elements of folds that equal i are in the test set, and the remainder are in the training set.
  for(i in seq(folds)){ #for (i in sort(unique(data$group))){
    if(verbose) cat("\n--------- + Starting with training data (all data but fold ", i, "). + ---------\n", sep="")
    msg <- paste0(msg, "\n--------- + Starting with training data (all data but fold ", i, "). + ---------\n")
    training_data <- data[data$fold_ass!=i, ] # use all data but i for TRAINING
    
    training_features <- features
    training_Formula <- Formula
    
    nzvs <- caret::nearZeroVar(training_data[ , which(names(training_data) %in% features[[k]])], freqCut = 95/5, uniqueCut=10) 
    if (length(nzvs) > 0){
      to_drop <- colnames(training_data)[which(names(training_data) %in% features[[k]])[nzvs]]
      if(verbose) cat("We should be dropping", length(to_drop), "feature(s), which is (are):", to_drop, "\n")
      msg <- paste0(msg, "We should be dropping", length(to_drop), "feature(s), which is (are):", to_drop, "\n")
      
      if(drop_nzv){
        # just names in features[[k]] so we dont drop group, folds and training vars
        if(verbose) cat("Dropping", length(to_drop), "feature(s), which is (are):", to_drop, "\n")
        msg <- paste0(msg, "Dropping", length(to_drop), "feature(s), which is (are):", to_drop, "\n")
        
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
            msg <- paste0(msg, "New training formula:", training_Formula[[m]])
          }
        }
        
      }
    }
    
    if(verbose) cat("Training data has ", nrow(training_data), " rows. And has groups ", sort(unique(training_data$group)), ".\n", sep="")
    msg <- paste0(msg, "Training data has ", nrow(training_data), " rows. And has groups ", sort(unique(training_data$group)), ".\n")
                  
    model <- training(training_data, features, training_Formula, k, sampling = sampling, package = package) # TRAINING
    
    if(verbose) cat("\nFinished training model on training data (all data but fold ", i, ").\n", sep="")
    msg <- paste0(msg,"\nFinished training model on training data (all data but fold ", i, ").\n")
                  
    test <- data[data$fold_ass==i, ] # use just i for TESTING
    
    predict_test <- function(noise1, threshold1, par){
        if (par){
        
        foreach::`%dopar%`(foreach::foreach(x = sort(unique(data$group))), {
          if (x %in% sort(unique(test$group))){
            abm_simulate(model=model, features=features, 
                         parameters=as.numeric(agg_patterns[x, ]), # have to convert df row to numeric vector
                         time_len = tseries_len,
                         noise = noise1, threshold = threshold1, iterations=35000,
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
                         time_len = tseries_len,
                         noise = noise1, threshold = threshold1, iterations=35000,
                         STAT = STAT)
          } else {
            NA # foreaching through all unique(data$group) so result of predict_test is a list length of number of groups
          } # with results for each test$group stored in the element of the list corresponding to that test group
        }) # and everything else is an NA
      }
    }
    
    if(verbose) cat("Test data has ", nrow(test), " rows. And has groups ", sort(unique(test$group)), ".\n", sep="")
    msg <- paste0(msg, "Test data has ", nrow(test), " rows. And has groups ", sort(unique(test$group)), ".\n")
                  
    stopifnot((nrow(test) + nrow(training_data)) == nrow(data))
    
    if(missing(abm_vars)) {
      if(verbose) cat("Starting to do ABM optimization with training data.\n") # TRAINING
      msg <- paste0(msg, "Starting to do ABM optimization with training data.\n")
      
      squared_loss <- function(x, s) sqrt(mean((x - s)^2, na.rm=TRUE))
      
      fitness <- function(parameter){
        abm_predicted <- rep(NA, nrow(agg_patterns))
        abm_dynamics <- matrix(NA, nrow=nrow(agg_patterns), ncol= tseries_len)
        for (u in unique(training_data$group)){ # make prediction for each of the groups 
          abm_results <- abm_simulate(model= model, features = features, 
                                      parameters = as.numeric(agg_patterns[u, ]),
                                      time_len = tseries_len,
                                      noise = parameter[1], threshold = parameter[2], iterations = parameter[1]*17000,
                                      STAT = STAT) 
          if(is.na(abm_results$action_avg)) 
            stop(paste("Fitness function tried to return an NA value for avg action of group", u, "with param values as", parameter))
          if(any(is.na(abm_results$dynamics))) 
            stop(paste("Fitness function tried to return an NA value for dynamic action of group", u, 
                       "at time", which(is.na(abm_results$dynamics)) , "with param values as", parameter))
          abm_predicted[u] <- abm_results$action_avg
          abm_dynamics[u, ] <- abm_results$dynamics
        }
        # leaving out groups in vec unique(test$group) in this comparison because it not used for the training, its the test.
        avg_action_error <- squared_loss(abm_predicted[-unique(test$group)], agg_patterns[-unique(test$group), which(names(agg_patterns) %in% "action")]) 
        if(is.na(avg_action_error)) 
          stop(paste("Fitness function tried to return an NA value for avg action error with param values as", parameter))
        dynamic_action_error <- rep(NA, nrow(agg_patterns))
        for (l in unique(training_data$group)){
          dynamic_action_error[l] <- squared_loss(abm_dynamics[l, ], 
                                                as.numeric(agg_patterns[l, which(names(agg_patterns) %in% paste(1:tseries_len))]))
        }
        if(any(is.na(dynamic_action_error[-unique(test$group)])))
          stop(paste("Fitness function tried to return an NA value for dynamic action error with param values as", parameter))
        result <- -(avg_action_error + mean(dynamic_action_error[-unique(test$group)], na.rm=TRUE)) # MSE of the avg action + mean of the MSE's of the time series of all training data groups
        if(is.na(result)) stop(paste("Fitness function tried to return an NA value with param values as", parameter))
        if(length(result) != 1) stop(paste("Fitness function returned a result that is not of length one with param values as", parameter))
        result
      }
      
      num_cores <- parallel::detectCores() - 1
      popSize <- ifelse(num_cores > 20, num_cores, 21)
      
      doParallel::registerDoParallel(cores = num_cores)
      
      if(abm_optim == "GA"){
        ga_solution <- GA::ga(type = "real-valued", fitness = fitness,
                              min = c(0.25, 0.46), max = c(0.35, 0.51),
                              popSize = popSize, run = 3, maxfitness = 0,
                              names = c("noise", "threshold"),
                              parallel = TRUE)
        solution <- ga_solution@solution[1, ]
      }
      if(abm_optim == "DE"){
        fitness_min <- function(parameter) -fitness(parameter) # this optimization routine minimizes objective function
        de_solution <- DEoptim::DEoptim(fitness_min, lower = c(0.2, 0.45), upper = c(0.4, 0.55), 
                                        control = DEoptim::DEoptim.control(parallelType = 2,
                                                                           foreachArgs = list(.packages = c("caret", "dplyr", "DEoptim"),
                                                                                              .export = c("agg_patterns", "training_data",
                                                                                                          "abm_simulate", "model", "features",
                                                                                                          "squared_loss", "fitness",
                                                                                                          "fitness_min"), .verbose = TRUE),
                                                                           NP = popSize, itermax = 20,
                                                                           steptol = 3))
        solution <- de_solution$optim$bestmem
      }
      
      if(verbose) cat("\nOptimal value of noise is ", solution[1], ". Optimal value of threshold is ", solution[2],".\n", sep="")
      msg <- paste0(msg,"\nOptimal value of noise is ", solution[1], ". Optimal value of threshold is ", solution[2],".\n")
                    
      # build abm with predictive models trained on all data but i then predict on i data
      if(verbose) cat("Starting to do ABM simulations to predict test data.\n") # TESTING
      msg <- paste0(msg, "Starting to do ABM simulations to predict test data.\n")
      
      num_cores <- parallel::detectCores() - 1
      doParallel::registerDoParallel(cores = num_cores)
      predicted_test <- predict_test(noise1 = solution[1], threshold1 = solution[2],
                                     par = predict_test_par)
      
      for(x in sort(unique(test$group))){
        predicted_patterns[[x]]$predicted <- predicted_test[[x]]$action_avg
        predicted_patterns[[x]]$dynamics <- predicted_test[[x]]$dynamics
        predicted_patterns[[x]]$simdata <- predicted_test[[x]]$simdata
      }
      
    } else {
      
      doParallel::registerDoParallel(cores = parallel::detectCores()-1)
      predicted_test <- predict_test(noise1 = abm_vars$noise, threshold1 = abm_vars$threshold,
                                     par = predict_test_par)
      
      for(x in sort(unique(test$group))){ 
        predicted_patterns[[x]]$predicted <- predicted_test[[x]]$action_avg
        predicted_patterns[[x]]$dynamics <- predicted_test[[x]]$dynamics
        predicted_patterns[[x]]$simdata <- predicted_test[[x]]$simdata
      }
      
    }
    
    if(verbose) cat("Finished ABM simulations.\n")
    msg <- paste0(msg, "Finished ABM simulations.\n")
    
    for(x in sort(unique(test$group))){
      predicted_patterns[[x]]$actual <- mean(ifelse(test[test$group==x, ]$my.decision=="action", 1, 0))
      
      if(verbose) cat("Predicted: ", predicted_patterns[[x]]$predicted, ". Actual: ", predicted_patterns[[x]]$actual, ".\n", sep="")
      if(verbose) cat("Predicted Dynamics: ", predicted_patterns[[x]]$dynamics, ".\n Actual Dynamics: ", 
                      as.numeric(agg_patterns[x, which(names(agg_patterns) %in% paste(1:tseries_len))]), ".\n", sep="")
      if(verbose) cat("Null model predictions.\n")
      msg <- paste0(msg, "Predicted: ", predicted_patterns[[x]]$predicted, ". Actual: ", predicted_patterns[[x]]$actual, ".\n",
                    "Predicted Dynamics: ", predicted_patterns[[x]]$dynamics, ".\n Actual Dynamics: ", 
                    as.numeric(agg_patterns[x, which(names(agg_patterns) %in% paste(1:tseries_len))]), ".\n",
                    "Null model predictions.\n")
      
      predicted_patterns[[x]]$null_model <- mean(ifelse(training_data$my.decision=="action", 1, 0))
    }
    
    if (saving) {
      if(verbose) cat("Saving file.\n")
      msg <- paste0(msg, "Saving file.\n")
      save(predicted_patterns, file=filename)
    }
  }
  
  object <- new("cv_abm",
                call = call,
                predicted_patterns = predicted_patterns,
                timing = as.numeric(proc.time()[[1]]) - start_time,
                diagnostics = msg)
  
  object
}
