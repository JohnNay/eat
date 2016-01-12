#'Estimate an Agent Model with Supervised Learning
#'
#'\code{training} uses a \code{caret::train} or \code{stats::glm} algorithm to 
#'estimate an individual-level model and return a list where each element is 
#'either a list of with element of class "train" (if \code{caret::train} was 
#'used) or a list with each element of class "glm" if  (if \code{stat::glm} was 
#'used).
#'
#'@param trainData \code{data.frame} with each row (obervational unit) being an 
#'  individual agent decision. With a column called "group" specifying which 
#'  group of \code{agg_patterns} each obseravtion is in.
#'@param tune_length optional numeric vector length one specifying how many rows
#'  for caret::train to create in design matrix of hyper-parameter sets.
#'@param parallel optional logical vector length one, default is \code{FALSE}. 
#'  Uses \code{doParallel::registerDoParallel()}. Should be \code{FALSE} when
#'  training() is being called from inside cv_abm(), which, by default, is
#'  already running in parallel.
#'  @param cores optional numeric vector length one.
#'@param cv_type optional character vector length one, default is \code{c("cv", 
#'  "repeatedcv")}. Passed on to \code{caret::trainControl()}.
#'@inheritParams cv_abm
#'@inheritParams estimate_program
#'  
#'@return Returns a \code{list} length \code{k} where each element of the list 
#'  is an estimated model (estimated agent decision function).
#'  @export

training <- function(trainData, features, Formula,
                     sampling = FALSE, sampling_size = 1000,
                     package = c("caretglm", "caretglmnet", "glm", "caretnnet", "caretdnn",
                                 "estimate_program"),
                     tune_length = 10, mins = 10,
                     parallel = FALSE, cores = NULL,
                     cv_type = c("cv", "repeatedcv")){
  # if sampling == TRUE, samples equal numbers of observations from each game structure
  
  package <- match.arg(package)
  cv_type <- match.arg(cv_type)
  
  if(parallel){
    num_cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = num_cores)
  }
  
  if(!(identical(length(features), length(Formula))))
    stop("identical(length(features), length(Formula)) should be TRUE, but it's FALSE.")
  
  k <- length(Formula)
  
  stopifnot(identical(length(features), as.integer(k)))
  # TODO: add error checking for terms in Formula == length(features) == k
    
  model <- as.list(rep(NA, k))
  
  if (sampling){
    training_index <- TRUE
    outcome_var_name <- all.vars(eval(parse(text=Formula[[1]])))[1]
    for(i in unique(trainData$group)){
      prop_for_train <- 1/(nrow(trainData[trainData$group==i, ])/sampling_size)
      train_index <- caret::createDataPartition(trainData[trainData$group==i, outcome_var_name], 
                                                p = prop_for_train, list=FALSE)
      training_index <- append(training_index, seq(nrow(trainData[trainData$group==i, ])) %in% train_index)
    }
    training_index <- training_index[-1]
    trainData <- trainData[training_index, ]
  }
  
  ###############################################################################
  ###############################################################################
  if(package=="glm"){
    for( i in seq(k)){
      if(identical(i,as.integer(as.integer(k)))) {
        data_use <- trainData[trainData$period>=i, ]
      } else {
        data_use <- trainData[trainData$period==i, ]
      }
      model[[i]] <- glm(
        eval(parse(text=Formula[[i]])), family = binomial(link="logit"), data = data_use
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  } 
  ###############################################################################
  ###############################################################################
  if(package=="caretglm"){
    for( i in seq(k)){
      if(identical(i,as.integer(k))) {
        data_use <- trainData[trainData$period>=i, ]
      } else {
        data_use <- trainData[trainData$period==i, ]
      }
      set.seed(77) # ensures that the same resampling sets are used, facilitates model comparison on the same data
      model[[i]] <- caret::train(
        eval(parse(text=Formula[[i]])), 
        data=data_use,
        method =  'glm',
        family =  binomial(link="logit"),
        trControl = caret::trainControl(method = "none")
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  }
  ###############################################################################
  ###############################################################################
  if(package=="caretglmnet"){
    
#     eGrid <- expand.grid(.alpha = (1:10) * 0.1, 
#                          .lambda = seq(2, 0.005, by=-0.01))
    
    Control <- caret::trainControl(method = cv_type,
                            repeats = 3,
                            verboseIter =TRUE,
                            allowParallel= parallel)
    for(i in seq(k)){
      if(identical(i,as.integer(k))) {
        data_use <- trainData[trainData$period>=i, ]
      } else {
        data_use <- trainData[trainData$period==i, ]
      }
      set.seed(77) # ensures that the same resampling sets are used, facilitates model comparison on the same data
      model[[i]] <- caret::train(
        eval(parse(text=Formula[[i]])), 
        data = data_use,
        method =  'glmnet',
        family =  "binomial",
        tuneLength = tune_length,
        trControl = Control
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  } 
  ###############################################################################
  ###############################################################################
  if(package=="caretnnet"){
        ctrl <- caret::trainControl(
          method = cv_type,
          number = 10,
          allowParallel = parallel,
          verboseIter = FALSE)
        
    for( i in seq(k)){
      if(identical(i,as.integer(k))) {
        data_use <- trainData[trainData$period>=i, ]
      } else {
        data_use <- trainData[trainData$period==i, ]
      }
      set.seed(77) # ensures that the same resampling sets are used, facilitates model comparison on the same data
      model[[i]] <- caret::train(
        x = data_use[ , which(names(data_use) %in% features[[i]])],
        y = data_use[ , which(names(data_use) %in% "my.decision")],
        maxit = 2500,
        method =  'avNNet',
        preProcess = c("center", "scale"),
        trControl = ctrl,
        tuneLength = tune_length
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  } 
  ###############################################################################
  ###############################################################################
  if(package=="caretdnn"){
#     grid <- expand.grid(layer1 = 1:5,
#                         layer2 = 1:3,
#                         layer3 = 1:3)
#     grid$hidden_dropout <- 0
#     grid$visible_dropout <- 0

    ctrl <- caret::trainControl(
      method = cv_type,
      number = 10,
      allowParallel = parallel,
      verboseIter = TRUE)
    
    for( i in seq(k)){
      if(identical(i,as.integer(k))) {
        data_use <- trainData[trainData$period>=i, ]
      } else {
        data_use <- trainData[trainData$period==i, ]
      }
      set.seed(77) # ensures that the same resampling sets are used, facilitates model comparison on the same data
      model[[i]] <- caret::train(
        x = data_use[ , which(names(data_use) %in% features[[i]])],
        y = data_use[ , which(names(data_use) %in% "my.decision")],
        preProcess = c("center", "scale"),
        trControl = ctrl,
        #tuneGrid = grid,
        tuneLength = tune_length,
        method =  'dnn',
        numepochs = 2500
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  }
  ###############################################################################
  ###############################################################################
  if(package=="estimate_program"){
    for( i in seq(k)){
      trainData <- trainData
      if(identical(i,as.integer(k))) {
        data_use <- trainData[trainData$period>=i, ]
      } else {
        data_use <- trainData[trainData$period==i, ]
      }
      model[[i]] <- estimate_program(
        formula = eval(parse(text=Formula[[i]])),
        data = data_use,
        loss = "log_lik",
        link = "logit",
        mins = mins,
        parallel = parallel,
        cores = cores
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  }
  ###############################################################################
  ###############################################################################
  if(!identical(length(model),as.integer(k)))
    stop("length(model) != k so list being returned is not the right length.")
  
  model
}
