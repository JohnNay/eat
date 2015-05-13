#' Estimate an Agent Model
#'
#'\code{training} uses a \code{caret::train} or \code{stats::glm} algorithm to estimate an
#' individual-level model
#'
#' @param trainData data.frame with each row (obervational unit) being an individual decision. With 
#' a column called "group" specifying which group of \code{agg_patterns} each obseravtion is in.
#' @param parallel optional logical vector length one, default is \code{FALSE}.
#' @param cv_type optional character vector length one, default is \code{c("cv", "repeatedcv")}.
#' @inheritParams cv_abm
#'
#'@return Returns a \code{list} length \code{k}.

training <- function(trainData, features, Formula, k, 
                     sampling = FALSE, sampling_size = 1000, outcome_var_name = "my.decision",
                     package = c("caretglm", "caretglmnet", "glm", "caretnnet", "caretdnn"),
                     parallel = FALSE,
                     cv_type = c("cv", "repeatedcv")){
  # sampling == TRUE samples equal numbers of observations from each game structure
  
  package <- match.arg(package)
  cv_type <- match.arg(cv_type)
  
  if(parallel){
    num_cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = num_cores)
  }
  
  stopifnot(length(features) == k)
  # TODO: add error checking for terms in Formula == length(features) == k
    
  model <- as.list(rep(NA, k))
  
  if (sampling){
    training_index <- TRUE
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
  if(package=="caretglm"){
    
    if (k == 1){
      model[[k]] <- caret::train(
        eval(parse(text=Formula[[k]])), 
        data = trainData,
        method =  'glm',
        family =  binomial(link="logit"),
        trControl = caret::trainControl(method = "none")
      )
      cat("Done with model training.\n") 
    } else {
      for( i in seq(k)){
        if(i==k) {
          data_use <- cbind(trainData[trainData$period>=i, which(names(trainData) %in% features[[i]])],
                            my.decision = trainData[trainData$period>=i, which(names(trainData) %in% c("my.decision"))])
        } else {
          data_use <- cbind(trainData[trainData$period==i, which(names(trainData) %in% features[[i]])],
                            my.decision = trainData[trainData$period==i, which(names(trainData) %in% c("my.decision"))])
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
  } 
  ###############################################################################
  ###############################################################################
  if(package=="caretglmnet"){
    
    eGrid <- expand.grid(.alpha = (1:10) * 0.1, 
                         .lambda = seq(2, 0.005, by=-0.01))
    
    Control <- caret::trainControl(method = cv_type,
                            repeats = 3,
                            verboseIter =TRUE,
                            allowParallel=TRUE)
    for( i in seq(k)){
      if(i==k) {
        data_use <- cbind(trainData[trainData$period>=i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period>=i, which(names(trainData) %in% c("my.decision"))])
      } else {
        data_use <- cbind(trainData[trainData$period==i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period==i, which(names(trainData) %in% c("my.decision"))])
      }
      set.seed(77) # ensures that the same resampling sets are used, facilitates model comparison on the same data
      model[[i]] <- caret::train(
        eval(parse(text=Formula[[i]])), 
        data = data_use,
        method =  'glmnet',
        family =  "binomial",
        trControl = Control
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  } 
  ###############################################################################
  ###############################################################################
  if(package=="glm"){ # TODO: add in more methods here
    for( i in seq(k)){
      if(i==k) {
        data_use <- cbind(trainData[trainData$period>=i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period>=i, which(names(trainData) %in% c("my.decision"))])
      } else {
        data_use <- cbind(trainData[trainData$period==i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period==i, which(names(trainData) %in% c("my.decision"))])
      }
      model[[i]] <- glm(
        eval(parse(text=Formula[[i]])), family = binomial(link="logit"), data = data_use
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  } 
  ###############################################################################
  ###############################################################################
  if(package=="caretnnet"){
        ctrl <- caret::trainControl(
          method = cv_type,
          number = 8,
          allowParallel = TRUE,
          verboseIter = FALSE)
    for( i in seq(k)){
      if(i==k) {
        data_use <- cbind(trainData[trainData$period>=i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period>=i, which(names(trainData) %in% c("my.decision"))])
      } else {
        data_use <- cbind(trainData[trainData$period==i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period==i, which(names(trainData) %in% c("my.decision"))])
      }
      set.seed(77) # ensures that the same resampling sets are used, facilitates model comparison on the same data
      model[[i]] <- caret::train(
        x = data_use[ , which(names(data_use) %in% features[[i]])],
        y = data_use[ , which(names(data_use) %in% "my.decision")],
        maxit = 2500,
        method =  'avNNet',
        preProcess = c("center", "scale"),
        trControl = ctrl,
        tuneLength = 7
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  } 
  ###############################################################################
  ###############################################################################
  if(package=="caretdnn"){
    grid <- expand.grid(layer1 = 1:5,
                        layer2 = 1:3,
                        layer3 = 1:3)
    grid$hidden_dropout <- 0
    grid$visible_dropout <- 0

    ctrl <- caret::trainControl(
      method = cv_type,
      number = 10,
      allowParallel = TRUE,
      verboseIter = TRUE)
    for( i in seq(k)){
      if(i==k) {
        data_use <- cbind(trainData[trainData$period>=i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period>=i, which(names(trainData) %in% c("my.decision"))])
      } else {
        data_use <- cbind(trainData[trainData$period==i, which(names(trainData) %in% features[[i]])],
                          my.decision = trainData[trainData$period==i, which(names(trainData) %in% c("my.decision"))])
      }
      set.seed(77) # ensures that the same resampling sets are used, facilitates model comparison on the same data
      model[[i]] <- caret::train(
        x = data_use[ , which(names(data_use) %in% features[[i]])],
        y = data_use[ , which(names(data_use) %in% "my.decision")],
        preProcess = c("center", "scale"),
        trControl = ctrl,
        tuneGrid = grid,
        #tuneLength = 12,
        method =  'dnn',
        numepochs = 2500
      )
      cat("Done with", i, "out of", k, "models.\n") 
    }
  }
  ###############################################################################
  ###############################################################################
  model
}
