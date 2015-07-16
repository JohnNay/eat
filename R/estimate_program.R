#' Pick one of the (inverse) link functions
#' 
#' These are functions that map the real number line to the (0,1) interval.
#' 
#' @param link Character vector length one specifying which link function you 
#'   want. \code{logit}, \code{probit}, \code{cauchit} (logistic, normal and 
#'   Cauchy CDFs respectively).
#'   
#' @return Returns a function that has one argument (Numeric vector length one),
#'   returns a Numeric vector length one.
#'   
#' @examples
#' create_link_func("logit")
#' create_link_func("probit")
#' create_link_func("cauchit")
#' 
#' @export
create_link_func <- function(link){
  switch(link,
         logit = stats::binomial(link = "logit")$linkinv,
         probit = stats::binomial(link = "probit")$linkinv,
         cauchit = stats::binomial(link = "cauchit")$linkinv
  )
}

#' Determine the class of the outcome
#' 
#' @inheritParams estimate_program
#'   
#' Trying to determine if this is a > 2 class problem.
#'   
#' @return Returns a list with two elements: \code{outcome_class} and
#'   \code{num_facs}.
#'   
#' @export
what_outcome <- function(data){
  num_facs <- 0
  
  if (class(data[, which(colnames(data) %in% "outcome")]) == "factor"){
    num_facs <- length(levels(data[, which(colnames(data) %in% "outcome")]))
    
    if(length(levels(data[, which(colnames(data) %in% "outcome")])) > 2){
      outcome_class <- "multi_factor"
    } else {
      outcome_class <- "factor"
    }
  } else {
    outcome_class <- class(data[, which(colnames(data) %in% "outcome")])
  }
  
  list(outcome_class = outcome_class, 
       num_facs = num_facs)
}

#' Create a fitness function to estimate a program (R function)
#' 
#' @param loss_function function resulting from a call to 
#'   \code{\link{create_loss_func}}.
#' @inheritParams estimate_program
#'   
#' @return Returns a function that takes in a function as an argument and then 
#'   assigns it a numeric fitness value we want to minimize. Use this in
#'   \code{\link{estimate_program}}.
#'   
#' @export
create_fit_func <- function(loss_function, data, parallel){
  
  if(parallel) {
    forloop <- foreach::`%dopar%`
  } else {
    forloop <- foreach::`%do%`
  }
  
  # Meta-data about the predictor variables:
  meta <- data.frame(vars = colnames(data)[-which(colnames(data) %in% "outcome")], 
                     types = sapply(data, class)[-which(colnames(data) %in% "outcome")])
  # make sure everything is a character vec
  meta[] <- lapply(meta, as.character)
  
  num_facs <- what_outcome(data)$num_facs
  num_predictors <- ncol(data) - 1
  # For each of these, there is num_facs-dependent initialization of 
  # pfunc output, which should be a container for each output, 
  # with as many slots as there are observations in the data.
  
  if(num_predictors == 4){
    fit_func <- function(pfunc){
      out <- forloop(foreach::foreach(i=seq(nrow(data)), .combine='rbind'), {
        pfunc(data[i, 1], data[i, 2], data[i, 3], data[i, 4])
      })
      
      if (num_facs == 1) out <- as.vector(out)
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = data[, which(colnames(data) %in% "outcome")],
                    prediction = out)
    }
    ## INPUT VARIABLE SET 
    input_set <- rgp::inputVariableSet(rgp::`%::%`(meta[1,1], rgp::st(meta[1,2])),
                                       rgp::`%::%`(meta[2,1], rgp::st(meta[2,2])),
                                       rgp::`%::%`(meta[3,1], rgp::st(meta[3,2])),
                                       rgp::`%::%`(meta[4,1], rgp::st(meta[4,2])))
  }
  if(num_predictors == 3){
    fit_func <- function(pfunc){
      out <- forloop(foreach::foreach(i=seq(nrow(data)), .combine='rbind'), {
        pfunc(data[i, 1], data[i, 2], data[i, 3])
      })
      
      if (num_facs == 1) out <- as.vector(out)
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = data[, which(colnames(data) %in% "outcome")],
                    prediction = out)
    }
    ## INPUT VARIABLE SET 
    input_set <- rgp::inputVariableSet(rgp::`%::%`(meta[1,1], rgp::st(meta[1,2])),
                                       rgp::`%::%`(meta[2,1], rgp::st(meta[2,2])),
                                       rgp::`%::%`(meta[3,1], rgp::st(meta[3,2])))
  }
  if(num_predictors == 2){
    fit_func <- function(pfunc){
      out <- forloop(foreach::foreach(i=seq(nrow(data)), .combine='rbind'), {
        pfunc(data[i, 1], data[i, 2])
      })
      
      if (num_facs == 1) out <- as.vector(out)
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = data[, which(colnames(data) %in% "outcome")],
                    prediction = out)
    }
    ## INPUT VARIABLE SET 
    input_set <- rgp::inputVariableSet(rgp::`%::%`(meta[1,1], rgp::st(meta[1,2])),
                                       rgp::`%::%`(meta[2,1], rgp::st(meta[2,2])))
  }
  if(num_predictors == 1){
    fit_func <- function(pfunc){
      out <- forloop(foreach::foreach(i=seq(nrow(data)), .combine='rbind'), {
        pfunc(data[i, 1])
      })
      
      if (num_facs == 1) out <- as.vector(out)
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = data[, which(colnames(data) %in% "outcome")],
                    prediction = out)
    }
    ## INPUT VARIABLE SET 
    input_set <- rgp::inputVariableSet(rgp::`%::%`(meta[1,1], rgp::st(meta[1,2])))
  }
  
  list(fit_func = fit_func, input_set = input_set)
}


#'Estimate a program (R function) from data.
#'
#'\code{estimate_program} uses \code{rgp}'s \code{rgp::typedGeneticProgramming}.
#'
#'@param data data.frame where (i.) the last column is named \code{"outcome"} 
#'  and has the outcome variable we are evolving programs to predict; (ii.) all 
#'  other columns (there must be at least one other column) that precede the 
#'  \code{"outcome"} column are named columns containing the predictor 
#'  variables, which can be of any type. The order matters: when using the 
#'  evolved program for predictions the (named) arguments to the function will 
#'  be the predictor variables in the order they are supplied to this 
#'  \code{estimate_program()} function.
#'@param loss Optional Character vector length one
#'@param link Optional Character vector length one
#'@param mins Optional Integer vector length one
#'@param parallel Optional Logical vector length one. Default is \code{parallel 
#'  = FALSE}; \code{parallel = TRUE} can be slower if the data set is small 
#'  relative to the numner of population evolutions desired
#'@param cores Optional Integer vector length one
#'  
#'@return The function returns an S4 object. See \linkS4class{estimate_program} 
#'  for the details of the \code{slots} (objects) that this type of object will 
#'  have.
#'  
#'@importFrom rgp %->% %::% st
#'  
#'@export
estimate_program <- function(data, 
                             loss = c("log_lik", "rmse", "identity", "identity_multi_class"),
                             link = c("logit", "probit", "cauchit", "identity"),
                             mins = 2,
                             parallel = FALSE, 
                             cores = NULL){
  
  start_time <- as.numeric(proc.time()[[3]])
  call <- match.call()
  
  loss <- match.arg(loss) 
  loss_function <- create_loss_func(loss)
  link <- match.arg(link)
  link <- create_link_func(link)
  
  if (parallel) {
    if (missing(cores)) cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = cores)
  } # without registering the backend the %dopar% should just run sequentially as %do%
  
  if (loss == "log_lik"){
    ## CONSTANT SET
    # Would much rather not do global bindings. But because of how the rgp package authors
    # forced use of the globalenv for location of the constant set, I have to.
    e <- globalenv()
    e$booleanConstantFactory <- function() runif(1) > .5
    e$numericConstantFactory <- function() runif(1, 0.1, 0.9)
    #e$integerConstantFactory <- function() round(runif(1, 1, 100))
    #e$integerVecConstantFactory <- function() round(runif(3, 1, 100))
    e$probConstantFactory <- function() runif(1, 0, 1)
    
    constant_set <- rgp::constantFactorySet("booleanConstantFactory" %::% (list() %->% st("logical")),
                                            "numericConstantFactory" %::% (list() %->% st("numeric")),
                                            "probConstantFactory" %::% (list() %->% st("prob"))
                                            #"integerConstantFactory" %::% (list() %->% st("integer"))
                                            #"integerVecConstantFactory" %::% (list() %->% st("3integers"))
    )
    
    ## FUNCTION SET
    one_rnorm <- function(.mean) rnorm(1, .mean, 1)
    # fac1 <- function(.logical) if(.logical) unique(d$outcome)[1] else sample(unique(d$outcome)[-1], 1)
    # fac2 <- function(.logical) if(.logical) unique(d$outcome)[2] else sample(unique(d$outcome)[-2], 1)
    # fac3 <- function(.logical) if(.logical) unique(d$outcome)[3] else sample(unique(d$outcome)[-3], 1)
    ifelse2 <- function(cond, opt1, opt2) if(cond) opt1 else opt2
    round2 <- function(x) base::round(x)
    create_vec <- function(x,y,z) sapply(c(x,y,z), round)
    
    function_set <- rgp::functionSet(
      # Math
      "+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "/" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "exp" %::% (list(st("numeric")) %->% st("numeric")),
      ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
      "<" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
      # Logical
      "&" %::% (list(st("logical"), st("logical")) %->% st("logical")),
      "|" %::% (list(st("logical"), st("logical")) %->% st("logical")),
      "!" %::% (list(st("logical")) %->% st("logical")),
      rgp::pw("ifelse2" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric")), 0.5),
      # Randomness
      rgp::pw("one_rnorm" %::% (list(st("numeric")) %->% st("numeric")), 2.5),
      # Utilities
      #"create_vec" %::% (list(st("numeric"), st("numeric"), st("numeric")) %->% st("3integers")),
      #"round2" %::% (list(st("numeric")) %->% st("integer")),
      "link" %::% (list(st("numeric")) %->% st("prob")),
      parentEnvironmentLevel = 1
    )
    
    type <-  rgp::st("prob")
  }
  if (loss == "rmse"){
    ## CONSTANT SET
    e <- globalenv()
    e$booleanConstantFactory <- function() runif(1) > .5
    e$numericConstantFactory <- function() runif(1, 0.1, 0.9)
    
    constant_set <- rgp::constantFactorySet("booleanConstantFactory" %::% (list() %->% st("logical")),
                                            "numericConstantFactory" %::% (list() %->% st("numeric"))
                                            #"integerConstantFactory" %::% (list() %->% st("integer"))
                                            #"integerVecConstantFactory" %::% (list() %->% st("3integers"))
    )
    
    ## FUNCTION SET
    one_rnorm <- function(.mean) rnorm(1, .mean, 1)
    # fac1 <- function(.logical) if(.logical) unique(d$outcome)[1] else sample(unique(d$outcome)[-1], 1)
    # fac2 <- function(.logical) if(.logical) unique(d$outcome)[2] else sample(unique(d$outcome)[-2], 1)
    # fac3 <- function(.logical) if(.logical) unique(d$outcome)[3] else sample(unique(d$outcome)[-3], 1)
    ifelse2 <- function(cond, opt1, opt2) if(cond) opt1 else opt2
    round2 <- function(x) base::round(x)
    create_vec <- function(x,y,z) sapply(c(x,y,z), round)
    
    function_set <- rgp::functionSet(
      # Math
      "+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "/" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
      "exp" %::% (list(st("numeric")) %->% st("numeric")),
      ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
      "<" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
      # Logical
      "&" %::% (list(st("logical"), st("logical")) %->% st("logical")),
      "|" %::% (list(st("logical"), st("logical")) %->% st("logical")),
      "!" %::% (list(st("logical")) %->% st("logical")),
      rgp::pw("ifelse2" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric")), 0.5),
      # Randomness
      rgp::pw("one_rnorm" %::% (list(st("numeric")) %->% st("numeric")), 2.5),
      # Utilities
      #"create_vec" %::% (list(st("numeric"), st("numeric"), st("numeric")) %->% st("3integers")),
      #"round2" %::% (list(st("numeric")) %->% st("integer")),
      parentEnvironmentLevel = 1
    )
    
    type <-  rgp::st("numeric")
  }
  
  ## Prep for EVOLUTION
  fit_func <- create_fit_func(loss_function = loss_function,
                              data = data, parallel = parallel)$fit_func
  input_set <- create_fit_func(loss_function = loss_function,
                               data = data, parallel = parallel)$input_set
  
  ## Do EVOLUTION
  full <- rgp::typedGeneticProgramming(fitnessFunction = fit_func, 
                               type = type,
                               functionSet = function_set,
                               inputVariables = input_set,
                               constantSet = constant_set,
                               stopCondition = rgp::makeTimeStopCondition(mins*60))
  # We minimize loss, so the lowest fitness value is the best:
  best_func <- full$population[[which.min(full$fitnessValues)]] 
  
  new("estimate_program",
      call = call,
      timing = as.numeric(proc.time()[[3]]) - start_time,
      session = sessionInfo(),
      full = full,
      best_func = best_func)
}

# data("iris")
# d <- iris
# names(d)[which(names(d) == "Species")] <- "outcome" 
# # Convert it to integer:
# d$outcome <- as.integer(d$outcome)
# # Convert it to a two-class problem:
# d <- d[!d$outcome==3, ]
# d$outcome[d$outcome!=1] <- 0
# 
# res1 <- estimate_program(data = d, 
#                          loss = "log_lik",
#                          link = "probit",
#                          mins = 5,
#                          parallel = FALSE)
# bestFunction1 <- res1@best_func
# bestFunction1 # It has named arguments, but can use positions, if we want.
# # Want this to be close to 1:
# obs <- 1; bestFunction1(Sepal.Length = d[obs, 1], Sepal.Width = d[obs, 2], 
#                         Petal.Length = d[obs, 3], Petal.Width = d[obs, 4])
# # Want this to be close to 0:
# obs <- 100; bestFunction1(Sepal.Length = d[obs, 1], Sepal.Width = d[obs, 2], 
#                           Petal.Length = d[obs, 3], Petal.Width = d[obs, 4])
# # Can also use the estimated program without named arguments:
# obs <- 100; bestFunction1(d[obs, 1], d[obs, 2], d[obs, 3], d[obs, 4])
# # Because this often evolves a probabilistic function, we can replicate it many times 
# # to get a sense of the function:
# plot(density(replicate(1000, 
#                {obs <- 1; bestFunction1(d[obs, 1], d[obs, 2], d[obs, 3], d[obs, 4])})))


