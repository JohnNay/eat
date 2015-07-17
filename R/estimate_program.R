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
#' @param y outcome vector
#'   
#' Trying to determine if this is a > 2 class problem.
#'   
#' @return Returns a list with two elements: \code{outcome_class} and
#'   \code{levels}.
#'   
#' @export
what_outcome <- function(y){
  levels <- 1
  
  if (class(y)=="factor"){
    levels <- length(levels(y))
    
    if(length(levels(y)) > 2){
      outcome_class <- "multi_factor"
    } else {
      outcome_class <- "factor"
    }
  } else {
    outcome_class <- class(y)
  }
  
  list(outcome_class = outcome_class, 
       levels = levels)
}

#' Create a fitness function to estimate a program (R function)
#' 
#' @param loss_function function resulting from a call to 
#'   \code{\link{create_loss_func}}.
#' @param X model.matrix
#' @param y outcome
#'   
#' @return Returns a function that takes in a function as an argument and then 
#'   assigns it a numeric fitness value we want to minimize. Use this in
#'   \code{\link{estimate_program}}.
#'   
#' @export
create_fit_func <- function(loss_function, X, y, 
                            parallel){
  
  if(parallel) {
    forloop <- foreach::`%dopar%`
  } else {
    forloop <- foreach::`%do%`
  }
  
  # Meta-data about the predictor variables:
  meta <- data.frame(vars = colnames(X), 
                     types = apply(X, 2, class))
  # All classes should be numeric because we used model.matrix() TODO
  # make sure everything is a character vec
  meta[] <- lapply(meta, as.character)
  
  levels <- what_outcome(y)$levels
  num_predictors <- ncol(X)
  # For each of these, there is levels-dependent initialization of 
  # pfunc output, which should be a container for each output, 
  # with as many slots as there are observations in the data.
  if(num_predictors > 4) 
    stop("We have not created the function to work with more than 4 predictors yet.")
  if(num_predictors == 4){
    fit_func <- function(pfunc){
      out <- forloop(foreach::foreach(i=seq(nrow(X)), .combine='rbind'), {
        pfunc(X[i, 1], X[i, 2], X[i, 3], X[i, 4])
      })
      
      out <- out[, levels]
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = y,
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
      out <- forloop(foreach::foreach(i=seq(nrow(X)), .combine='rbind'), {
        pfunc(X[i, 1], X[i, 2], X[i, 3])
      })
      
      out <- out[, levels]
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = y,
                    prediction = out)
    }
    ## INPUT VARIABLE SET 
    input_set <- rgp::inputVariableSet(rgp::`%::%`(meta[1,1], rgp::st(meta[1,2])),
                                       rgp::`%::%`(meta[2,1], rgp::st(meta[2,2])),
                                       rgp::`%::%`(meta[3,1], rgp::st(meta[3,2])))
  }
  if(num_predictors == 2){
    fit_func <- function(pfunc){
      out <- forloop(foreach::foreach(i=seq(nrow(X)), .combine='rbind'), {
        pfunc(X[i, 1], X[i, 2])
      })
      
      out <- out[, levels]
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = y,
                    prediction = out)
    }
    ## INPUT VARIABLE SET 
    input_set <- rgp::inputVariableSet(rgp::`%::%`(meta[1,1], rgp::st(meta[1,2])),
                                       rgp::`%::%`(meta[2,1], rgp::st(meta[2,2])))
  }
  if(num_predictors == 1){
    fit_func <- function(pfunc){
      out <- forloop(foreach::foreach(i=seq(nrow(X)), .combine='rbind'), {
        pfunc(X[i, 1])
      })
      
      out <- out[, levels]
      
      if (sum(complete.cases(out)) < 1) return(Inf)
      
      loss_function(actual = y,
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
#'@param data a data.frame with named columns containing the variables in
#'  formula. Neither a matrix nor an array will be accepted. We use the
#'  \code{formula} to turn this into a data.frame where (i.) the first column is
#'  named \code{"outcome"} and has the outcome variable we are evolving programs
#'  to predict; (ii.) all other columns (there must be at least one other
#'  column) that follow the \code{"outcome"} column are named columns containing
#'  the predictor variables, which can be of any type. The order matters: when
#'  using the evolved program for predictions the (named) arguments to the
#'  function will be the predictor variables in the order they are supplied to
#'  this \code{estimate_program()} function.
#'@param formula formula used to create the data.frame needed, ensuring that the
#'  outcome variable, the variable to the left of "~", is the first column in
#'  the data.frame and the following columns are the predictor variables.
#'@param subset a specification of the rows to be used: defaults to all rows. 
#'  This can be any valid indexing vector (see [.data.frame) for the rows of 
#'  data or if that is not supplied, a data frame made up of the variables used 
#'  in formula.
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
estimate_program <- function(data, formula, 
                             subset = NULL,
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
  
  if(!missing(formula)){
    # the outcome variable, the variable to the left of "~",
    # is the last column in the data.frame we create here and the preceding columns
    # are the predictor variables.
    if(!missing(subset)){
      temp <- stats::model.frame(formula = formula, data = data,
                                 subset = subset) 
    } else {
      temp <- stats::model.frame(formula = formula, data = data)
    }
    attr(attr(temp, "terms"), "intercept") <- 0
    Terms <- terms(temp)
    X <- stats::model.matrix(object = Terms,
                             data = temp)
    
    xint <- match("(Intercept)", colnames(X), nomatch = 0)
    if (xint > 0) 
      X <- X[, -xint, drop = FALSE]   
    
    y <- temp[ , 1]
  } else {
    stop("You did not provide a formula, we require this to be sure your data is formatted right.")
  }
  
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
                              X = X, y = y,
                              parallel = parallel)$fit_func
  input_set <- create_fit_func(loss_function = loss_function,
                               X = X, y = y,
                               parallel = parallel)$input_set
  
  ## Do EVOLUTION
  full <- rgp::typedGeneticProgramming(fitnessFunction = fit_func, 
                                       type = type,
                                       functionSet = function_set,
                                       inputVariables = input_set,
                                       constantSet = constant_set,
                                       stopCondition = rgp::makeTimeStopCondition(mins*60))
  # We minimize loss, so the lowest fitness value is the best:
  best <- full$population[[which.min(full$fitnessValues)]]
  levels <- what_outcome(y)$levels
  out <- new("model_program",
             func = best,
             levels = levels,
             Terms = Terms)
  
  new("estimate_program",
      call = call,
      timing = as.numeric(proc.time()[[3]]) - start_time,
      session = sessionInfo(),
      full = full,
      best_func = out)
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
#                          formula = outcome ~ .,
#                          loss = "log_lik",
#                          link = "probit",
#                          mins = 0.5,
#                          parallel = FALSE)
# bestFunction1 <- res1@best_func
# bestFunction1@func # It has named arguments, but can use positions, if we want.
# round(predict(bestFunction1, d))
# # # Because this often evolves a probabilistic function, we can replicate it many times 
# # # to get a sense of the function:
# # plot(density(replicate(1000, 
# #                {obs <- 1; predict(bestFunction1, d[obs, ])})))


