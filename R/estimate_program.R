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
         cauchit = stats::binomial(link = "cauchit")$linkinv,
         identity = function(x) x
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
#' @inheritParams estimate_program
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
  #num_predictors <- ncol(X)
  # For each of these, there is levels-dependent initialization of 
  # pfunc output, which should be a container for each output, 
  # with as many slots as there are observations in the data.
  fit_func <- function(pfunc){
    i <- NULL # http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
    out <- forloop(foreach::foreach(i=seq(nrow(X)), .combine='rbind'), {
      do.call(pfunc, lapply(X[i, ], function(x) x))
    })
    
    out <- out[, levels]
    
    if (sum(complete.cases(out)) < 1) return(Inf)
    
    loss_function(actual = y,
                  prediction = out)
  }
  
  # List of meta-data
  lmeta <- as.list(apply(meta, MARGIN = 1, function(y) rgp::`%::%`(y[1], rgp::st(y[2]))))
  input_set <- do.call(rgp::inputVariableSet, lmeta)
  
  list(fit_func = fit_func, input_set = input_set)
}

#' Create function set
#' 
#' @param func_list List where each element is a length one character vector.
#' @param link Length one character vector to pass to create_link_func
#'   
#' @return Returns a function set.
#'   
#' @export
create_func_set <- function(func_list, link){
  
  ## FUNCTION SET
  link <- create_link_func(link)
  
  one_rnorm <- function(.mean) {
    if(is.na(.mean)) .mean <- 1
    rnorm(1, .mean, 1)
  }
  # fac1 <- function(.logical) if(.logical) unique(d$outcome)[1] else sample(unique(d$outcome)[-1], 1)
  # fac2 <- function(.logical) if(.logical) unique(d$outcome)[2] else sample(unique(d$outcome)[-2], 1)
  # fac3 <- function(.logical) if(.logical) unique(d$outcome)[3] else sample(unique(d$outcome)[-3], 1)
  ifelse2 <- function(cond, opt1, opt2) {
    if(anyNA(cond)) return(Inf)
    if(cond) opt1 else opt2
  }
  
  # round2 <- function(x) base::round(x)
  
  create_vec <- function(x,y,z) sapply(c(x,y,z), round)
  
  divide <- function(a,b) a/b
  logn <- function(x) log(x)
  sqrtn <- function(x) sqrt(x)
  
  as_integer <- function(x) as.integer(x)
  
  # Math
  "+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric"))
  "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric"))
  "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric"))
  "divide" %::% (list(st("numeric"), st("nonzero")) %->% st("numeric"))
  "exp" %::% (list(st("numeric")) %->% st("numeric"))
  "logn" %::% (list(st("positive")) %->% st("numeric"))
  "sqrtn" %::% (list(st("positive")) %->% st("numeric"))
  ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical"))
  "<" %::% (list(st("numeric"), st("numeric")) %->% st("logical"))
  
  # Logical
  "&" %::% (list(st("logical"), st("logical")) %->% st("logical"))
  "|" %::% (list(st("logical"), st("logical")) %->% st("logical"))
  "!" %::% (list(st("logical")) %->% st("logical"))
  rgp::pw("ifelse2" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric")), 0.5)
  
  # Randomness
  rgp::pw("one_rnorm" %::% (list(st("numeric")) %->% st("numeric")), 1.5)
  
  # Utilities
  #"create_vec" %::% (list(st("numeric"), st("numeric"), st("numeric")) %->% st("3integers")),
  "as_integer" %::% (list(st("numeric")) %->% st("integer"))
  "link" %::% (list(st("numeric")) %->% st("prob"))
  
  rgp::functionSet(list = func_list, parentEnvironmentLevel = 1)
}

#'Estimate a program (R function) from data.
#'
#'\code{estimate_program} uses \code{rgp}'s \code{rgp::typedGeneticProgramming}.
#'
#'@param formula formula used to create the data.frame needed, ensuring that the
#'  outcome variable, the variable to the left of "~", is the first column in 
#'  the data.frame and the following columns are the predictor variables.
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
#'@param subset a specification of the rows to be used: defaults to all rows. 
#'  This can be any valid indexing vector (see [.data.frame) for the rows of 
#'  data or if that is not supplied, a data frame made up of the variables used 
#'  in formula.
#'@param loss Optional Character vector length one
#'@param identity_outcome_type Optional Character vector length one only needed
#'  if \code{loss=="identity"}.
#'@param link Optional Character vector length one
#'@param func_list Optional List where each element is a length one character 
#'  vector.
#'@param mins Optional Integer vector length one
#'@param steps Optional Integer vector length one
#'@param repeats Optional Integer vector length one sepcifying how many times to
#'  repeat the model fitting
#'@param parallel Optional Logical vector length one. Default is \code{parallel 
#'  = FALSE}; \code{parallel = TRUE} can be slower if the data set is small 
#'  relative to the numner of population evolutions desired
#'@param cores Optional Integer vector length one
#'@param enable_complexity Optional logical vector length one
#'@param lambda Optional integer vector length one for the number of children 
#'  rgp::typedGeneticProgramming() creates in each generation
#'@param crossover_probability Optional numeric vector length one (default == 
#'  0.5) for rgp::typedGeneticProgramming()
#'  
#'@return The function returns an S4 object. See \linkS4class{estimate_program} 
#'  for the details of the \code{slots} (objects) that this type of object will 
#'  have.
#'  
#'  
#'@importFrom rgp %->% %::% st
#'  
#'@export
estimate_program <- function(formula, data, 
                             subset = NULL,
                             loss = c("log_lik", "rmse", "identity", "identity_multi_class"),
                             identity_outcome_type = c("integer", "character", "factor"),
                             link = c("logit", "probit", "cauchit", "identity"),
                             func_list = list("+", "-", "*", "divide", "exp", ">", "<",
                                              "logn", "sqrtn",
                                              # Logical
                                              "&", "|", "!", "ifelse2",
                                              # Randomness
                                              "one_rnorm"
                                              # Utilties
                                              ),
                             # Timing params:
                             mins = 10, steps = 2000,
                             repeats = 2,
                             # Parallel params:
                             parallel = FALSE, 
                             cores = NULL,
                             # Optimization params:
                             enable_complexity = FALSE,
                             lambda = 50,
                             crossover_probability = 0.5){
  
  start_time <- as.numeric(proc.time()[[3]])
  call <- match.call()
  
  loss <- match.arg(loss)
  loss_function <- create_loss_func(loss)
  link <- match.arg(link)
  
  # Change all integers to numeric so they work with type system for numerics:
  data <- data.frame(lapply(data,
                            function(x) {
                              if (class(x)=="integer") {
                                as.numeric(x)
                              } else {
                                x
                              }}))
  
  if (parallel) {
    if (missing(cores)) cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = cores)
  }
  
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
    
    # To get this to work with interaction terms, which use ":", 
    # we need variable names to not have ":" so function args dont have ":"
    colnames(X) <- gsub(":", "I", colnames(X))
    
    xint <- match("(Intercept)", colnames(X), nomatch = 0)
    if (xint > 0)
      X <- X[, -xint, drop = FALSE]   
    
    # Kept the outcome variable in here, i.e. didnt call delete.response(Terms)
    y <- stats::model.response(temp) # temp[ , 1]
  } else {
    stop("You did not provide a formula, we require this to be sure your data is formatted right.")
  }
  
  ################################################################################
  ## CONSTANT SET
  # Would much rather not do global bindings. But because of how the rgp package authors
  # forced use of the globalenv for location of the constant set, I have to.
  e <- globalenv()
  e$booleanConstantFactory <- function() runif(1) > .5
  e$numericConstantFactory <- function() runif(1, -1e5, 1e5)
  e$integerConstantFactory <- function() sample.int(2, 1)-1
  #e$integerVecConstantFactory <- function() round(runif(3, 1, 100))
  e$probConstantFactory <- function() runif(1, 0, 1)
  e$positiveConstantFactory <- function() runif(1, 1e-5, 1e5)
  e$nonzeroConstantFactory <- function() {
    r <- runif(1, -1, 1)
    while(r == 0) r <- runif(1, -1, 1)
    r
  }
  constant_set <- rgp::constantFactorySet("booleanConstantFactory" %::% (list() %->% st("logical")),
                                          "numericConstantFactory" %::% (list() %->% st("numeric")),
                                          "nonzeroConstantFactory" %::% (list() %->% st("nonzero")),
                                          "positiveConstantFactory" %::% (list() %->% st("positive")),
                                          "probConstantFactory" %::% (list() %->% st("prob")),
                                          "integerConstantFactory" %::% (list() %->% st("integer"))
                                          #"integerVecConstantFactory" %::% (list() %->% st("3integers"))
  )
  
  ################################################################################
  fit_func <- create_fit_func(loss_function = loss_function,
                              X = X, y = y,
                              parallel = parallel)$fit_func
  if (identical(loss, "log_lik")){
    func_list <- append(func_list, "link")
    type <-  rgp::st("prob")
  }
  if (identical(loss, "rmse")){
    type <-  rgp::st("numeric")
  }
  if (identical(loss, "identity")){
    func_list <- append(func_list, "as_integer")
    
    identity_outcome_type <- match.arg(identity_outcome_type)
    type <-  rgp::st(identity_outcome_type)
  }
  
  function_set <- create_func_set(func_list, link)
  input_set <- create_fit_func(loss_function = loss_function,
                               X = X, y = y,
                               parallel = parallel)$input_set
  
  ## Do EVOLUTION
  SH <- rgp::makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = lambda,
                                                             crossoverProbability = crossover_probability, 
                                                             enableComplexityCriterion = enable_complexity,
                                                             enableAgeCriterion = FALSE)
  
  candidates <- lapply(vector(mode="list", length=repeats), 
                       function(x){
                         full <- rgp::typedGeneticProgramming(fitnessFunction = fit_func, 
                                                              type = type,
                                                              functionSet = function_set,
                                                              inputVariables = input_set,
                                                              constantSet = constant_set,
                                                              stopCondition = rgp::orStopCondition(rgp::makeStepsStopCondition(steps), 
                                                                                                   rgp::makeTimeStopCondition(mins*60)),
                                                              searchHeuristic = SH)
                         # We minimize loss, so the lowest fitness value is the best:
                         best <- full$population[[which.min(full$fitnessValues)]]
                         score <- full$fitnessValues[which.min(full$fitnessValues)]
                         list(full = full, best = best, score = score)})
  
  full <- candidates[[which.min(sapply(candidates, function(x)x$score))]]$full
  best <- candidates[[which.min(sapply(candidates, function(x)x$score))]]$best
  
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

# ## Classification:
# data("iris", package = "datasets")
# d <- iris
# # Convert it to integer:
# d$Species <- as.integer(d$Species)
# # Convert it to a two-class problem:
# d <- d[!d$Species==3, ]
# d$Species[d$Species!=1] <- 0
# res1 <- eat::estimate_program(Species ~ ., d, loss="identity", 
#                               repeats = 2, mins = 3)
# res1@best_func@func
# paste0(mean(ifelse(predict(res1, d, type="raw")==d$Species, 1, 0))*100, 
#         "% accruacy on training data with 'identity' loss function.")
# res1 <- estimate_program(Species ~ ., d, loss="log_lik", mins = 3)
# res1@best_func@func
# paste0(mean(ifelse(predict(res1, d, type="raw")==d$Species, 1, 0))*100, 
#         "% accruacy on training data with 'log_lik' loss function.")
# # Because this often evolves a probabilistic function, we can replicate it many times 
# # to get a sense of the function:
# plot(density(replicate(1000, 
#                 {obs <- 1; predict(res1, d[obs, ])})))


# # Regression:
# data(cars, package = "caret")
# res2 <- estimate_program(Price ~ ., cars, 
#                          loss = "rmse",
#                          mins = 120,
#                          parallel = FALSE)
# # bestFunction2 <- res2@best_func
# bestFunction2@func # It has named arguments, but can use positions, if we want.
# predict(bestFunction2, cars)
# # Because this often evolves a probabilistic function, we can replicate it many times 
# # to get a sense of the function:
# mean(replicate(100, predict(bestFunction2, cars[1,])))
# plot(density(replicate(1000, 
#                        {obs <- 1; predict(bestFunction2, cars[obs, ])})))
# mean((cars$Price - predict(bestFunction2, cars))^2) # rmse 
# mean((longley$Employed - predict(stats::lm(Employed~., longley), longley))^2) # rmse 

# data(GermanCredit, package = "caret")
# d <- GermanCredit
# # Convert it to integer: 
# # TODO: get this to take in a factor and internally do this like what glm does
# d$Class <- as.integer(d$Class)
# d$Class[d$Class!=1] <- 0
# res1 <- estimate_program(Class ~., 
#                          d,
#                          loss = "log_lik",
#                          #                         link = "logit",
#                          #                          func_list = list("+", "-", "*", "divide", "exp", ">", "<",
#                          #                                           # Logical
#                          #                                           "&", "|", "!", "ifelse2",
#                          #                                           # Randomness
#                          #                                           "one_rnorm"),
#                          mins = 100, steps = 60000,
#                          parallel = TRUE, cores = 6,
#                          enable_complexity = FALSE, crossover_probability = 0.7)
# bestFunction1 <- res1@best_func
# bestFunction1@func # It has named arguments, but can use positions, if we want.
# mean(ifelse(round(predict(res1, d))==d$Class, 1, 0))
# table(predict(res1, d), useNA = "ifany")
# # Because this often evolves a probabilistic function, we can replicate it many times 
# # to get a sense of the function:
# mean(replicate(100,predict(bestFunction1, d[50,])))
# plot(density(replicate(1000, 
#                 {obs <- 1; predict(bestFunction1, d[obs, ])})))
