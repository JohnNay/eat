#' Compute Log Likelihood
#' 
#' Bernoulli likelihood: x = 0, 1; f(x,theta) = (theta^x)*(1-theta)^(1-x)
#' Bernoulli log-likelihood: $$ ln(L) = sum_{i=1}^I sum_{t=1}^T  D_i^C(t)  *  ln(P_{i}^{C} (t)) + (1 - D_i^C(t)) * ln(1 - P_{i}^{C} (t)) $$
#'
#' @param prediction Numeric vector same length as actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#' 
#' @examples 
#' compute_log_lik(runif(10), sample(c(1,0), 10, replace=TRUE))
#' 
#' @export
compute_log_lik <- function(prediction, actual){
  stopifnot(length(prediction)==length(actual))
  
  log.likelihood <- 0
  for (i in seq(length(prediction))) {
    p <- prediction[i]
    log.likelihood <- log.likelihood + 
      base::log(ifelse(actual[i] == 1, p, 1 - p))
  }
  
  log.likelihood
}

# # Testing Bernoulli log-likelihood computation by comparing to R's glm(): 
# g1 <- glm(my.decision ~ 1, family = binomial, data = data)
# stopifnot(round(-2 * compute_log_lik(as.numeric(predict(g1, data, type="response")),
#                                      ifelse(data$my.decision=="coop", 1, 0))) == 
#             round(g1$deviance))
# rm(g1)
# g2 <- glm(my.decision ~ r1 + r2 + risk + error + delta + r1:delta + r2:delta + infin, 
#           family = binomial, data = data)
# stopifnot(round(-2 * compute_log_lik(as.numeric(predict(g2, data, type="response")),
#                                      ifelse(data$my.decision=="coop", 1, 0))) == 
#             round(g2$deviance))
# rm(g2)

#' Compute Identity
#' 
#' Best is -1, worst is 0. Objective is to minimize.
#' 
#' @param prediction Numeric vector same length as actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#' 
#' @examples
#' compute_identity(1:10, 1:10)
#' compute_identity(1:10, c(1:9, 1))
#' compute_identity(runif(10), sample(c(1,0), 10, replace=TRUE))
#' 
#' @export
compute_identity <- function(prediction, actual){
  # objective is to minimize
  stopifnot(length(prediction)==length(actual))
  - mean(ifelse(prediction == actual, 1, 0)) # best is -1, worst is 0
}

#' Compute Identity Multi Class 
#' 
#' Best is -1, worst is 0. Objective is to minimize.
#' 
#' @param prediction Numeric matrix same number of rows as length of actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#' 
#' @examples
#' compute_identity_multi_class(matrix(1:100, nrow=10), 1:10)
#' compute_identity_multi_class(matrix(rep(1:2, 10), nrow=10), rep(2,10))
#' 
#' @export
compute_identity_multi_class <- function(prediction, actual){
  stopifnot(nrow(prediction)==length(actual))
  
  out <- 0
  for (i in seq(nrow(prediction))){
    out <- out + ifelse(which(prediction[i, ] == max(prediction[i, ])) == actual[i], 1, 0)
  }
  
  - mean(out)/nrow(prediction) # best is -1, worst is 0
}

#' Compute Root Mean Squared Error
#' 
#' @param prediction Numeric vector same length as actual
#' @param actual Numeric vector same length as prediction
#'
#' @return Numeric vector length one
#' 
#' @examples
#' compute_rmse(1:10, 1:10)
#' compute_rmse(1:10, c(1:9, 1))
#' compute_rmse(rnorm(10), rnorm(10))
#' 
#' @export
compute_rmse <- function(prediction, actual) 
  sqrt(mean((prediction - actual)^2))
