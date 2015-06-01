library(datafsm)
context("Test cv_abm() function")

test_that("cv_abm() returns correct type of object", {
  # Helper fuction:
  period_vec_create <- function(datasubset, periods,
                                STAT = "mean"){
    period_vec <- rep(NA, length(periods))
    for (i in seq(periods)){
      if (nrow(datasubset[datasubset$period==i, ]) > 0){
        period_vec[i] <- do.call(STAT,
                                 list(x = as.numeric(datasubset[datasubset$period==i, which(names(datasubset) %in% "outcome")]),
                                      na.rm = TRUE))
        # period_vec[i] <- mean(datasubset[datasubset$period==i, which(names(datasubset) %in% "decision_p")], na.rm = TRUE)
      } else{
        period_vec[i] <- NA
      }
    }
    stopifnot(length(period_vec)==periods)
    period_vec
  }
  # Create data:
  cdata <- data.frame(period = rep(seq(10), 1000),
                      outcome = rep(0:1, 5000),
                      my.decision1 = sample(1:0, 10000, TRUE),
                      other.decision1 = sample(1:0, 10000, TRUE),
                      group = c(rep(1, 5000), rep(2, 5000)))
  time_len <- 2
  agg_patterns <- data.frame(group = c(1,2),
                             action = c( mean(as.numeric(cdata[cdata$group==1, "outcome"])),
                                         mean(as.numeric(cdata[cdata$group==2, "outcome"]))),
                             c(period_vec_create(cdata[cdata$group==1, ], time_len)[1],
                               period_vec_create(cdata[cdata$group==2, ], time_len)[1]),
                             c(period_vec_create(cdata[cdata$group==1, ], time_len)[2],
                               period_vec_create(cdata[cdata$group==2, ], time_len)[2]))
  names(agg_patterns)[3:4] <- c("1", "2")
  
  # Create ABM:
  simulate_abm <- function(model, features, parameters, time_len,
                           tuning_parameters,
                           iterations = 1250, STAT = "mean"){
    matrixOut <- data.frame(period = rep(1:10, 1000),
                            outcome = rep(0:1, 5000),
                            my.decision1 = sample(1:0, 10000, TRUE),
                            other.decision1 = sample(1:0, 10000, TRUE))
    action_avg <- mean(matrixOut$outcome, na.rm=TRUE)
    dynamics <- period_vec_create(matrixOut, time_len)
    list(dynamics = dynamics, action_avg = action_avg, simdata = matrixOut)
  }
  # Create features and formula lists:
  k <- 1
  features <- as.list(rep(NA, k)) # create list to fill
  features[[1]] <- c("my.decision1", "other.decision1")
  Formula <- as.list(rep(NA, k)) # create list to fill
  Formula[[1]] <- "outcome ~ my.decision1 + other.decision1"
  # Call cv_abm():
  res <- cv_abm(cdata, features, Formula, k, agg_patterns,
                abm_simulate = simulate_abm,
                abm_vars = list(values = c(0.3, 0.5)),
                iters = 1000,
                tseries_len = time_len,
                tp = c(1, 2),
                package = "caretglm",
                STAT = "mean",
                saving = FALSE, filename = NULL,
                validate = "lgocv",
                drop_nzv = FALSE,
                predict_test_par = FALSE)
  
  expect_is(res, "cv_abm")
})

test_that("cv_abm() throws warnings and errors when appropriate", {
  
  # Helper fuction:
  period_vec_create <- function(datasubset, periods,
                                STAT = "mean"){
    period_vec <- rep(NA, length(periods))
    for (i in seq(periods)){
      if (nrow(datasubset[datasubset$period==i, ]) > 0){
        period_vec[i] <- do.call(STAT,
                                 list(x = as.numeric(datasubset[datasubset$period==i, which(names(datasubset) %in% "outcome")]),
                                      na.rm = TRUE))
        # period_vec[i] <- mean(datasubset[datasubset$period==i, which(names(datasubset) %in% "decision_p")], na.rm = TRUE)
      } else{
        period_vec[i] <- NA
      }
    }
    stopifnot(length(period_vec)==periods)
    period_vec
  }
  # Create data:
  cdata <- data.frame(period = rep(seq(10), 1000),
                      outcome = rep(0:1, 5000),
                      other.decision1 = sample(1:0, 10000, TRUE),
                      group = c(rep(1, 5000), rep(2, 5000)))
  time_len <- 2
  agg_patterns <- data.frame(group = c(1,2),
                             action = c( mean(as.numeric(cdata[cdata$group==1, "outcome"])),
                                         mean(as.numeric(cdata[cdata$group==2, "outcome"]))),
                             c(period_vec_create(cdata[cdata$group==1, ], time_len)[1],
                               period_vec_create(cdata[cdata$group==2, ], time_len)[1]),
                             c(period_vec_create(cdata[cdata$group==1, ], time_len)[2],
                               period_vec_create(cdata[cdata$group==2, ], time_len)[2]))
  names(agg_patterns)[3:4] <- c("1", "2")
  
  # Create ABM:
  simulate_abm <- function(model, features, parameters, time_len,
                           tuning_parameters,
                           iterations = 1250, STAT = "mean"){
    matrixOut <- data.frame(period = rep(1:10, 1000),
                            outcome = rep(0:1, 5000),
                            my.decision1 = sample(1:0, 10000, TRUE),
                            other.decision1 = sample(1:0, 10000, TRUE))
    action_avg <- mean(matrixOut$outcome, na.rm=TRUE)
    dynamics <- period_vec_create(matrixOut, time_len)
    list(dynamics = dynamics, action_avg = action_avg, simdata = matrixOut)
  }
  # Create features and formula lists:
  k <- 1
  features <- as.list(rep(NA, k)) # create list to fill
  features[[1]] <- c("my.decision1", "other.decision1")
  Formula <- as.list(rep(NA, k)) # create list to fill
  Formula[[1]] <- "outcome ~ my.decision1 + other.decision1"
  # Call cv_abm():
  expect_error(cv_abm(cdata, features, Formula, k, agg_patterns,
                        abm_simulate = simulate_abm,
                        abm_vars = list(values = c(0.3, 0.5)),
                        iters = 1000,
                        tseries_len = time_len,
                        tp = c(1, 2),
                        package = "caretglm",
                        STAT = "mean",
                        saving = FALSE, filename = NULL,
                        validate = "lgocv",
                        drop_nzv = FALSE,
                        predict_test_par = FALSE), "Not all of the features specified are in the data provided", 
                 all = FALSE)
  
})

