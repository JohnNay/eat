#' Summarize a time series
#' 
#' @param datasubset \code{data.frame} with each row (obervational unit) being
#'   an individual decision. With a column named "group" specifying which group
#'   of \code{agg_patterns} each obseravtion is in, and a column named "period" 
#'   specifying at what time period each behavior was taken.
#' @param periods Numeric vector length one specifying maximum number of time 
#'   periods to use for model testing.
#' @param STAT optional character vector length one, default is \code{c("mean", 
#'   "median")}.
#' @param outcome_var_name optional character vector length one, default is
#'   \code{c("action")}.
#'   
#' @return Returns a numeric vector with the \code{c("mean", "median")} of the
#'   \code{outcome_var_name} of the \code{datasubset}.
#' @export

period_vec_create <- function(datasubset, periods, 
                              STAT = c("mean", "median"), 
                              outcome_var_name = "action"){
  
  STAT <- match.arg(STAT)
  
  period_vec <- rep(NA, length(periods))
  
  for (i in seq(periods)){
    if (nrow(datasubset[datasubset$period==i, ]) > 0){ 
      period_vec[i] <- do.call(STAT, 
                               list(x = as.numeric(datasubset[datasubset$period==i, which(names(datasubset) %in% outcome_var_name)]), 
                                    na.rm = TRUE))
    } else{
      period_vec[i] <- NA
    }
  }
  
  stopifnot(length(period_vec)==periods)
  
  period_vec
}
