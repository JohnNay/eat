% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/period_vec_create.R
\name{period_vec_create}
\alias{period_vec_create}
\title{Summarize a time series}
\usage{
period_vec_create(datasubset, periods, STAT = c("mean", "median"),
  outcome_var_name = "action")
}
\arguments{
\item{datasubset}{\code{data.frame} with each row (obervational unit) being
an individual decision. With a column named "group" specifying which group
of \code{agg_patterns} each obseravtion is in, and a column named "period" 
specifying at what time period each behavior was taken.}

\item{periods}{Numeric vector length one specifying maximum number of time 
periods to use for model testing.}

\item{STAT}{optional character vector length one, default is \code{c("mean", 
"median")}.}

\item{outcome_var_name}{optional character vector length one, default is
\code{c("action")}.}
}
\value{
Returns a numeric vector with the \code{c("mean", "median")} of the
  \code{outcome_var_name} of the \code{datasubset}.
}
\description{
Summarize a time series
}

