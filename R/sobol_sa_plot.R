correct_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  x[x < 0] <- 0
  x[x > 1] <- 1
  cbind(var = row.names(x), x)
}

plot_sobol_fo <- function(x, outcome_var = "Outcome"){
  ss <- correct_bias(x$S)
  ggplot2::ggplot(ss, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() +
    ggplot2::xlab("Variable") + ggplot2::ylab("Estimated Effect") +
    ggplot2::ggtitle(paste("First Order Effects of Variables on", outcome_var)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) +
    ggplot2::ylim(c(0,1))
}

plot_sobol_total <- function(x, outcome_var = "Outcome"){
  tt <- correct_bias(x$T)
  ggplot2::ggplot(tt, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() + 
    ggplot2::xlab("Variable") + ggplot2::ylab("Estimated Effect") +
    ggplot2::ggtitle(paste("Total Sensitivity Effects of Variables on", outcome_var)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) + 
    ggplot2::ylim(c(0,1))
}
