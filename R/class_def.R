
################################################################################
#' An S4 class to return the results of using a cv_abm to estimate and test an ABM
#'
#' @slot call Language from the call of the function \code{\link{cv_abm}}.
#' @slot predicted_patterns List with predicted vec length 1, actual vec length 1, 
#' null_model vec length 1, dynamics vec, simdata data.frame
#' @slot timing Numeric vector length one with the total elapsed time it took
#'   \code{\link{cv_abm}} to execute.
#' @slot diagnostics Character vector length one, to be printed with base::cat().
#'
#' @export

setClass(Class = "cv_abm",
                  slots = c(call = "language",
                            predicted_patterns = "list",
                            timing = "numeric",
                            diagnostics = "character")
)

################################################################################
#' @describeIn cv_abm An S4 method for printing a cv_abm S4 object
#' @param x S4 cv_abm object
#' @param ... ignored
#'  @export

setMethod("print", "cv_abm",
                   function(x, ...) str(x)
)

################################################################################
#' @describeIn cv_abm An S4 method for showing a cv_abm S4 object
#'  @export

setMethod("show", "cv_abm",
                   function(object) {
                     cat("An object of class \"cv_abm\"\n")
                     cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
                     cat("Available slots:\n")
                     print(slotNames(object))
                   }
)

################################################################################
#' Turns cv_abm S4 object into list of summaries for printing and then prints it.
#' @describeIn cv_abm An S4 method for summarizing a cv_abm S4 object
#'
#' @param object S4 cv_abm object
#' @param digits Optional numeric vector length one for how many significant digits to
#' print, default is 3.
#'
#'  @export

setMethod("summary", "cv_abm",
                   function(object, digits = 3) {
                     cat("                                    \n")
                     cat("           CV_ABM Results:          \n")
                     cat("                                    \n")
                     
                     
                     
                     invisible(object)
                   }
)
