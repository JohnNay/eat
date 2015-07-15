
################################################################################
#' An S4 class to return the results of using the estimate_program function
#' 
#' @slot call Language from the call of the function \code{\link{estimate_program}}.
#' @slot timing Numeric vector length one with the total elapsed time it took 
#'   \code{\link{estimate_program}} to execute.
#' @slot session the results from calling \code{sessionInfo()} at end of
#'   \code{\link{estimate_program}} function.
#'   
#' @export

setClass(Class = "estimate_program",
         slots = c(call = "language",
                   timing = "numeric",
                   session = "ANY",
                   full = "ANY",
                   best_func = "function")
)

################################################################################
#' @describeIn estimate_program An S4 method for printing an estimate_program S4 object
#' @param x S4 estimate_program object
#' @param ... ignored
#'  @export

setMethod("print", "estimate_program",
          function(x, ...) str(x)
)

################################################################################
#' @describeIn estimate_program An S4 method for showing a estimate_program S4 object
#'  @export

setMethod("show", "estimate_program",
          function(object) {
            cat("An object of class \"estimate_program\"\n")
            cat("\nCall:\n", 
                paste(deparse(object@call), sep = "\n", collapse = "\n"), 
                "\n\n", sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          }
)

