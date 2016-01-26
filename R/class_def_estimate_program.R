################################################################################
#' An S4 class to return the best results of using the estimate_program 
#' function.
#' 
#' @slot func function
#' @slot levels Numeric vector length one.
#' @slot Terms terms used for fitting the model, got these from \code{model.frame}.
#'   
#' @export
setClass(Class = "model_program",
         slots = c(func = "function",
                   levels = "numeric",
                   Terms = "ANY")
)


################################################################################
#' An S4 class to return the results of using the estimate_program function
#' 
#' @slot call Language from the call of the function 
#'   \code{\link{estimate_program}}.
#' @slot timing Numeric vector length one with the total elapsed time it took 
#'   \code{\link{estimate_program}} to execute.
#' @slot session the results from calling \code{sessionInfo()} at end of 
#'   \code{\link{estimate_program}} function.
#' @slot full full main results
#' @slot best_func S4 object of class \linkS4class{model_program}, which has a
#'   predict method.
#'   
#' @export

setClass(Class = "estimate_program",
         slots = c(call = "language",
                   timing = "numeric",
                   session = "ANY",
                   full = "ANY",
                   best_func = "model_program")
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
#' @param object S4 estimate_program object
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

################################################################################
#' @describeIn estimate_program An S4 method for predicting with an
#'   estimate_program S4 object
#' @param newdata data.frame
#' @param type Optional Character vector length one.
#' @param na.action Optional function.
#' @param thresh Optional numeric vector length one -- the threshold value to
#'   use for converting probabilities into discrete outcomes.
#' @inheritParams estimate_program
#' 
#' @export
setMethod("predict", "estimate_program",
          function(object, newdata, parallel = FALSE, cores = NULL,
                   type = c("prob", "raw"), thresh = 0.5, na.action = na.omit, ...){
            
            object <- object@best_func
            
#             if(!(type %in% c("raw", "prob"))) 
#               stop("type must be either \"raw\" or \"prob\"")
            type <- match.arg(type)
            
            if(parallel) {
              forloop <- foreach::`%dopar%`
              if (missing(cores)) cores <- parallel::detectCores() - 1
              doParallel::registerDoParallel(cores = cores)
            } else {
              forloop <- foreach::`%do%`
            }
            
            newdata <- as.data.frame(newdata)
            Terms <- delete.response(object@Terms)
            m <- model.frame(Terms, newdata, 
                             na.action = na.action)
            if (!is.null(cl <- attr(Terms, "dataClasses"))) 
              .checkMFClasses(cl, m)
            
            X <- model.matrix(Terms, m)
            
            # To get this to work with interaction terms, which use ":", 
            # we need variable names, and thus names of function args to not have ":"
            # We did the same thing in estimate_program(), thus, we must do it here.
            colnames(X) <- gsub(":", "I", colnames(X))
            
            # This model will never use an intercept:
            xint <- match("(Intercept)", colnames(X), nomatch = 0)
            if (xint > 0) 
              X <- X[, -xint, drop = FALSE]   
            #vn <- attr(Terms, "term.labels")
            
            mod <- object@func
            # n_args <- length(formals(mod))
            
            i <- NULL # http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
            out <- forloop(foreach::foreach(i=seq(nrow(X)), .combine='rbind'), {
              do.call(mod, lapply(X[i, ], function(x) x))
            })
            
            if(identical(type, "prob")){
              return(out)
            } 
            if(identical(type, "raw")){
              return(ifelse(out > thresh, 1, 0))
            }
            #             obsLevels <- object@levels
            #             out <- out[, obsLevels, drop=FALSE]
          }
)

