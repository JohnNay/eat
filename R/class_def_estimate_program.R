################################################################################
#' An S4 class to return the best results of using the estimate_program 
#' function.
#' 
#' @slot func function
#' @slot formula formula used for fitting the model.
#' @slot terms terms used for fitting the model, got these from model.frame.
#'   
#' @export
setClass(Class = "model_program",
         slots = c(func = "function",
                   levels = "numeric",
                   Terms = "ANY")
)

################################################################################
#' @describeIn model_program An S4 method for printing an estimate_program S4
#'   object
#' @param object S4 model_program object
#' @param newdata data.frame
#' @param type Optional Character vector length one.
#' @param na.action Optional function.
#' @param ... ignored
#' @inheritParams estimate_program
#' 
#' @export
setMethod("predict", "model_program",
          function(object, newdata, parallel = FALSE, cores = NULL,
                   type = "prob", na.action = na.omit, ...){
            
            if(!(type %in% c("raw", "prob"))) 
              stop("type must be either \"raw\" or \"prob\"")
            
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
            xint <- match("(Intercept)", colnames(X), nomatch = 0)
            if (xint > 0) 
              X <- X[, -xint, drop = FALSE]   
            #vn <- attr(Terms, "term.labels")
            
            mod <- object@func
            n_args <- length(formals(mod))
            
            if(type == "prob"){
              if(n_args==4){
                out <- forloop(foreach::foreach(i=seq(nrow(X)), .combine='rbind'), {
                  mod(X[i,1], X[i,2], X[i,3], X[i,4])
                })
              }
            } else {
              # TODO out <- 
            }
#             obsLevels <- object@levels
#             out <- out[, obsLevels, drop=FALSE]
            out  
          }
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
