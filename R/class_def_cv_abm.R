
################################################################################
#' An S4 class to return the results of using a cv_abm to estimate and test an
#' ABM
#' 
#' @slot call Language from the call of the function \code{\link{cv_abm}}.
#' @slot predicted_patterns List with predicted vec length 1, actual vec length
#'   1, null_model vec length 1, dynamics vec, simdata data.frame
#' @slot timing Numeric vector length one with the total elapsed time it took 
#'   \code{\link{cv_abm}} to execute.
#' @slot diagnostics Character vector length one, to be printed with
#'   base::cat().
#' @slot session the results from calling \code{sessionInfo()} at end of
#'   \code{\link{pc_sa}} function.
#'   
#' @export

setClass(Class = "cv_abm",
                  slots = c(call = "language",
                            predicted_patterns = "list",
                            timing = "numeric",
                            diagnostics = "character",
                            session = "ANY")
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
                     cat("           Cross Validated Predictions:          \n")
                     cat("                                    \n")
                     cat(object@diagnostics)
                     
                     cat("\n\nThis process took", object@timing, "seconds.")
                     invisible(object)
                   }
)


squared_loss <- function(x, s) sqrt(mean((x - s)^2))

################################################################################
#' Turns cv_abm S4 object into useful outputs for summary or plotting.
#' 
#' @param results S4 cv_abm object
#' @param output Optional character vector length one indicating what the 
#'   desired output is, must be one of: \code{c("MSE", "cor", "cor_pval", "SE", 
#'   "plot")}.
#'   
#' @export

setGeneric("performance", function(results, output = c("MSE", "cor", "cor_pval", "SE",
                                                       "plot")){
  standardGeneric("performance")
})

################################################################################
#' Turns cv_abm S4 object into useful outputs for summary or plotting.
#' @describeIn cv_abm An S4 method for extracting performance measures of an
#'   cv_abm S4 object
#'   
#' @param results S4 cv_abm object
#' @param output Optional character vector length one indicating what the 
#'   desired output is, must be one of: \code{c("MSE", "cor", "cor_pval", "SE", 
#'   "plot")}.
#'   
#' @export
setMethod("performance", "cv_abm",
          function(results, 
                   output = c("MSE", "cor", "cor_pval", "SE",
                              "plot")){
            output <- match.arg(output)
            tp <- eval(results@call$tp)
            patterns <- eval(results@call$agg_patterns)
            # extract the relevant results from the cv_abm object
            results <- results@predicted_patterns
            
            rgames <- rep(NA, sum(tp))
            sgames <- rep(NA, sum(tp))
            Time <- rep(NA, sum(tp))
            Group <- rep(NA, sum(tp))
            cors <- rep(NA, sum(tp))
            missings <- 0
            
            for(i in seq(nrow(patterns))){
              index <- sum(c(0,tp)[1:i]) + 1
              rgames[index:(index + tp[i] - 1)] <- as.numeric(patterns[i, which(names(patterns) %in% paste(seq(tp[i])))])
              sgames[index:(index + tp[i] - 1)] <- results[[i]]$dynamics[seq(tp[i])]
              Time[index:(index + tp[i] - 1)] <- seq(tp[i])
              Group[index:(index + tp[i] - 1)] <- i
              # make all simulated time period results NA that are NA in the data patterns
              if (any(is.na(rgames[index:(index + tp[i] - 1)]))){
                missings <- missings + sum(is.na(rgames[index:(index + tp[i] - 1)]))
                sgames[index:(index + tp[i] - 1)][is.na(rgames[index:(index + tp[i] - 1)])] <- NA
                Time[index:(index + tp[i] - 1)][is.na(rgames[index:(index + tp[i] - 1)])] <- NA
                Group[index:(index + tp[i] - 1)][is.na(rgames[index:(index + tp[i] - 1)])] <- NA
              }
              
              cors[index:(index + tp[i] - 1)] <- paste("Structure ", i, ": cor = ", round(cor(rgames[index:(index + tp[i] - 1)],
                                                                                              sgames[index:(index + tp[i] - 1)], 
                                                                                              use = "complete.obs"), 2), 
                                                       sep="")
              if (any(is.na(rgames[index:(index + tp[i] - 1)]))){
                cors[index:(index + tp[i] - 1)][is.na(rgames[index:(index + tp[i] - 1)])] <- NA
              }
            }
            
            rgames <- rgames[!is.na(rgames)]
            sgames <- sgames[!is.na(sgames)]
            Time <- Time[!is.na(Time)]
            Group <- Group[!is.na(Group)]
            cors <- cors[!is.na(cors)]
            if(!(length(rgames) == length(sgames) & length(sgames) == sum(tp) - missings))
              warning("This plot may not work because it is not true that '(length(rgames) == length(sgames) & length(sgames) == sum(tp) - missings)'. Probably because you did not run the ABM for enough simulations.")
            
            plot_data <- data.frame(Action = c(rgames, sgames), Time = rep(Time, 2), Group = rep(Group, 2), 
                                    Model = factor(c(rep("Actual", length(rgames)), rep("Predicted", length(sgames)))),
                                    cors = rep(cors, 2))
            
            if(!any(is.na(plot_data[ , "cors"]))){
              plot_data[ , "cors"] <- factor(plot_data[ , "cors"], levels = gtools::mixedsort(unique(plot_data[ , "cors"])))
            }
            switch(output,
                   MSE = squared_loss(rgames, sgames),
                   cor = cor(rgames, sgames), # use = "complete.obs"
                   cor_pval = paste(round(Hmisc::rcorr(rgames, sgames)[[1]][1,2], 2), ", p=", round(Hmisc::rcorr(rgames, sgames)[[3]][1,2], 2), sep=""),
                   SE = (rgames - sgames)^2,
                   plot = plot_data) 
          })

################################################################################
#' Plots cv_abm S4 object
#' @describeIn cv_abm An S4 method for plotting a cv_abm S4 object
#'   
#' @param y not used.
#' @param ncol optional numeric vector length one specifying number of columns 
#'   of the faceted subsetted graphs, i.e. how many columns ggplot2 will use 
#'   when wrapping the structures around.
#' @export

setMethod("plot", "cv_abm",
          function(x, y, ncol = 4){
            x <- performance(x, output = "plot")
            ggplot2::ggplot(data = x, 
                            ggplot2::aes(x = Time, y = Action, color = Model)) +
              ggplot2::geom_line(ggplot2::aes(linetype = Model), size = 1) + # stat="smooth", method = "loess", fill=NA, alpha=0.5
              ggplot2::scale_linetype_manual(values=c("solid", "dashed")) + # Change linetypes so predicted is solid and actual is dashed
              ggplot2::facet_wrap(~cors, ncol = ncol) +
              ggplot2::scale_y_continuous(limits=c(0, 1)) +
              ggplot2::ylab("") + ggplot2::xlab("") +
              ggplot2::theme_bw() +
              ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=5)))
          })

