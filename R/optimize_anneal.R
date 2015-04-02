# #'Simulated Annealing Optimization (with box constraints) for a Simulation Model
# #'
# #'\code{sobol_sa} conducts a global variance decomposition.
# #'
# #'This is function of the \strong{eat} package. It takes an abm in function form
# #'and a list of input values.
# #'
# #'
# #'@param abm A function that takes as input values for each of the 
# #'  \code{input_values}
# #'@param input_values List
# #'@param out Character vector length one to be passed an argument to the 
# #'  \code{abm} function to specify what outcome measure to use.
# #'@param sample_count  Optional Numeric vector length one. Default is 100.
# #'@param constraints Optional Character vector that is either "none" or is using
# #'  only variable names that are specified in the input_values List argument. 
# #'  This character vector is evaluated in an environment created for the sampled
# #'  data on the variables, and its evaluation results in a Logical vector that 
# #'  that subsets sampled.
# #'@param sobol_nboot Optional Numeric vector length one. Default is 1000.
# #'@param iterations Optional numeric vector length one.
# #'@param parallel Optional logical vector length one. Default is FALSE.
# #'@param cores Optional Numeric vector length one. Default is
# #'  parallel::detectCores().
# #'@param verbose Optional logical vector.
# #'  
# #'@return Returns a sobol objects that can be plotted by functions
# #'  
# 
# optimize_anneal <- function(abm, 
#                             input_values,
#                             iterations = NULL,
#                             max_iteration = 300){
# 
#         # Need to have a simulator that takes in a set of params and the "iterations"
#         # argument and returns an objective function created by evaluating the
#         # outcome of running the ABM iterations times with those params.
# 
#         # get the min and max values of the parameter ranges, store in a vec.
#         mins <- sapply(seq(1,length(params)), function(i) {
#                 params[[i]]$min})
#         maxs <- sapply(seq(1,length(params)), function(i) {
#                 params[[i]]$max})
# 
#         # start with the initial values of the params in the ABM object
#         initial.values <- sapply(seq(1,length(params)), function(i) {
#           params[[i]]$value})
#         
#         GenSA::GenSA(par = initial.values,
#               fn = abm,
#               lower = mins,
#               upper = maxs,
#               control=list(max.call=max_iteration),
#               # put arguments to simulator here:
#               iterations = iterations
#         )
# }
# 
