# 
# full_factorial <- function(abm, 
#                            input_values,
#                            parms.length = 2, simulator=NULL, iterations = 10){
#         # Gives the same functionality that BehaviorSpace does for NetLogo.
#         # In the @parms slot of the ABM there is a list of lists where
#         # each param is a list that has c(value, min, max).
#         #  1. extract those. 2. create a design matrix using parms.length
#         #  3. iterate through that design matrix, simulating each combination.
# 
#         # parms.length is how many of each variable to list out for each of n variables
#         # then applying expand.grid to these sequences creates n^parms.length param sets
#         # for simulation, so this grows exponentially in parms.length
# 
#         # Need to have a simulator that takes in a set of params and "iterations" and
#         # returns either an objective function (if you are trying to find best fitting params),
#         # or some summary stats if you are just analyzing the I/O of the model.
# 
#         params <- parms(ABM) # extracts the parms slot from the abm object.
# 
#         param.sequences <- lapply(params, function(x) {
#                 seq(from=x$min, to=x$max, length.out=parms.length)})
#         # for each param, creates a vector (of length == parms.length)
#         # of equally spaced values from min to max
# 
#         full.factorial.design <- expand.grid(param.sequences)
#         design.combinations <- nrow(full.factorial.design)
# 
#         # simulate all param sets and return all evalulation criteria as list:
#         sim.results <- apply(full.factorial.design, 1, abm, iterations)
#         # iterations is an argument that gets passed onto the simulator
# 
#         sim.results
# }
