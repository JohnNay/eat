#'Create set of samples by sampling with LHS and then checking constraints.
#'
#'\code{create_set} creates sample that stay within constraints.
#'
#'@param input_values List
#'@param input_names Character vector
#'@param sample_count Numeric vector length one.
#'  
#'@return Returns a data.frame of samples.
#'@export
create_set <- function(input_values, input_names, sample_count){
  input.sets <- create_sample(input_values, input_names, sample_count)
  input.sets <- keep_satisfied(input.sets)
  while(nrow(input.sets) < sample_count) { 
    # Create input factor sets by latin hypercube sampling:
    input.sets <- rbind(input.sets,
                        create_sample(input_values, input_names, sample_count))  
    # Discard input factor sets that violate constraints:
    input.sets <- keep_satisfied(input.sets)
  }
  input.sets
}

################################################################################
#' @describeIn create_set Create a sample.
#'
#'@return Returns a data.frame of samples.
#'  @export
create_sample <- function(input_values, input_names, sample_count) {
  # will create values from 0 to 1 and must be transformed afterwards, if need be.
  
  # create a random sample of input factor sets with Latin Hypercube Sampling
  lhs_design <- lhs::improvedLHS(sample_count, length(input_values))
  
  # transform the standardized random values to the real input value range (if need be)
  # and apply the desired random distribution
  lhs_design <- lapply(seq(1,length(input_values)), function(i) {
    input_values[[i]]$ARGS$p <- as.vector(lhs_design[ ,i])
    do.call(input_values[[i]]$random_function, input_values[[i]]$ARGS) # input_values[[i]]$min, input_values[[i]]$max
  })
  names(lhs_design) <- input_names
  data.frame(lhs_design)
}

################################################################################
#' @describeIn create_set Stay within constraints.
#'
#' @param sampled Output of create sample_sample
#' 
#'@return Returns a data.frame of samples.
#'  @export
keep_satisfied <- function(sampled){
  # TODO: add any constraints
  constraints <- rep(TRUE, nrow(sampled))
  data.frame(sampled[constraints, , drop=FALSE])
}

