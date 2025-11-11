#' @title pdyn
#' 
#' @description Population dynamics function
#' 
#' @export
#' 
#' @include om-class.R
#' 
#{{{ pdyn()
# wrapper for execution of population
# dynamics function
# -- executes object@pdyn for each monte-carlo sample
setGeneric("pdyn", function(object, ...) standardGeneric("pdyn"))
setMethod("pdyn", signature = "om", function(object, ...) {
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object)
    
    n <- array(dim = c(nages, ntime, niter))
    
    for(i in 1:niter) {
        
        # load values stored in:
        # - object@productivity
        # - object@fishing
        # - object@life_history
        # into function environment
        # per iteration
        get_values(object, i)
        
        # call population dynamics function
        # per iteration using values and dimensions
        # within function environment
        n[,,i] <- object@population_dynamics()
    }
    
    dimnames(n) <- list(age = ages, time = time, iter = 1:niter)
    
    object@.Data <- n
    
    return(object)
})
#}}}
