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
    
    # check environment for function call is
    # consistent with current environment
    environment(object@population_dynamics) <- environment()
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = environment())
    
    # setup numbers array
    n <- array(dim = c(nages, ntime, niter))
    
    # iterate dynamics
    for(i in 1:niter) {
        
        # load values stored in:
        # - object@pars
        # - object@fishery_inputs
        # - object@life_history
        # into function environment
        # per iteration
        get_values(object, iter = i, env = environment())
        
        # call population dynamics function
        # per iteration using values and dimensions
        # within function environment
        n[,,i] <- object@population_dynamics()
        
        # calculate total numbers and PST
        # reference point
        
        
        # calculate diagnostics
        # (catch > PST)
        
        # (H > rmax / 2)
        
    }
    
    dimnames(n) <- list(age = ages, time = time, iter = 1:niter)
    
    object@.Data <- n
    
    return(object)
})
#}}}
