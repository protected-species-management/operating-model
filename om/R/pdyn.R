#' @title pdyn
#' 
#' @description Population dynamics function
#' 
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
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
    if (any(is.na(ages))) {
        n <- array(dim = c(1, ntime, niter))
    } else {
        n <- array(dim = c(nages, ntime, niter))
    }
    
    # arrays to record catch
    # and depletion
    x <- array(dim = c(ntime, niter))
    y <- array(dim = c(ntime, niter))
    z <- array(dim = c(ntime, niter))
    
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
        
        # record depletion
        # (updated by function call)
        x[,i] <- depletion
        
        # record catch
        # (updated by function call)
        y[,i] <- catch
        
        # record harvest_rate
        # (updated by function call)
        z[,i] <- harvest_rate
        
        # calculate total numbers and PST
        # reference point
        #object@pst$numbers[,i] <- apply(n[,,i, drop = FALSE], 2, sum)
        object@pst$value[,i] <- pst_value
        
        # calculate diagnostics
        # (catch)
        object@diagnostics$catch[,i] <- y[,i]
        # (catch)
        object@diagnostics$depletion[,i] <- x[,i]
        # (catch)
        object@diagnostics$harvest_rate[,i] <- z[,i]
    }
    
    # calculate objectives
    object@objectives$catch        <- apply(sweep(object@diagnostics$catch,        2, object@targets$catch * (1 + 1e-4), '<='), 1, mean)
    object@objectives$depletion    <- apply(sweep(object@diagnostics$depletion,    2, object@targets$depletion * (1 - 1e-3), '>='), 1, mean)
    object@objectives$harvest_rate <- apply(sweep(object@diagnostics$harvest_rate, 2, object@targets$harvest_rate * (1 + 1e-7), '<='), 1, mean)
    
    # dimnames (after calculations)
    dimnames(n) <- list(age = ages, time = time, iter = 1:niter)
    
    object@targets     <- lapply(object@targets,     function(x) { y <- data.frame(iter = 1:niter, value = x[1,]);  as_tibble(y) })
    object@diagnostics <- lapply(object@diagnostics, function(x) { dimnames(x) <- list(time = time, iter = 1:niter);  y <- array2DF(x, responseName = "value"); y$iter <- as.integer(y$iter); y$time <- as.integer(y$time); as_tibble(y)})
    object@objectives  <- lapply(object@objectives,  function(x) { y <- data.frame(time = time, value = x);  as_tibble(y) })
    
    # assign data
    object@.Data <- n
    
    return(object)
})
#}}}
