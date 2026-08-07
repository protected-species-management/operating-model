#' @title Return population biomass
#' 
#' @description Reconstruct biomass trajectory based on input data
#' 
#' @export
#' 
#' @include om-class.R
#' 
#{{{ biomass()
setGeneric("biomass", function(object, ...) standardGeneric("biomass"))
setMethod("biomass", signature = "om",function(object, type, ...) {
    
    if(!(length(object@.Data) > 0)) {
        object <- pdyn(object)
    }

    get_dim(object, environment())
    
    NITER <- get("NITER")
    NTIME <- get("NTIME")
    NAGES <- get("NAGES")
    time  <- get("time")
    
    n <- object@.Data
    
    biomass <- list()
    biomass[['total']]       <- array(dim = c(NTIME, NITER), dimnames = list(time = time, iter = 1:NITER)) 
    biomass[['mature']]      <- array(dim = c(NTIME, NITER), dimnames = list(time = time, iter = 1:NITER))
    biomass[['exploitable']] <- array(dim = c(NTIME, NITER), dimnames = list(time = time, iter = 1:NITER))
    
    if (NAGES > 1) { 
        
        selectivity <- as.matrix(object@fishery_inputs$selectivity)
        maturity    <- as.matrix(object@life_history$maturity)
        mass        <- as.matrix(object@life_history$mass)
        
        for(i in 1:NITER) {
            biomass[['total']][,i]       <- apply(sweep(n[,,i], 1:2, mass[,i]                  , '*'), 2, sum, na.rm = TRUE)
            biomass[['mature']][,i]      <- apply(sweep(n[,,i], 1:2, mass[,i] * maturity[,i]   , '*'), 2, sum, na.rm = TRUE)
            biomass[['exploitable']][,i] <- apply(sweep(n[,,i], 1:2, mass[,i] * selectivity[,i], '*'), 2, sum, na.rm = TRUE)
        }
    } else {
        
        for(i in 1:NITER) {
            biomass[['total']][,i]       <- n[1,,i]
            biomass[['mature']][,i]      <- n[1,,i]
            biomass[['exploitable']][,i] <- n[1,,i]
        }
    }
    
    if(!missing(type)) {
        
        if(type == 'total')       biomass <- biomass[['total']]
        if(type == 'mature')      biomass <- biomass[['mature']]
        if(type == 'exploitable') biomass <- biomass[['exploitable']]
    }
    
    return(biomass)
})
