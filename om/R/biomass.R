#' @title biomass
#' 
#' @description Reconstruct biomass trajectory based on input data
#' 
#' @export
#' 
#' @include om-class.R
#' 
#{{{ biomass()
setGeneric("biomass", function(object, ...) standardGeneric("biomass"))
setMethod("biomass",signature="om",function(object, type, ...) {
    
    if(!(length(object@.Data) > 0)) {
        object <- pdyn(object)
	}

    n <- object@.Data

    selectivity <- as.matrix(object@fishing$selectivity)
    maturity    <- as.matrix(object@life_history$maturity)
    mass        <- as.matrix(object@life_history$mass)

    time  <- object@time
    tmax  <- length(object@time)
    niter <- object@iter
    
    
    biomass <- list()
    biomass[['total']]       <- array(dim=c(tmax,niter),dimnames=list(time=time,iter=1:niter)) 
    biomass[['mature']]      <- array(dim=c(tmax,niter),dimnames=list(time=time,iter=1:niter))
    biomass[['exploitable']] <- array(dim=c(tmax,niter),dimnames=list(time=time,iter=1:niter))
    for(i in 1:niter) {
        biomass[['total']][,i]       <- apply(sweep(n[,,i], 1:2, mass[,i]                  ,'*'), 2, sum, na.rm = TRUE)
        biomass[['mature']][,i]      <- apply(sweep(n[,,i], 1:2, mass[,i] * maturity[,i]   ,'*'), 2, sum, na.rm = TRUE)
        biomass[['exploitable']][,i] <- apply(sweep(n[,,i], 1:2, mass[,i] * selectivity[,i],'*'), 2, sum, na.rm = TRUE)
    }
    
    if(!missing(type)) {
        if(type == 'total')       biomass <- biomass[['total']]
        if(type == 'mature')      biomass <- biomass[['mature']]
        if(type == 'exploitable') biomass <- biomass[['exploitable']]
    }
    
    return(biomass)
})
