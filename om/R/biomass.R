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
    
    if(!(length(object@n) > 0)) {
        object <- pdyn(object)
	}

    n <- object@n

    selectivity <- as.matrix(object@selectivity)
    maturity    <- as.matrix(object@lh_data$maturity)
    mass        <- as.matrix(object@lh_data$mass)

    time  <- object@empirical_data$time
    tmax  <- length(object@empirical_data$time)
    niter <- object@iter
    
    
    biomass <- list()
    biomass[['total']]       <- array(dim=c(tmax,niter),dimnames=list(time=time,iter=1:niter)) 
    biomass[['mature']]      <- array(dim=c(tmax,niter),dimnames=list(time=time,iter=1:niter))
    biomass[['exploitable']] <- array(dim=c(tmax,niter),dimnames=list(time=time,iter=1:niter))
    for(i in 1:niter) {
        biomass[['total']][,i]       <- apply(sweep(n[,,i],1:2,mass[,i]                  ,'*'),2,sum)
        biomass[['mature']][,i]      <- apply(sweep(n[,,i],1:2,mass[,i] * maturity[,i]   ,'*'),2,sum)
        biomass[['exploitable']][,i] <- apply(sweep(n[,,i],1:2,mass[,i] * selectivity[,i],'*'),2,sum)
    }
    
    if(!missing(type)) {
        if(type == 'total')       biomass <- biomass[['total']]
        if(type == 'mature')      biomass <- biomass[['mature']]
        if(type == 'exploitable') biomass <- biomass[['exploitable']]
    }
    
    return(biomass)
})
