#' @title biomass
#' 
#' @description Reconstruct biomass trajectory based on input data
#' 
#' @export
#' 
#' @include dsm-class.R
#' 
#{{{ biomass()
setGeneric("biomass", function(.Object, ...) standardGeneric("biomass"))
setMethod("biomass",signature="dsm",function(.Object, type, ...) {
    
    if(!(length(.Object@n)>0))
        .Object <- pdyn(.Object)

    n <- .Object@n

    selectivity <- as.matrix(.Object@selectivity)
    maturity    <- as.matrix(.Object@lh.data$maturity)
    mass        <- as.matrix(.Object@lh.data$mass)

    time  <- .Object@empirical.data$time
    tmax  <- length(.Object@empirical.data$time)
    niter <- .Object@iter
    
    
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