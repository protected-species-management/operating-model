#' @title catchability
#' 
#' @description Calculate catchability using empirical abundance data and reconstructed exploitable biomass trajectory
#' 
#' @export
#' 
#' @include dsm-class.R
#' 
#{{{ catchability()
setGeneric("catchability", function(.Object, ...) standardGeneric("catchability"))
setMethod("catchability",signature="dsm",function(.Object, ...) {
    
    if(!(length(.Object@n)>0))
        .Object <- pdyn(.Object)

    bexp <- biomass(.Object, type='exploitable')

    index <- .Object@empirical.data$index

    nidx  <- dim(.Object@empirical.data$index)[2]
    niter <- .Object@iter

    q <- array(dim=c(nidx,niter))

    for(i in 1:nidx) {
        index.tmp <- matrix(rep(index[,i],niter),ncol=niter)
        q[i,] <- apply(sweep(index.tmp,1:2,bexp,'/'),2,function(x) exp(mean(log(x),na.rm=TRUE)))
    }

    dimnames(q) <- list(index=1:nidx,iter=1:niter)
    
    .Object@q <- q
    
    .Object
    
})
    