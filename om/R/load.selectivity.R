#' @title load.selectivity
#' 
#' @description Load selectivity ogives
#' 
#' @export
#' 
#' @include dsm-class.R
#' 
#{{{ load selectivity assumption into dsm object
setGeneric("load.selectivity", function(.Object,x, ...) standardGeneric("load.selectivity"))
setMethod("load.selectivity",signature=c("dsm","matrix"),function(.Object,x, ...) {
    
    niter <- .Object@iter
    ainf  <- .Object@ainf
    
    # check age dimension
    x <- apply(x, 2, function(y) { if(length(y) < ainf) y[(length(y)+1):ainf] <- y[length(y)]; y}) 
    
    # check iteration dimension
    if(dim(x)[2] < niter) {
        if(dim(x)[2] > 1) {
            stop('conflict between selectivity dimension and number of mc-samples\n')
        } else {
            x <- matrix(rep(x, niter), ncol=niter)
        } 
    }
    
    .Object@selectivity <- x
    
    .Object
})
#}}}
