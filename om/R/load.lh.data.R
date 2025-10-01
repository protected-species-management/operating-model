#' @title load.lh.data
#' 
#' @description Load life-history data into dsm object
#' 
#' @export
#' @importClassesFrom lhm lhm
#' 
#' @include dsm-class.R
#' 
#{{{ load life history data into dsm object
setGeneric("load.lh.data", function(.Object,x, ...) standardGeneric("load.lh.data"))
#{{ lhm object
setMethod("load.lh.data",signature=c("dsm","lhm"),function(.Object,x, ...) {
    
    .Object@lh.data <- x@lhdat
    .Object@iter    <- x@iter
    .Object@ainf    <- x@ainf
    .Object@sr      <- x@sr
    
    # match dimensions of .Object@B0 
    # to .Object@iter
    if(length(.Object@B0) < .Object@iter) {
        if(length(.Object@B0)>1) {
            stop('conflict between B0 dimension and number of mc-samples\n')
        } else .Object@B0 <- rep(.Object@B0,.Object@iter)    
    }
    
    # match dimensions of .Object@selectivity 
    # to .Object@iter
    if(dim(.Object@selectivity)[2] < .Object@iter) {
        if(dim(.Object@selectivity)[2]>1) {
            stop('conflict between selectivity dimensions and number of mc-samples\n')
        } else .Object@selectivity <- matrix(rep(.Object@selectivity,.Object@iter),ncol=.Object@iter)    
    }
    
    return(.Object)
})
    