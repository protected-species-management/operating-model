#' @title load.lh_data
#' 
#' @description Load life-history data into om object
#' 
#' @export
#' @importClassesFrom lhm lhm
#' 
#' @include om-class.R
#' 
#{{{ load life history data into om object
setGeneric("load_lh_data", function(object,x, ...) standardGeneric("load_lh_data"))
#{{ lhm object
setMethod("load_lh_data",signature=c("om","lhm"),function(object,x, ...) {
    
    object@lh_data <- x@lhdat
    object@iter    <- x@iter
    object@ainf    <- x@ainf
    object@sr      <- x@sr
    
    # match dimensions of object@B0 
    # to object@iter
    if(length(object@B0) < object@iter) {
        if(length(object@B0)>1) {
            stop('conflict between B0 dimension and number of mc-samples\n')
        } else object@B0 <- rep(object@B0,object@iter)    
    }
    
    # match dimensions of object@selectivity 
    # to object@iter
    if(dim(object@selectivity)[2] < object@iter) {
        if(dim(object@selectivity)[2]>1) {
            stop('conflict between selectivity dimensions and number of mc-samples\n')
        } else object@selectivity <- matrix(rep(object@selectivity,object@iter),ncol=object@iter)    
    }
    
    return(object)
})
