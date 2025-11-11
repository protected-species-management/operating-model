#' @title load_life_history
#' 
#' @description Load life-history data into \code{om-class} object from \code{lhm-class} object.
#' 
#' @import lhm
#' 
#' @include om-class.R
#' @export
#{{{ load life history data into om object
setGeneric("load_life_history", function(object, x, ...) standardGeneric("load_life_history"))
#{{ lhm object
setMethod("load_life_history", signature = c("om", "lhm"), function(object, x, ...) {
    
    object@life_history      <- x@lhdat
    object@iter              <- x@iter
    object@ages              <- x@ages
    
    # match dimensions of object@B0 
    # to object@iter
    if (length(object@productivity) > 0) {
        if (length(object@productivity$B0) < object@iter) {
            if (length(object@productivity$B0)>1) {
                stop('conflict between B0 dimension and number of mc-samples\n')
            } else object@productivity$B0 <- rep(object@productivity$B0, object@iter)    
        }
    }
    
    # match dimensions of object@selectivity 
    # to object@iter
    if (length(object@fishing) > 0) {
        if (dim(object@fishing$selectivity)[2] < object@iter) {
            if(dim(object@fishing$selectivity)[2]>1) {
                stop('conflict between selectivity dimensions and number of mc-samples\n')
            } else object@fishing$selectivity <- matrix(rep(object@fishing$selectivity, object@iter), ncol = object@iter)    
        }
    }
	
	# calculate rmax
	object@pst$rmax <- rCalc(x)@.Data
	
	# tidy
	stopifnot(all(object@life_history$F == 0))
	object@life_history$F <- NULL
    
	# return    
    return(object)
})
