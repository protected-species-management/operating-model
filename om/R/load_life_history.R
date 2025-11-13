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
    
    object@life_history <- x@lhdat
    
    if (is.na(object@iter)) {
        object@iter <- as.integer(x@iter)
    } else {
        if (object@iter != x@iter) {
            stop("'iter' does not match")
        } else {
            object@iter <- as.integer(x@iter)
        }
    }
    
    if (any(is.na(object@ages))) {
        object@ages <- as.integer(x@ages)
    } else {
        if (any(object@ages != x@ages)) {
            stop("'ages' does not match")
        } else {
            object@ages <- as.integer(x@ages)
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
