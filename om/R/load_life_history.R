#' @title load_life_history
#' 
#' @description Load life-history data into \code{om-class} object from \code{lhm-class} object.
#' 
#' @import lhm
#' 
#' @include om-class.R
#' @export
#{{{ load life history data into om object
setGeneric("load_life_history", function(object, value, ...) standardGeneric("load_life_history"))
#{{ lhm object
setMethod("load_life_history", signature = c("om", "lhm"), function(object, value, ...) {
    
    if (!is.null(object@ages)) {
        
        stopifnot(all(value@lhdat$F == 0))
        
        loc <- match(object@ages, value@ages)
        stopifnot(!any(is.na(loc)))
        
        object@life_history <- lapply(value@lhdat, function(x) {
            if (nrow(x) > 1) {
                x[loc,]
            } else {
                x
            }})
    }
    
    if (is.na(object@iter)) {
        object@iter <- as.integer(value@iter)
    } else {
        if (object@iter != value@iter) {
            stop("'iter' does not match")
        } else {
            object@iter <- as.integer(value@iter)
        }
    }
    
    #if (any(is.na(object@ages))) {
    #    object@ages <- as.integer(value@ages)
    #} else {
    #    if (any(object@ages != value@ages)) {
    #        stop("'ages' does not match")
    #    } else {
    #        object@ages <- as.integer(value@ages)
    #    }
    #}
	
	# calculate rmax and 
    # add dimensions to 
    # reference point
	object@pst$rmax    <- rCalc(value)@.Data
	object@pst$numbers <- if (all(is.na(object@time))) matrix(NA_real_, nrow = 1, ncol = object@iter) else matrix(NA_real_, nrow = length(object@time), ncol = object@iter)
	object@pst$value   <- if (all(is.na(object@time))) matrix(NA_real_, nrow = 1, ncol = object@iter) else matrix(NA_real_, nrow = length(object@time), ncol = object@iter)
	
	# return    
    return(object)
})
