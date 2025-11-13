#' @title Load fishery inputs
#' 
#' @description Load fishery inputs data into \code{om-class} object from \code{fim-class} object. Checks ensure that 'iter', 'time' and 'ages' arguments match.
#' @details The \code{fim-class} object can store any relevant information necessary for the operating model projection. These values are accessible within the \code{populations_dynamics} function stored in the \code{om-class} object.
#' 
#' @import fim
#' @include om-class.R
#' @export
#{{{ load fishery inputs into om object
setGeneric("load_fishery_inputs", function(object, x, ...) standardGeneric("load_fishery_inputs"))
#{{ fim object
setMethod("load_fishery_inputs", signature = c("om", "fim"), function(object, x, ...) {
    
    object@fishery_inputs <- x@.Data
    
    if (is.na(object@iter)) {
        object@iter <- as.integer(x@iter)
    } else {
        if (object@iter != x@iter) {
            stop("'iter' does not match")
        } else {
            object@iter <- as.integer(x@iter)
        }
    }
    
    if (is.na(object@ages)) {
        object@ages <- as.integer(x@ages)
    } else {
        if (any(object@ages != x@ages)) {
            stop("'ages' does not match")
        } else {
            object@ages <- as.integer(x@ages)
        }
    }
    
    if (is.na(object@time)) {
        object@time <- as.integer(x@time)
    } else {
        if (any(object@time != x@time)) {
            stop("'time' does not match")
        } else {
            object@time <- as.integer(x@time)
        }
    }
    
	# return    
    return(object)
})
