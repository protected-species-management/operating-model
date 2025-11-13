#' @title Load fishery inputs
#' 
#' @description Load fishery inputs data into \code{om-class} object from \code{fim-class} object. Checks ensure that 'iter', 'time' and 'ages' arguments match.
#' @details The \code{fim-class} object can store any relevant information necessary for the operating model projection. These values are accessible within the \code{populations_dynamics} function stored in the \code{om-class} object.
#' 
#' @import fim
#' @include om-class.R
#' @export
#{{{ load fishery inputs into om object
setGeneric("load_fishery_inputs", function(object, value, ...) standardGeneric("load_fishery_inputs"))
#{{ fim object
setMethod("load_fishery_inputs", signature = c("om", "fim"), function(object, value, ...) {
    
    object@fishery_inputs <- value@.Data
    names(object@fishery_inputs) <- names(value)
    
    if (is.na(object@iter)) {
        object@iter <- as.integer(value@iter)
    } else {
        if (object@iter != value@iter) {
            stop("'iter' does not match")
        } else {
            object@iter <- as.integer(value@iter)
        }
    }
    
    if (any(is.na(object@ages))) {
        object@ages <- as.integer(value@ages)
    } else {
        if (any(object@ages != value@ages)) {
            stop("'ages' does not match")
        } else {
            object@ages <- as.integer(value@ages)
        }
    }
    
    if (is.na(object@time)) {
        object@time <- as.integer(value@time)
    } else {
        if (any(object@time != value@time)) {
            stop("'time' does not match")
        } else {
            object@time <- as.integer(value@time)
        }
    }
    
	# return    
    return(object)
})
