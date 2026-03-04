#' @title load rmax
#' 
#' @description Load rmax into the \code{pst} slot of an \code{om-class} object.
#' 
#' @include om-class.R distribution-class.R sample.distribution.R
#' @export
#{{{ load rmax into om object
setGeneric("load_rmax", function(object, value, ...) standardGeneric("load_rmax"))
#{{ distribution object
setMethod("load_rmax", signature = c("om", "distribution"), function(object, value, ...) {
    
    if (is.na(object@iter)) {
        
        object@iter     <- as.integer(value@iter)
        object@pst$rmax <- value
        
    } else {
        if (object@iter == value@iter | value@iter == 0) {
            
            object@pst$rmax <- value
            
        } else {
            
            warning("'iter' does not match: resampling rmax distribution")
            
            value@iter  <- object@iter
            value@.Data <- sample(value, n = object@iter)
            
            object@pst$rmax <- value
        }
    }
    
    # add dimensions to 
    # reference point
    #object@pst$value <- if (all(is.na(object@time))) matrix(NA_real_, nrow = 1, ncol = object@iter) else matrix(NA_real_, nrow = length(object@time), ncol = object@iter)
    
    # return    
    return(object)
})

