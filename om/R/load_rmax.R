#' @title load rmax
#' 
#' @description Load rmax into \code{om-class} object.
#' 
#' @include om-class.R distribution-class.R
#' @export
#{{{ load rmax into om object
setGeneric("load_rmax", function(object, value, ...) standardGeneric("load_rmax"))
#{{ distribution object
setMethod("load_rmax", signature = c("om", "distribution"), function(object, value, ...) {
    
    if (is.na(object@iter)) {
        object@iter     <- as.integer(value@iter)
        object@pst$rmax <- value@.Data
    } else {
        if (object@iter == value@iter) {
            object@pst$rmax <- value@.Data
        } else {
            if (value@iter == 1) {
                object@pst$rmax <- rep(value@.Data, object@iter)
            } else {
                stop("'iter' does not match")
            }
        }
    }
    
    # add dimensions to 
    # reference point
    #object@pst$value <- if (all(is.na(object@time))) matrix(NA_real_, nrow = 1, ncol = object@iter) else matrix(NA_real_, nrow = length(object@time), ncol = object@iter)
    
    # return    
    return(object)
})

