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
    
    # assign
    object@pst$rmax <- value
    
    # return    
    return(object)
})

