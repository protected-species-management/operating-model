#' @title Load parameters into \code{\link{om-class}} object. 
#' 
#' @description Load parameters for use within the \code{population_dynamics()} function.
#' @param value named list object containing parameters
#' 
#' @include om-class.R
#' @export
#{{{
setGeneric("load_pars", function(object, value, ...) standardGeneric("load_pars"))
setMethod("load_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # match dimensions
    # to object@iter
    value <- lapply(value, function(x) {
        if (length(x) < object@iter) {
            if (length(x) > 1) {
                stop('conflict between value dimension and number of mc-samples\n')
            } else {
                x <- rep(x, object@iter)
            }
        }
    }
    
    # assign
    object@pars <- value
    
    # return
    return(object)
})
#}}}




