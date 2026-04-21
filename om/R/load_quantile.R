#' @title Load observation quantile 
#' @aliases load_quantiles
#' @description Load observation quantile into \code{\link{om-class}} object for stochastic projection.
#' @param value named list object containing value for \code{observation} only. 
#' @param ... (not used)
#' @include om-class.R
#' @export
#{{{
setGeneric("load_quantile", function(object, value, ...) standardGeneric("load_quantile"))
setMethod("load_quantile", signature = c("om", "list"), function(object, value, ...) {
    
    # assign
    for (i in names(object@settings$qn)) {
        if (i %in% names(value)) {
            object@settings$qn[[i]] <- value[[i]]
        }
    }
    
    # don't assign
    for (i in names(value)) {
        if (!(i %in% names(object@settings$qn))) {
            warning("'", i, "' ignored")    
        }
    }
    
    # return
    return(object)
})
#}}}


