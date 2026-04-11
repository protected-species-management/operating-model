#' @title Load settings into \code{\link{om-class}} object. 
#' 
#' @description Load settings.
#' @param value named list object containing values for one or all of \code{samples}, \code{stochastic_iterations}, \code{equilibrirum_time},\code{cv_survivorship} and \code{cv_birth}.
#' @param ... (not used)
#' @include om-class.R
#' @export
#{{{
setGeneric("load_settings", function(object, value, ...) standardGeneric("load_settings"))
setMethod("load_settings", signature = c("om", "list"), function(object, value, ...) {
    
    # assign
    for (i in names(object@settings)) {
        if (i %in% names(value)) {
            object@settings[[i]] <- value[[i]]
        }
    }
    
    # don't assign
    for (i in names(value)) {
        if (!(i %in% names(object@settings))) {
            warning("'", i, "' ignored")    
        }
    }
    
    # return
    return(object)
})
#}}}




