#' @title Load fixed values into \code{\link{om-class}} object. 
#' 
#' @description Load fixed values for use within the \code{\link{population_dynamics}} function.
#' @param value named list object containing parameters
#' @param ... (not used)
#' @include om-class.R
#' @export
#{{{
setGeneric("load_fixed", function(object, value, ...) standardGeneric("load_fixed"))
setMethod("load_fixed", signature = c("om", "list"), function(object, value, ...) {
    
    # assign
    object@fixed <- value
    
    # return
    return(object)
})
#}}}




