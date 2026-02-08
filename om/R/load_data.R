#' @title Load data into \code{\link{om-class}} object. 
#' 
#' @description Load data for use within the \code{\link{population_dynamics}} function.
#' @param value named list object containing parameters
#' @param ... (not used)
#' @include om-class.R
#' @export
#{{{
setGeneric("load_data", function(object, value, ...) standardGeneric("load_data"))
setMethod("load_data", signature = c("om", "list"), function(object, value, ...) {
    
    # assign
    object@data <- value
    
    # return
    return(object)
})
#}}}




