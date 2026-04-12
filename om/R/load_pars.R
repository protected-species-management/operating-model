#' @title Load parameters into \code{\link{om-class}} object. 
#' 
#' @description Load parameters for use within the \code{\link{population_dynamics}} function.
#' @param value named list object containing parameters
#' 
#' @include om-class.R
#' @export
#{{{
setGeneric("load_pars", function(object, value, ...) standardGeneric("load_pars"))
setMethod("load_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # assign
    object@pars <- value
    
    # return
    return(object)
})
#}}}




