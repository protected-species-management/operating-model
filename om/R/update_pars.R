#' @title Update parameters in \code{\link{om-class}} object. 
#' 
#' @description Update parameters already loaded in \code{\link{om-class}} object for use within the \code{\link{population_dynamics}} function.
#' @param value named list object containing parameters
#' @seealso [load_pars()]
#' @include om-class.R distribution-class.R
#' @export
#{{{
setGeneric("update_pars", function(object, value, ...) standardGeneric("update_pars"))
setMethod("update_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # check names
    lapply(names(value), function(a) stopifnot(a %in% names(object@pars)))
    
    # assign
    for (i in 1:length(value)) {
		if (is(value[[i]], 'distribution')) {
			object@pars[[which(names(object@pars) %in% names(value)[i])]] <- value[[i]]
		} else {
			stop("value must be of class 'distribution'")
		}
    }
    
    # return
    return(object)
})
#}}}




