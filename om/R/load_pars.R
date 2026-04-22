#' @title Load or update parameters 
#' 
#' @description Load or update parameters in \code{\link{om-class}} object. Each parameter should be provided as a \code{\link{distribution-class}}.
#' @param value named list object containing parameter distributions. 
#' @include om-class.R distribution-class.R
#' @export
#{{{
setGeneric("load_pars", function(object, value, ...) standardGeneric("load_pars"))
setMethod("load_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # assign
    object <- update_pars(object, value)
    
    # return
    return(object)
})
#}}}
#' @export
#{{{
setGeneric("update_pars", function(object, value, ...) standardGeneric("update_pars"))
setMethod("update_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # check names
    lapply(names(value), function(a) stopifnot(a %in% names(object@pars)))
    
    # assign
    for (i in 1:length(value)) {
		if (is(value[[i]], 'distribution')) {
			if (names(value)[i], names(object@pars)) {
			    object@pars[[which(names(object@pars) %in% names(value)[i])]] <- value[[i]]
			} else {
			    stop(paste0("'", names(value)[i], "' not assigned"))
			}
		} else {
			stop("value must be of class 'distribution'")
		}
    }
    
    # return
    return(object)
})
#}}}




