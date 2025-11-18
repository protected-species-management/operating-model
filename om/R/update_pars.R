#' @title Update parameters in \code{\link{om-class}} object. 
#' 
#' @description Update parameters already loaded in \code{\link{om-class}} object for use within the \code{\link{population_dynamics}} function.
#' @param value named list object containing parameters
#' @seealso [load_pars()]
#' @include om-class.R
#' @export
#{{{
setGeneric("update_pars", function(object, value, ...) standardGeneric("update_pars"))
setMethod("update_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # check names
    lapply(names(value), function(a) stopifnot(a %in% names(object@pars)))

    ff <- function(x) {
        if (is.null(dim(x))) {
            # vector
            if (length(x) < object@iter) {
                if (length(x) > 1) {
                    stop('conflict between value dimension and number of mc-samples\n')
                } else {
                    x <- matrix(rep(x, object@iter), ncol = object@iter)
                }
            } else {
                x <- matrix(x, ncol = object@iter)
            }
        } else {
            # matrix
            if (dim(x)[2] < object@iter) {
                if(dim(x)[2] > 1) {
                    stop('conflict between value dimension and number of mc-samples\n')
                } else {
                    x <- matrix(rep(x, object@iter), ncol = object@iter)
                }
            }
        }
        return(x)
    }
    
    # assign
    for (i in 1:length(value)) {
        object@pars[[which(names(object@pars) %in% names(value)[i])]] <- ff(value[[i]])
    }
    
    # return
    return(object)
})
#}}}




