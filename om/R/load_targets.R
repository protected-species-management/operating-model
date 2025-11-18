#' @title Load targets into \code{\link{om-class}} object. 
#' 
#' @description Load management targets required for evaluation of PST reference point. These should be the Maximum Net Productivity Level (MNPL) and corresponding harvest rate and depletion values. 
#' @param value named list object containing target reference points. List elements must be \code{catch}, \code{depletion} and \code{harvest_rate}.
#' @details Targets are assumed to be known without error, including those derived from biological parameters (e.g., $$r$$ or $$H_{MNPL}$$). 
#' 
#' @include om-class.R
#' @export
#{{{
setGeneric("load_targets", function(object, value, ...) standardGeneric("load_targets"))
# assignment function
#' @rdname load_targets
setMethod("load_targets", signature = c("om", "list"), function(object, value) {
    
    # check that required targets are included
    stopifnot(all(c("catch", "depletion", "harvest_rate") %in% names(value)))
    
    # match dimensions
    # to object@iter
    value <- lapply(value, function(x) {
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
            } else {
                x
            }
        }
    })
    
    # assign
    object@targets <- value
    
    # return
    return(object)
})
#}}}



