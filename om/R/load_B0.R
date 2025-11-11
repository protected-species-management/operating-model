#' @title load_B0
#' 
#' @description Load initial mature biomass (B0) into \code{om-class} object.
#' 
#' @include om-class.R productivity_pars.R
#' @export
#{{{ load life history data into om object
setGeneric("load_B0", function(object, x, ...) standardGeneric("load_B0"))
setMethod("load_B0", signature = c("om", "numeric"), function(object, x, ...) {
    
    # check dimensions
    if(length(x) < object@iter) {
        if(length(x) > 1) {
            stop('conflict between B0 dimension and number of mc-samples\n')
        } else x <- rep(x, object@iter)
    }
    
    # load values
    object@productivity$B0 <- structure(x, dim = c(1, object@iter))
    
    # calculate recruitment/productivity
    # parameters
    object <- productivity_pars(object)
    
    # calculate initial conditions
    
    
    # return
    return(object)
})
#}}}
