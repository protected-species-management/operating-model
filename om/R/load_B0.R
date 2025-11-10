#' @title load.B0
#' 
#' @description Load initial mature biomass (B0) into om object
#' 
#' @export
#' 
#' @include om-class.R
#' 
#{{{ load life history data into om object
setGeneric("load_B0", function(object, x, ...) standardGeneric("load_B0"))
setMethod("load_B0", signature = c("om", "numeric"), function(object, x, ...) {
    
    if(length(x) < object@iter) {
        if(length(x) > 1) {
            stop('conflict between B0 dimension and number of mc-samples\n')
        } else x <- rep(x, object@iter)
    }
    
    object@productivity$B0 <- x
    
    return(object)
})
#}}}
