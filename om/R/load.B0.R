#' @title load.B0
#' 
#' @description Load initial mature biomass (B0) into dsm object
#' 
#' @export
#' 
#' @include dsm-class.R
#' 
#{{{ load life history data into dsm object
setGeneric("load.B0", function(.Object,x, ...) standardGeneric("load.B0"))
setMethod("load.B0",signature=c("dsm","numeric"),function(.Object,x, ...) {
    
    if(length(x) < .Object@iter) {
        if(length(x) > 1) {
            stop('conflict between B0 dimension and number of mc-samples\n')
        } else x <- rep(x,.Object@iter)
    }
    
    .Object@B0 <- x
    
    .Object
})
#}}}
