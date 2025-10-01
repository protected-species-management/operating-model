#' @title Initialize dsm object
#' 
#' @description Method to initialize dsm class object
#' 
#' @include dsm-class.R
#'
#{{{
# initialisation function
setMethod("initialize","dsm",function(.Object, pdyn.function, iter, ...) {
    
    if(!missing(pdyn.function))
        .Object@pdyn <- pdyn.function
    
    if(!missing(iter))
        .Object@iter <- iter
    
    .Object
    
})
#}}}