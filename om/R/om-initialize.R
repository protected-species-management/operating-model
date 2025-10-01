#' @title Initialize om object
#' 
#' @description Method to initialize om class object
#' 
#' @include om-class.R
#'
#{{{
# initialisation function
setMethod("initialize","om",function(.Object, pdyn_function, iter, ...) {
    
    if(!missing(pdyn_function)) {
        .Object@pdyn <- pdyn_function
    }
	
    if(!missing(iter)) {
        .Object@iter <- iter
    }
	
    return(.Object)
})
#}}}
