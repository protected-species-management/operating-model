#' @title Initialize om object
#' 
#' @description Method to initialize om class object
#' 
#' @include om-class.R
#'
#{{{
# initialisation function
setMethod("initialize", "om", function(.Object, pdyn_function, iter, time, ...) {
    
    if(!missing(pdyn_function)) {
        .Object@population_dynamics <- pdyn_function
    }
	
    if(!missing(iter)) {
        .Object@iter <- iter
    }
    
    if(!missing(time)) {
        .Object@time <- time
    }
	
    return(.Object)
})
#}}}
