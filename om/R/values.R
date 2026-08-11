#' @title Values calculated by the population dynamics function
#' @description Extracts values generated internally for projection by \code{\link{pdyn}}. 
#' @note Requires that the \code{om} object already has values for \code{stochastic} and \code{iterations} stored in \code{object@settings}. 
#' @param object \code{om} class object
#' @param ... arguments for the generic function definition
#' @export
#' @importFrom tibble as_tibble
#' @include pdyn.R
#{{{ values()
setGeneric("values", function(object, ...) standardGeneric("values"))
#' @rdname values
setMethod("values", signature = "om", function(object) {
    
    if (all(unlist(lapply(lapply(object@values, is.na), all)))) {
	    values <- suppressMessages(pdyn(object, time = 1, initial_depletion = 1.0, verbose = FALSE))
    } else {
        values <- object
    }
    
	return(as_tibble(data.frame(sample = 1:object@samples, values@values)))
})

