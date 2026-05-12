#' @title Values calculated by the population dynamics function
#' 
#' @description Extracts values generated internally for projection by \code{pdyn()}. 
#' 
#' @export
#' @include pdyn.R
#{{{ values()
setGeneric("values", function(object, ...) standardGeneric("values"))
setMethod("values", signature = "om", function(object, stochastic, iterations, ...) {

	values <- suppressMessages(pdyn(object, stochastic, iterations, time = 1, initial_depletion = 1.0, verbose = FALSE, ...))
	values@values
})

