#' @rdname om-class
#' @description Initialise operating model class object
#' @param ages a numeric value giving the maximum age or a vector of ages
#' @param time a numeric value giving the number of time steps or a vector of times
#' @param harvest_function harvest rate function for use in projection
#' @param ... optional input arguments with default values: \code{samples = 1}, \code{shape = 1}, \code{phi = 1}
#' @include om-class.R
#' @importFrom methods new
#{{{
# constructor
#' @export
om <- function(ages, time, harvest_function = function(object, numbers, selectivity, pst, i) {
			# return target harvest rate
			ifelse(all(is.na(object@targets$harvest_rate)), 0, object@targets$harvest_rate[i])
		}, ...) new('om', ages, time, harvest_function = harvest_function, ...)
#}}}
