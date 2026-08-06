#' @title Create \code{om-class} object
#' 
#' @description Initialise operating model class object
#' 
#' @export
#' @seealso \code{\link{om-class}}
#' @include om-class.R
#' @importFrom methods new
#{{{
# constructor
om <- function(ages, harvest_function = function(object, numbers, selectivity, pst, i) {
			# return target harvest rate
			ifelse(all(is.na(object@targets$harvest_rate)), 0, object@targets$harvest_rate[i])
		}, ...) new('om', ages, harvest_function, ...)
#}}}
