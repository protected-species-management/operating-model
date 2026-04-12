#' @title Create \code{\link{om-class}} object
#' 
#' @description Initialise operating model class object
#' 
#' @export
#' @seealso [om-class]
#' @include om-class.R
#'
#{{{
# constructor
om <- function(ages, harvest_function = .harvest_rate, ...) new('om', ages, harvest_function, ...)
#}}}
#{{{
# default
.harvest_rate <- function(object, i) {
    
    # return target harvest rate
    ifelse(all(is.na(object@targets$harvest_rate)), 0, object@targets$harvest_rate[i])
}
#}}}
