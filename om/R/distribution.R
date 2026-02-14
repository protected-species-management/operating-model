#' 
#' @rdname distribution-class
#' 
#' @param value either an integer specifiying the length of an empty vector or a vector of derived values
#' 
#' @examples
#' # create object containing
#' # vector of r values
#' iter <- 100
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- distribution(x)
#'
#' @include distribution-class.R
#' 
#' @export
distribution <- function(value, ...) UseMethod("distribution")
#' @export
distribution.list <- function(value, ...) new("distribution", value, ...)
    
# functionality
# add vector plus distribution -> calculate parameters
# add distribution plus pars <- simulate vector

