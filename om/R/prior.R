#' 
#' @rdname prior-class
#' 
#' @param x either an integer specifiying the length of an empty vector or a vector of derived values
#' 
#' @examples
#' # create object containing
#' # vector of r values
#' iter <- 100
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- prior(x)
#'
#' @include prior-class.R
#' 
#' @export
prior <- function(value, ...) new("prior", value, ...)

