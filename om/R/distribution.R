#' @rdname distribution-class
#' 
#' @param ... optional input arguments: \code{values}, \code{name}, \code{pars}, \code{density}
#' @note density distribution specified by \code{density} argument can be one of \code{beta}, \code{uniform}, \code{normal}, \code{lognormal}, \code{logitnormal} or \code{gamma}
#' @examples
#' # create object containing
#' # vector of values
#' iter <- 1e5
#' cv <- 0.2
#' sd <- sqrt(log(1 + cv^2))
#' mu <- log(1) - sd^2/2
#' x <- rlnorm(iter, mu, sd)
#' y <- distribution(value = x, density = "lognormal")
#' 
#' # show
#' y
#' 
#' # plot histogram
#' hist(y)
#' abline(v = mean(y), col = 2)
#' 
#' # summarise
#' summary(y)
#' 
#' # create object
#' # without values
#' y <- distribution(pars = c(mu, sd), density = "lognormal")
#'
#' # summarise
#' summary(y)
#'
#' # plot
#' plot(y)
#' 
#' # parametric sampling
#' hist(sample(y, size = 1e5))
#' 
#' # non-parametric sampling
#' z <- distribution(values = 0:10, density = "unspecified")
#' sample(z, size = 3)
#'  
#' @include distribution-class.R sample.distribution.R
#' @importFrom methods new
#' @export
distribution <- function(...) UseMethod("distribution")
#' @export
distribution.numeric <- function(...) new("distribution", ...)
#' @export
distribution.character <- function(...) new("distribution", ...)
    
# functionality
# add vector plus distribution -> calculate parameters
# add distribution plus pars <- simulate vector

