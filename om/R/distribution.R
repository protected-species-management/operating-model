#' @rdname distribution-class
#' @param values vector of values to be stored in the object. If a \code{density} argument is also supplied, then parameters for that distribution are estimated from the \code{values} using maximum likelihood.
#' @param pars vector of length two containing parameters for the parametric distribution specified by the \code{density} argument. If \code{pars} are provided but no \code{density} then this generates a warning.
#' @param density character string specifying the probability density (or mass) function. Can be one of \code{beta}, \code{int-uniform}, \code{uniform}, \code{normal}, \code{lognormal}, \code{logitnormal} or \code{gamma}.
#' @param ... optional \code{name} or \code{iter} arguments
#' @examples
#' # create object containing
#' # vector of values
#' iter <- 1e3
#' cv <- 0.2
#' sd <- sqrt(log(1 + cv^2))
#' mu <- log(1) - sd^2/2
#' x <- rlnorm(iter, mu, sd)
#' y <- distribution(value = x, density = "lognormal")
#' 
#' # show
#' y
#' 
#' # summarise
#' summary(y)
#' 
#' # when values are provided
#' # then sampling is non-parametric
#' all(sample(y, size = length(y)) %in% x)
#'  
#' # create object
#' # without values
#' z <- distribution(pars = c(mu, sd), density = "lognormal")
#'
#' # with no values the
#' # sampling is parametric
#' all(sample(z, size = length(y)) %in% x)
#'
#' # plotting will show histogram
#' # if values are present
#' plot(y)
#' plot(z)
#' 
#' # object otherwise behaves
#' # like a numeric vector:
#' length(y)
#' y[1:10] <- 3
#' y 
#'
#' # removing values from
#' # object to allow parametric
#' # sampling
#' y[] <- numeric(0)
#' y
#' 
#' # if values are provided but no density
#' # then sampling is always non-parametric
#' z <- distribution(values = 0:10, density = "unspecified")
#' sample(z, size = 3)
#' 
#' # if pars are provided but no density
#' # then no values can be simulated
#' z <- distribution(pars = c(mu, sd))
#' z
#' try(sample(z, size = 3))
#'  
#' @include distribution-class.R sample.distribution.R
#' @importFrom methods new
#' @export
distribution <- function(values, pars, density, ...) new("distribution", values = values, pars = pars, density = density, ...)



