#' @title Class containing a prior distribution derived from life-history data
#' 
#' @description This is an S4 object class that includes both a numeric vector for storage of derived values generated using Monte Carlo methods, such as the intrinsic growth rate \eqn{r}, and a list of parameters describing the associated parameteric distribution. Currently only a log-normal distribution is supported. If \code{length(x)>1} then the function creates an object containing values of \code{x}, otherwise it creates a vector of zero's of length equal to \code{x}. For example, values for \eqn{r} can be simulated directly or generated using the \code{\link{rCalc}} function. The class contains an additional slot to hold parameters of the log-normal distribution to describe the prior for \eqn{r}.
#' 
#' @slot .Data numeric vector of derived values
#' @slot iter integer value
#' @slot pars log-normal distribution parameter values
#'
#' @export
setClass("prior", contains = "numeric", slots = list(iter = "integer", distribution = "character", pars = "list", name = "character"))

setMethod("initialize", "prior", function(.Object, x) {
    
    if (missing(x)) .Object@.Data <- numeric()
    else {
        if (length(x[!is.na(x)]) > 1) {
            
            x <- as.numeric(x[!is.na(x)])
            
            .Object@.Data <- x
            
            # calculate log-normal pars
            
            # transform to normal
            y <- log(x)
            
            # estimate parameters of
            # normal distribution log(x)
            mu     <- mean(y)
            sigma  <- sd(y)
            sigma2 <- sigma^2
            
            # estimate parameters of
            # log-normal distribution
            theta <- exp(mu + sigma2/2)
            nu    <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
            cv    <- sqrt(exp(sigma2) - 1)
            
            # assign
            .Object@iter         <- length(x)
            .Object@distribution <- "lognormal"
            .Object@pars         <- list('E[log(x)]' = mu, 'SD[log(x)]' = sigma, 'E[x]' = theta, 'VAR[x]' = nu, 'CV[x]' = cv)
            
        } else {
            if (length(x[!is.na(x)]) == 1) {
                
                x <- as.numeric(x[!is.na(x)])
                .Object@.Data <- x
                .Object@iter  <- 1L
                
            } else {
                .Object@.Data <- rep(NA_real_, length(x))
            }
        }
    }
    return(.Object)
})

# {{{
setMethod("show", "prior",
          function(object) {
              message(blue("prior S4 object class"))
              message("iter: ", object@iter)
              message("distribution: ", object@distribution)
              message("pars: ", object@pars)
              message("values: ", if (length(object@.Data) == 0) red("EMPTY") else object@.Data)
          })
# }}}




