#' @title Sample from distribution class object
#' @description Overwrites the generic \code{sample} function to sample from a \code{distribution} class object.
#' @param x input distribution class object
#' @param size sample size
#' @param ... (ignored)
#' @importFrom logitnorm rlogitnorm
#' @importFrom cli cli_alert_warning cli_abort
#' @details Monte-Carlo samples are generated from a \code{\link{distribution}} class object. If values are stored in the object then these are sampled non-parameterically (with replacement if necessary). If values are not present, and \code{pars} and \code{density} are specified in the object, then parametric sampling is performed.
#' @seealso \code{\link{distribution}}
#' @examples
#' # non-parametric
#' # sampling
#' x <- distribution(values = 1:3)
#' sample(x, 3)
#' 
#' # non-parametric
#' # sampling
#' x <- distribution(values = 1:3, density = "uniform")
#' sample(x, 3)
#' 
#' # parametric sampling
#' x[] <- numeric()
#' sample(x, 3)

#' @export
sample <- function(x, size, ...) UseMethod("sample")
#' @rdname sample
#' @exportS3Method om::sample
sample.distribution <- function(x, size = 1, ...) {
    
    # if only a single value then
    # return this value
    if (length(x@.Data) == 1) {
        
        return(rep(x@.Data, times = size)) 
        
    } else {
        
        # if a vector of values is stored then sample
        # from this vector (non-parametric)
        if (length(x@.Data) > 1 & size >= 1) {
            
            if (size <= length(x@.Data)) {
                return(x@.Data[sample.int(length(x@.Data), size = size, replace = FALSE)])
            } else {
                return(x@.Data[sample.int(length(x@.Data), size = size, replace = TRUE)])
            }
            
        } else {
            
            if (any(is.na(x@pars))) cli_abort("'@pars' is empty")
            if (x@density == "unspecified") cli_abort("'@density' is unspecified")
            
            y <- NA_real_
            
            # otherwise sample from parametric
            # distribution
            if (grepl("^uniform", x@density)) {
                y <- runif(size, min = x@pars[1], max = x@pars[2])    
            }
            
            if (grepl("^int?.uniform", x@density)) {
                y <- (x@pars[1]:x@pars[2])[sample.int(length(x@pars[1]:x@pars[2]), size, replace = TRUE)]
            }
            
            if (grepl("^beta", x@density)) {
                y <- rbeta(size, shape1 = x@pars[1], shape2 = x@pars[2])    
            }
            
            if (grepl("^normal", x@density)) {
                y <- rnorm(size, mean = x@pars[1], sd = x@pars[2]) 
            }
            
            if (grepl("^zt?.normal", x@density)) {
                t <- (0 - x@pars[1]) / x@pars[2] 
                y <- x@pars[1] + x@pars[2] * qnorm(runif(size, pnorm(t), pnorm(Inf)))
            }
            
            if (grepl("^log?.normal", x@density)) {
                y <- rlnorm(size, meanlog = x@pars[1], sdlog = x@pars[2])    
            }
            
            if (grepl("^gamma", x@density)) {
                y <- rgamma(size, shape = x@pars[1], scale = x@pars[2])    
            }
            
            if (grepl("^logit?.normal", x@density)) {
                y <- rlogitnorm(size, mu = x@pars[1], sigma = x@pars[2])    
            }
            
            return(y)
        }
    }
}
#' @rdname sample
#' @exportS3Method om::sample
sample.numeric <- function(x, size = 1, replace = FALSE, ...) {
    
    if (size <= length(x)) {
        return(x[sample.int(length(x), size = size, replace = replace)])
    } else {
        if (!replace) {
            cli_alert_warning("setting 'replace <- TRUE'")
        }
        return(x[sample.int(length(x), size = size, replace = TRUE)])
    }
}



