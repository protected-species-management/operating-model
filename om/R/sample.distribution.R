#' @title Sample from distribution class object
#' @description Overwrites the generic \code{sample} function to sample from a \code{distribution} class object.
#' @param x input distribution class object
#' @param size sample size
#' @param replace (ignored)
#' @param prob (ignored)
#' @importFrom logitnorm rlogitnorm
#' @importFrom cli cli_alert_warning
#' @details Monte-Carlo samples are generated from the parametric distribution contained in the \code{\link{distribution}} class object. If the distribution is \code{'unspecified'} then values are sampled from the values stored in the object (with replacement if necessary). If \code{x} is a numeric value rather than a distribution, then that value is return (this is designed to prevent the function from breaking when distributions are not specified). 
#' @export
sample <- function(x, ...) UseMethod("sample")
#' @rdname sample
#' @exportS3Method om::sample
sample.distribution <- function(x, size = 1, replace = NULL, prob = NULL) {
    
    # if only a single value then
    # return this value
    if (x@iter == 1) {
        
        return(rep(x@.Data, times = size)) 
        
    } else {
        
        # if a vector of values is stored then sample
        # from this vector (non-parametric)
        if (x@iter > 1 & size >= 1) {
            
            if (size <= length(x@.Data)) {
                return(x@.Data[sample.int(length(x@.Data), size = size, replace = FALSE)])
            } else {
                return(x@.Data[sample.int(length(x@.Data), size = size, replace = TRUE)])
            }
            
        } else {
            
            stopifnot(!any(is.na(x@pars)))
            
            y <- NA_real_
            
            # otherwise sample from parametric
            # distribution
            if (grepl("^uniform", x@density)) {
                y <- runif(size, min = x@pars[1], max = x@pars[2])    
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
sample.numeric <- function(x, size = 1, replace = NULL, prob = NULL) {
    
	cli_alert_warning("Found empty parameter (no distribution)")
	
    return(x)
}



