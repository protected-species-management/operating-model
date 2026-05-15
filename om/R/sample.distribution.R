#' @title Sample from distribution class object
#' @import logitnorm
#' @export
sample <- function(x, n, ...) UseMethod("sample")
#' @rdname sample
#' @export
sample.distribution <- function(x, n = 1, ...) {
    
    # if only a single value then
    # return this value
    if (x@iter == 1 & n == 1) {
        
        return(x@.Data) 
        
    } else {
        
        # if a vector of values is stored then sample
        # from this vector (non-parametric)
        if (x@iter > 1 & n >= 1 & n <= length(x@.Data)) {
            
            return(x@.Data[sample.int(length(x@.Data), size = n, replace = FALSE)])
            
        } else {
            
            stopifnot(!any(is.na(x@pars)))
            
            y <- NA_real_
            
            # otherwise sample from parametric
            # distribution
            if (grepl("^uniform", x@density)) {
                y <- runif(n, min = x@pars[1], max = x@pars[2])    
            }
            
            if (grepl("^normal", x@density)) {
                y <- rnorm(n, mean = x@pars[1], sd = x@pars[2]) 
            }
            
            if (grepl("^log?normal", x@density)) {
                y <- rlnorm(n, meanlog = x@pars[1], sdlog = x@pars[2])    
            }
            
            if (grepl("^gamma", x@density)) {
                y <- rgamma(n, shape = x@pars[1], scale = x@pars[2])    
            }
            
            if (grepl("^logit?normal", x@density)) {
                y <- rlogitnorm(n, mu = x@pars[1], sigma = x@pars[2])    
            }
            
            return(y)
        }
    }
}
#' @rdname sample
#' @export
sample.numeric <- function(x, n = 1, ...) {
    
    return(x)
}



