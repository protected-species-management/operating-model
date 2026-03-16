#' @title Sample from distribution class object
#' @export
sample <- function(x, n, ...) UseMethod("sample")
#' @rdname sample
#' @export
sample.distribution <- function(x, n = 1, ...) {
    
    if (x@iter == 1 & n == 1) {
        
        return(x@.Data) 
        
    } else {
        
        stopifnot(!any(is.na(x@pars)))
        
        y <- NA_real_
        
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
        
        return(y)
    }
}


