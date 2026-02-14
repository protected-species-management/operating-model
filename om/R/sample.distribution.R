#' @title Sample from distribution class object
#' @export
sample <- function(x, n, ...) UseMethod("sample")
#' @rdname sample
#' @export
sample.distribution <- function(x, n, ...) {
    
    stopifnot(!any(is.na(x@pars)))
    
    y <- NA_real_
    
    if (grepl("^uniform", x@distribution)) {
        y <- runif(n, min = x@pars[1], max = x@pars[2])    
    }
    
    if (grepl("^normal", x@distribution)) {
        y <- rnorm(n, mean = x@pars[1], sd = x@pars[2]) 
    }
    
    if (grepl("^log?normal", x@distribution)) {
        y <- rlnorm(n, meanlog = x@pars[1], sdlog = x@pars[2])    
    }
    
    if (grepl("^gamma", x@distribution)) {
        y <- rgamma(n, shape = x@pars[1], scale = x@pars[2])    
    }
    
    return(y)
}


