#' @title pdyn
#' 
#' @description Population dynamics function
#' 
#' @export
#' 
#' @include dsm-class.R
#' 
#{{{ pdyn()
# wrapper for execution of population
# dynamics function
# -- strips out data from dsm object and
# -- executes .Object@pdyn for each monte-carlo
# -- sample
setGeneric("pdyn", function(.Object, ...) standardGeneric("pdyn"))
setMethod("pdyn", signature = "dsm", function(.Object, ...) {
    
    # strip out data for speed
    B0          <- as.numeric(.Object@B0)
    harvest     <- as.numeric(.Object@empirical.data$harvest)
    time        <- as.integer(.Object@empirical.data$time)
    selectivity <- as.matrix(.Object@selectivity)
    
    maturity <- as.matrix(.Object@lh.data$maturity)
    mass     <- as.matrix(.Object@lh.data$mass)
    size     <- as.matrix(.Object@lh.data$size)
    h        <- as.numeric(.Object@lh.data$h)
    M        <- as.matrix(.Object@lh.data$M)
    
    tmax   <- length(.Object@empirical.data$time)
    ainf   <- .Object@ainf
    niter  <- .Object@iter
    
    n <- array(dim=c(ainf, tmax, niter))
    
    for(i in 1:niter) {
        n[,,i] <- .Object@pdyn(B0 = B0[i],
                                harvest = harvest,
                                time = time,
                                selectivity = selectivity[,i],
                                maturity = maturity[,i],
                                mass = mass[,i],
                                size = size[,i],
                                h = h[i],
                                M = M[,i],
                                tmax = tmax,
                                ainf = ainf
                                )
    }
    
    dimnames(n) <- list(age = 1:ainf, time = time, iter = 1:niter)
    
    .Object@n <- n
    
    .Object
})
#}}}
