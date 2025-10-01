#' @title pdyn
#' 
#' @description Population dynamics function
#' 
#' @export
#' 
#' @include om-class.R
#' 
#{{{ pdyn()
# wrapper for execution of population
# dynamics function
# -- strips out data from om object and
# -- executes object@pdyn for each monte-carlo
# -- sample
setGeneric("pdyn", function(object, ...) standardGeneric("pdyn"))
setMethod("pdyn", signature = "om", function(object, ...) {
    
    # strip out data for speed
    B0          <- as.numeric(object@B0)
    harvest     <- as.numeric(object@empirical_data$harvest)
    time        <- as.integer(object@empirical_data$time)
    selectivity <- as.matrix(object@selectivity)
    
    maturity <- as.matrix(object@lh_data$maturity)
    mass     <- as.matrix(object@lh_data$mass)
    size     <- as.matrix(object@lh_data$size)
    h        <- as.numeric(object@lh_data$h)
    M        <- as.matrix(object@lh_data$M)
    
    tmax   <- length(object@empirical_data$time)
    ainf   <- object@ainf
    niter  <- object@iter
    
    n <- array(dim=c(ainf, tmax, niter))
    
    for(i in 1:niter) {
        n[,,i] <- object@pdyn(B0 = B0[i],
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
    
    object@n <- n
    
    return(object)
})
#}}}
