#' @title Create om object
#' 
#' @description Initialise Data simulation module (om) class object
#' 
#' @export
#' 
#' @include om-initialize.R
#'
#{{{
# constructor
om <- function(pdyn_function = .pdyn, iter = 1, ...) new('om', pdyn_function, iter, ...)
#}}}
#{
# default population dynamics function
# (should accept single monte-carlo sample only)
.pdyn <- function(B0, harvest, harvest_rate, time, selectivity, maturity, mass, fecundity, size, h, M, tmax, ages) {
    
    nage <- length(ages)
    n    <- array(dim = c(nage, tmax))
    bmat <- vector("numeric", length = tmax)
    bexp <- vector("numeric", length = tmax)
    hr   <- vector("numeric", length = tmax)
    p    <- vector("numeric", length = nage)
    
    trim <- function(x) min(max(x, 0), 1)
    
    # set up equilibrium population
    p[1] <- 1
    for(a in 2:nage)
        p[a] <- p[a-1]*exp(-M[a-1])
    p[nage] <- p[nage]/(1-exp(-M[nage]))
    rho <- sum(p * maturity * mass)
    R0 <- B0 / rho
    n[,1] <- R0 * p
    bmat[1] <- sum(n[,1] * maturity * mass)
    bexp[1] <- sum(n[,1] * selectivity * mass)
    hr[1]   <- trim(harvest[1] / bexp[1])
    
    # set up S-R parameters
    alp <- (4*h*R0) / (5*h-1)
    bet <- B0*(1-h) / (5*h-1)
    
    for(y in 2:tmax) {
        
        n[1,y] <- alp * bmat[y-1]/(bet + bmat[y-1])
        for(a in 2:nage)
            n[a,y] <- n[a-1,y-1]*exp(-M[a-1])*(1-selectivity[a-1]*hr[y-1])
        n[nage,y] <- n[nage,y] + n[nage,y-1]*exp(-M[a-1])*(1-selectivity[nage]*hr[y-1])
        bexp[y] <- sum(n[,y] * selectivity * mass)
        hr[y]   <- trim(harvest[y] / bexp[y])
        bexp[y] <- harvest[y] / hr[y]
        bmat[y] <- sum(n[,y] * maturity * mass)
    }
    
    # retun numbers at age
    return(array(n, dim = dim(n), dimnames = list(age = ages, time = time)))
}
#}
