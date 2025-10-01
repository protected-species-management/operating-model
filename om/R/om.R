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
om <- function(...) new('om', pdyn_function = .pdyn, iter = 1, ...)
#}}}
#{
# default population dynamics function
# (should accept single monte-carlo sample only)
.pdyn <- function(B0, harvest, time, selectivity, maturity, mass, size, h, M, tmax, ainf) {
    
    n    <- array(dim=c(ainf, tmax))
    bmat <- vector("numeric",length=tmax)
    bexp <- vector("numeric",length=tmax)
    hr   <- vector("numeric",length=tmax)
    p    <- vector("numeric",length=ainf)
    
    # set up equilibrium population
    p[1] <- 1
    for(a in 2:ainf)
        p[a] <- p[a-1]*exp(-M[a-1])
    p[ainf] <- p[ainf]/(1-exp(-M[ainf]))
    rho <- sum(p * maturity * mass)
    R0 <- B0 / rho
    n[,1] <- R0 * p
    bmat[1] <- sum(n[,1] * maturity * mass)
    bexp[1] <- sum(n[,1] * selectivity * mass)
    hr[1] <- harvest[1] / bexp[1]
    hr[1] <- max(hr[1],0)
    hr[1] <- min(hr[1],0.999)
    
    # set up S-R parameters
    alp <- (4*h*R0)/(5*h-1)
    bet <- B0*(1-h)/(5*h-1)
    
    for(y in 2:tmax) {
        
        n[1,y] <- alp * bmat[y-1]/(bet + bmat[y-1])
        #n[1,y] <- n[1,y] * srr[y]
        for(a in 2:ainf)
            n[a,y] <- n[a-1,y-1]*exp(-M[a-1])*(1-selectivity[a-1]*hr[y-1])
        n[ainf,y] <- n[ainf,y] + n[ainf,y-1]*exp(-M[a-1])*(1-selectivity[ainf]*hr[y-1])
        bexp[y] <- sum(n[,y] * selectivity * mass)
        hr[y] <- harvest[y] / bexp[y]
        hr[y] <- max(hr[y],0)
        hr[y] <- min(hr[y],0.999)
        bexp[y] <- harvest[y] / hr[y]
        bmat[y] <- sum(n[,y] * maturity * mass)
    }
    
    # retun numbers at age
    return(array(n, dim = dim(n), dimnames = list(age = 1:ainf, time = time)))
}
#}
