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
    
    # allowed input values
    # (must be arguments to population dynamics function)
    B0           <- tryCatch(as.numeric(object@productivity$B0), error = function(e) NULL)
    pars         <- tryCatch(as.matrix(object@productivity$pars), error = function(e) NULL)
    harvest      <- tryCatch(as.numeric(object@fishing$harvest), error = function(e) NULL)
    selectivity  <- tryCatch(as.matrix(object@fishing$selectivity), error = function(e) NULL)
    harvest_rate <- tryCatch(as.matrix(object@fishing$harvest_rate), error = function(e) NULL)
    
    maturity  <- tryCatch(as.matrix(object@life_history$maturity), error = function(e) NULL)
    mass      <- tryCatch(as.matrix(object@life_history$mass), error = function(e) NULL)
    fecundity <- tryCatch(as.matrix(object@life_history$fecundity), error = function(e) NULL)
    size      <- tryCatch(as.matrix(object@life_history$size), error = function(e) NULL)
    h         <- tryCatch(as.numeric(object@life_history$h), error = function(e) NULL)
    M         <- tryCatch(as.matrix(object@life_history$M), error = function(e) NULL)
    
    tmax   <- length(object@time)
    ages   <- object@ages
    time   <- object@time
    niter  <- object@iter
    nage   <- length(ages)
    
    n <- array(dim = c(nage, tmax, niter))
    
    for(i in 1:niter) {
        n[,,i] <- object@population_dynamics(B0 = B0[i],
                                harvest = harvest,
                                harvest_rate = harvest_rate[,i],
                                time = time,
                                selectivity = selectivity[,i],
                                maturity = maturity[,i],
                                mass = mass[,i],
                                fecundity = fecundity[,i],
                                size = size[,i],
                                #h = h[i],
                                pars = pars[,i],
                                M = M[,i],
                                tmax = tmax,
                                ages = ages
                                )
    }
    
    dimnames(n) <- list(age = ages, time = time, iter = 1:niter)
    
    object@.Data <- n
    
    return(object)
})
#}}}
