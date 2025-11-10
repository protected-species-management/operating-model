#' @title productivity_pars
#' 
#' @description Calculate productivity parameters for use within the \code{population_dynamics()} function.
#' 
#' @export
#' 
#' @include om-class.R
#' 
#{{{ calculate productivity
setGeneric("productivity_pars", function(object, ...) standardGeneric("productivity_pars"))
setMethod("productivity_pars", signature = c("om"), function(object, ...) {
    
    nage  <- length(object@ages)
    niter <- object@iter
    p    <- vector("numeric", length = nage)
    
    pars <- matrix(0, ncol = niter, nrow = 2)
    
    for(i in 1:niter) {      # THIS NEEDS TO BE A FUNCTION CALL PER ITERATION
        
        # set up equilibrium population
        p[1] <- 1
        for(a in 2:nage) {
            p[a] <- p[a - 1] * exp(-object@life_history$M[a - 1,i])
        }
        p[nage] <- p[nage] / (1 - exp(-object@life_history$M[nage, i]))
        rho <- sum(p * object@life_history$maturity[,i] * object@life_history$mass[,i])
        R0 <- object@productivity$B0[i] / rho
        
        # alpha
        pars[1, i] <- (4 * object@life_history$h[,i] * R0) / (5 * object@life_history$h[,i] - 1)
        
        # beta
        pars[2, i] <- object@productivity$B0[i] * (1 - object@life_history$h[,i]) / (5 * object@life_history$h[,i] - 1)
    }
    
    dimnames(pars) <- list(par = c('alpha', 'beta'), iter = 1:niter)
    
    object@productivity$pars <- pars
    
    return(object)
})
#}}}




