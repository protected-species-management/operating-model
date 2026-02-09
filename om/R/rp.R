#' @title Reference point calculation
#' @description 
#' Calculate the stochastic Maximum Net Productivity reference points.
#' 
#' @export
#' @include om-class.R
#' @import RTMB
#' @import tmbstan
#{{{ rp()
# wrapper for execution of function
setGeneric("rp", function(object, ...) standardGeneric("rp"))
setMethod("rp", signature = "om", function(object, ...) {
    
    # check environment for function call is
    # consistent with current environment
    environment(object@population_dynamics) <- environment()
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = environment())
    
    # setup numbers array
    if (any(is.na(ages))) {
        n <- array(dim = c(1, ntime, niter))
    } else {
        n <- array(dim = c(nages, ntime, niter))
    }
    
    # RTMB function
    fun <- function(parameters, data) {
    #fun <- function(parameters) {   
        
        #r <- 0.04
        #K <- 3500
        #equ_time <- 200
        #shape <- 1
        #cv_dynamics <- 0.055
        
        # load all available parameters
        # and data
        getAll(parameters, data, warn = FALSE)
        
        # back transform estimated
        # values
        target_harvest_rate <- exp(log_target_harvest_rate)
        b <- exp(log_b)
        
        # error term
        sigmap <- sqrt(log(1 + cv_dynamics^2))
        
        # AD vector
        mu <- AD(numeric(equ_time))

        # annual dynamics
        # (initialised at
        # deterministic value)
        mu[1] <- K * (1 / (shape + 1))^(1 / shape)
        for (i in 2:equ_time) {
            mu[i] <- b[i - 1] + r / shape * b[i - 1] * (1 - (b[i - 1] / K)^shape) - target_harvest_rate * b[i - 1]  
        }
        
        # state equation
        log(b) %~% dnorm(log(mu) - (sigmap^2) / 2, sigmap)
        
        # reference points
        target_depletion <- b[equ_time] / K
        target_catch     <- b[equ_time] * target_harvest_rate
        
        # AD reports
        REPORT(target_harvest_rate)
        REPORT(target_catch)
        REPORT(target_depletion)
        
        #catch_target <- r * K * (1 / (shape + 1))^((shape + 1) / shape)
        
        #return((catch_target - (r * K * (1 / (shape + 1))^((shape + 1) / shape)))^2)
        
        # return
        return(-1 * target_catch)
    }
    
    # set up objective function
    cmb <- function(f, d) function(p) f(p, d)
    
    object_data   <- object@data
    object_data$r <- exp(object@pars$log_r)
    object_data$K <- exp(object@pars$log_K)
        
    object_pars <- object@pars
    object_pars$log_K <- NULL
    object_pars$log_r <- NULL
    
    # iterate
    ff <- function() {
        
        object_data$r <- rlnorm(1, log(0.04), 0.01)
        
        obj <- MakeADFun(cmb(fun, object_data), object_pars, random = c("log_b"), silent = TRUE)
        invisible(optim(par = obj$par, fn = obj$fn, gr = obj$gr, method = "Brent", lower = -10, upper = 0))
    
        obj$report()
    }
    
    values <- list()
    for (i in 1:300) {
        values[[i]] <- ff()
    }
    values <- bind_rows(values)
    
    # targets
    # (catch)
    object@targets$catch <- values$target_catch 
    # (depletion)
    object@targets$depletion <- values$target_depletion
    # (harvest rate)
    object@targets$harvest_rate <- values$target_harvest_rate
    
    
    return(object)
})
#}}}
