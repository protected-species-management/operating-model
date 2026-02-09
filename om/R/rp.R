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
    
    # equilibrium time is hard-wired
    equ_time <- 200
    
    # setup numbers array
    if (any(is.na(ages))) {
        n <- array(dim = c(1, ntime, niter))
    } else {
        n <- array(dim = c(nages, ntime, niter))
    }
    
    # RTMB function
    fun <- function(parameters, data) {
        
        # load all available parameters
        # and data
        getAll(parameters, data, warn = FALSE)
        
        # back transform estimated
        # values
        h <- target_harvest_rate <- exp(log_target_harvest_rate)
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
            mu[i] <- b[i - 1] + r / shape * b[i - 1] * (1 - (b[i - 1] / K)^shape) - h * b[i - 1]  
        }
        
        # state equation
        log(b) %~% dnorm(log(mu) - (sigmap^2) / 2, sigmap)
        
        # reference points
        target_depletion <- b[n_time] / K
        target_catch     <- b[n_time] * h
        
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
    object_data$equ_time <- equ_time
    
    object_pars <- object@pars
    object_pars$log_K <- NULL
    object_pars$log_r <- NULL
    
    # iterate
    ff <- function() {
        
        obj <- MakeADFun(cmb(fun, object@data), object@pars, random = c("log_b"), silent = TRUE)
        invisible(optim(par = obj$par, fn = obj$fn, gr = obj$gr, method = "Brent", lower = -10, upper = 0))
    
        obj$report()
    }
    
    
    
    
    # get generated quantities
    post <- as.data.frame(opt_stan)
    
    # PST
    object@pst$r     <- apply(post, 1, \(x) obj$report(x)$r)
    object@pst$value <- apply(post, 1, \(x) obj$report(x)$pst) |> t() #%>% array2dfr(dim.names = list(iteration = 1:object@iter, time = object@time))
    
    # diagnostics
    # (catch)
    object@diagnostics$catch <- apply(post, 1, \(x) obj$report(x)$catch) |> t() #%>% array2dfr(dim.names = list(iteration = 1:object@iter, time = object@time))
    # (depletion)
    object@diagnostics$depletion <- apply(post, 1, \(x) obj$report(x)$depletion) |> t() #%>% array2dfr(dim.names = list(iteration = 1:object@iter, time = object@time))
    # (harvest rate)
    object@diagnostics$harvest_rate <- apply(post, 1, \(x) obj$report(x)$harvest_rate) |> t()# %>% array2dfr(dim.names = list(iteration = 1:object@iter, time = object@time))
    

    # targets
    # (catch)
    object@targets$catch <- apply(post, 1, \(x) obj$report(x)$target_catch) 
    # (depletion)
    object@targets$depletion <- apply(post, 1, \(x) obj$report(x)$target_depletion)
    # (harvest rate)
    object@targets$harvest_rate <- apply(post, 1, \(x) obj$report(x)$target_harvest_rate)
    
    
    # calculate objectives
    # (catch is less than that required to meet MNPL)
    object@objectives$catch        <- apply(sweep(object@diagnostics$catch,        1, object@targets$catch, '<='), 1, mean, na.rm = TRUE)
    # (depletion is greater than the depletion at MNPL)
    object@objectives$depletion    <- apply(sweep(object@diagnostics$depletion,    1, object@targets$depletion, '>='), 1, mean, na.rm = TRUE)
    # (harvest rate is less than that required to meet MNPL)
    object@objectives$harvest_rate <- apply(sweep(object@diagnostics$harvest_rate, 1, object@targets$harvest_rate, '<='), 1, mean, na.rm = TRUE)
    
    # dimnames (after calculations)
    dimnames(n) <- list(age = ages, time = time, iter = 1:niter)
    
    # assign data
    object@.Data <- n
    
    return(object)
})
#}}}
