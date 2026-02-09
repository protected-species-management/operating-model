#' @title Population dynamics function
#' 
#' @description The population dynamics function is called per-iteration.
#' 
#' @export
#' @include om-class.R
#' @import RTMB
#' @import tmbstan
#{{{ pdyn()
# wrapper for execution of population
# dynamics function
# -- executes object@pdyn for each monte-carlo sample
setGeneric("pdyn", function(object, ...) standardGeneric("pdyn"))
setMethod("pdyn", signature = "om", function(object, ...) {
    
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
    
    # function for generating
    # mc realisations of the
    # operating model
    rng <- function(a, cv) {
        exp(log(a / sqrt(1 + cv^2)) + rnorm(1) * sqrt(log(1 + cv^2)))
    }
    
    pow <- function(x, y) x^y
    
    # reference point
    # estimator
    g <- MakeTape(function(x) { 
        
        # harvest rate
        h <- x[1]
        r <- x[2] 
        p <- x[3]
        
        htarget <- r / (p + 1)
        
        (htarget - h)^2 
        
    }, numeric(3))
    ff <- g$newton(1)
    
    # RTMB function
    fun <- function(parameters, data) {
        
        # load all available parameters
        # and data
        getAll(parameters, data, warn = FALSE)
        
        # back transform estimated
        # values
        r <- exp(log_r)
        K <- exp(log_K)
        
        # biomass vectors
        b_predict <- c(K * initial_depletion, exp(log_b_predict))
        b_target  <- c(K * initial_depletion, exp(log_b_target))
        
        # target harvest rate
        # (stochastic MNPL)
        target_harvest_rate <- ff(c(r, shape))
        
        # error term
        sigmap <- sqrt(log(1 + cv_dynamics^2))
        
        # AD vectors
        sp_predict   <- AD(numeric(n_time))
        sp_target    <- AD(numeric(n_time))
        mu           <- AD(numeric(n_time))
        xi           <- AD(numeric(n_time))
        pst          <- AD(numeric(n_time))
        catch        <- AD(numeric(n_time))
        depletion    <- AD(numeric(n_time))
        harvest_rate <- AD(numeric(n_time))
        
        # surplus production
        sp_predict <- r / shape * b_predict * (1 - (b_predict / K)^shape)
        sp_predict <- r / shape * b_target  * (1 - (b_target  / K)^shape)
        
        # expected annual dynamics
        # (input harvest rate)
        mu[1] <- b_predict[1]
        for (t in 2:n_time) {
            mu[t] <- b_predict[t - 1] + sp_predict[t - 1] - input_harvest_rate * b_predict[t - 1]
        }
        # (target harvest rate)
        xi[1] <- b_target[1]
        for (t in 2:n_time) {
            xi[t] <- b_target[t - 1] + sp_target[t - 1] - target_harvest_rate * b_target[t - 1]
        }
        
        # state equations
        log(b_predict) %~% dnorm(log(mu) - (sigmap^2) / 2, sigmap)
        log(b_target)  %~% dnorm(log(xi) - (sigmap^2) / 2, sigmap)
        
        # priors
        log_r %~% dnorm(log(0.35) - 0.005, 0.10)
        log_K %~% dnorm(log(1500) - 0.005, 0.10)
        
        # PST reference point
        pst <- phi * r * b_predict * K / 2
        
        # diagnostics
        catch        <- input_harvest_rate * b_predict
        depletion    <- b_predict / K
        harvest_rate <- catch / b_predict
        
        # reference points
        target_depletion <- b_target[n_time] / K
        target_catch     <- b_target[n_time] * target_harvest_rate
        
        # AD reports
        REPORT(r)
        REPORT(pst)
        REPORT(catch)
        REPORT(depletion)
        REPORT(harvest_rate)
        REPORT(target_harvest_rate)
        REPORT(target_catch)
        REPORT(target_depletion)
    }
    
    # set up objective function
    cmb <- function(f, d) function(p) f(p, d)
    obj <- MakeADFun(cmb(fun, object@data), object@pars)
    
    # initialise
    invisible(nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr))
    
    # run stan
    suppressWarnings({
        opt_stan <- tmbstan::tmbstan(obj, init = "last.par.best", chains = 1, iter = object@iter * 2)
    })
    
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
