#' @title Reference point calculation
#' @description 
#' Calculate the stochastic Maximum Net Productivity reference points.
#' 
#' @export
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R
#' @import RTMB
#' @import cli
#' @importFrom glue glue
#{{{ rp()
# wrapper for execution of function
setGeneric("rp", function(object, ...) standardGeneric("rp"))
setMethod("rp", signature = "om", function(object, ...) {
    
    # current environment
    ENV <- environment()
    
    # check environment for function call is
    # consistent with current environment
    #environment(object@population_dynamics) <- environment()
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    get_data(object, env = ENV)
    
    # setup numbers array
    if (any(is.na(ages))) {
        n <- array(dim = c(1, niter, ntime))
    } else {
        n <- array(dim = c(nages, niter, ntime))
    }
    
    # reset targets
    # (catch)
    object@targets$catch <- c()
    # (depletion)
    object@targets$depletion <- c()
    # (harvest rate)
    object@targets$harvest_rate <- c()
    
    # error term
    sigmap <- sqrt(log(1 + cv_dynamics^2))
    
    # accessor functions
    get_K <- function() exp(get("pars", envir = ENV)[1])
    get_r <- function() exp(get("pars", envir = ENV)[2])
    
    get_perr <- function() get("perr", envir = ENV)
    
    # initial values
    # (process error)
    perr <- matrix(rnorm(equ_time * equ_iter, 0 - (sigmap^2) / 2, sigmap), nrow = equ_iter, ncol = equ_time)
    # (parameter values - check order)
    pars <- unlist(lapply(object@pars, sample, n = 1))
    
    # AD objective function
    obj_fun <- function(x) { 
        
        # parameter 
        # value
        h <- exp(x)

        # fixed input
        p <- shape

        # monte-carlo
        # inputs
        r <- DataEval(get_r)
        K <- DataEval(get_K)
        e <- DataEval(get_perr)
        
        # AD matrix
        b <- AD(matrix(nrow = equ_iter, ncol = equ_time))
        
        # stochastic 
        # dynamics
        for (i in 1:equ_iter) {
            b[i, 1] <- (K * (1 / (p + 1))^(1 / p)) * exp(e[i, 1])
            for (j in 2:equ_time) {
                b[i, j] <- (b[i, j - 1] + r / p * b[i, j - 1] * (1 - (b[i, j - 1] / K)^p) - h * b[i, j - 1]) * exp(e[i, j])  
            }
        }
        
        # mean equilibrium catch
        # over most recent 10%
        # of the projection period
        recent_time <- ceiling(0.9 * equ_time):equ_time
        catch <- mean(b[, recent_time] * h)
        
        # return catch with penalty if
        # harvest rate is greater than 
        # deterministic rate
        return(-1 * catch + max(x - log(r / (p + 1)), 0))
    }
    
    # progress message
    cli_progress_message("Compiling model...")
    
    # initialise with 
    # parameter value
    g <- MakeTape(obj_fun, object@pars$log_r@pars[1] - log(object@data$shape + 1))
    # function to 
    # estimate minimum
    # over first argument
    # (harvest rate)
    ff <- g$newton(1)

    # progress message
    msg <- ""
    cli_progress_step("Estimating the harvest rate at MNPL{msg}", spinner = TRUE, msg_done = "Estimated MNPL reference points")

    # loop over monte-carlo
    # samples
    for (i in 1:niter) {
        
        # set seed
        set.seed(rng_seed[i])
        
        # progress iteration
        msg <- glue(", iteration {i}/", niter)
        
        # spin spinner
        cli_progress_update()
        
        # sample
        perr <- matrix(rnorm(equ_time * equ_iter, 0 - (sigmap^2) / 2, sigmap), nrow = equ_iter, ncol = equ_time)
        pars <- unlist(lapply(object@pars, sample, n = 1))

        # estimate h_mnpl per
        # monte-carlo sample
        h_mnpl <- exp(ff(numeric()))
        
        # spin spinner
        cli_progress_update()
        
        # project under harvest rate
        # at MNPL
        # {{{
        b <- matrix(nrow = equ_iter, ncol = equ_time)
        K <- exp(pars[1])
        r <- exp(pars[2])
        p <- shape
        
        # dynamics
        for (j in 1:equ_iter) {
            b[j, 1] <- (K * (1 / (p + 1))^(1 / p)) * exp(perr[j, 1])
            for (k in 2:equ_time) {
                b[j, k] <- (b[j, k - 1] + r / p * b[j, k - 1] * (1 - (b[j, k - 1] / K)^p) - h_mnpl * b[j, k - 1]) * exp(perr[j, k])  
            }
        }
        #}}}
        
        # spin spinner
        cli_progress_update()
        
        # update targets
        recent_time <- ceiling(0.9 * equ_time):equ_time
        # (catch)
        object@targets$catch <- c(object@targets$catch, mean(b[, recent_time] * h_mnpl))
        # (depletion)
        object@targets$depletion <- c(object@targets$depletion, mean(b[, recent_time] / K)) 
        # (harvest rate)
        object@targets$harvest_rate <- c(object@targets$harvest_rate, h_mnpl)
        
        # spin spinner
        cli_progress_update()
    }
    
    # plot relative to
    # deterministic
    # equivalents
    #windows(width = 21)
    #par(mfrow = c(1,3))
    #hist(object@targets$harvest_rate); abline(v = c(mean(object@targets$harvest_rate), object@pars$log_r$pars[1] - log(object@data$shape + 1)), lty = c(1,2))
    #hist(object@targets$catch);
    #hist(object@targets$depletion);
    
    # create distributions
    # (catch)
    #object@targets$catch <- distribution(list(value = object@targets$catch, distribution = "lognormal"))
    # (depletion)
    #object@targets$depletion <- distribution(list(value = object@targets$depletion, distribution = "lognormal"))
    # (harvest rate)
    #object@targets$harvest_rate <- distribution(list(value = object@targets$harvest_rate, distribution = "lognormal"))
    
    # return
    return(object)
})
#}}}
