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
    
    # current environment
    ENV <- environment()
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    # get data
    get_data(object, env = ENV)
    
    # setup numbers array
    if (any(is.na(ages))) {
        n <- array(dim = c(1, niter, ntime))
    } else {
        n <- array(dim = c(nages, niter, ntime))
    }
    
    # error term
    sigmap <- sqrt(log(1 + object@data$cv_dynamics^2))
    
    harvest_rate <- function(object, i) {
        
        object@targets$harvest_rate[i]
    }
    
    # setup diagnostics
    # (catch)
    object@diagnostics$catch <- matrix(NA_real_, nrow = iter, ncol = ntime)
    # (depletion)
    object@diagnostics$depletion <- matrix(NA_real_, nrow = iter, ncol = ntime)
    # (harvest rate)
    object@diagnostics$harvest_rate <- matrix(NA_real_, nrow = iter, ncol = ntime)
    
    msg <- ""
    cli_progress_step("Projecting dynamics{msg}", spinner = TRUE, msg_done = "Projected dynamics")
    
    for (i in 1:niter) {
        
        # set seed
        set.seed(rng_seed[i])
        
        # progress iteration
        msg <- glue(", iteration {i}/", niter)
        
        # spin spinner
        cli_progress_update()
        
        # sample
        perr <- matrix(rnorm(ntime * equ_iter, 0 - (sigmap^2) / 2, sigmap), nrow = equ_iter, ncol = ntime)
        pars <- unlist(lapply(object@pars, sample, n = 1))
        
        # spin spinner
        cli_progress_update()
        
        # project under harvest rate
        # at MNPL
        # {{{
        b <- matrix(NA_real_, nrow = equ_iter, ncol = ntime)
        K <- exp(pars[1])
        r <- exp(pars[2])
        p <- object@data$shape
        h <- matrix(NA_real_, nrow = equ_iter, ncol = ntime)
        
        # dynamics
        for (j in 1:equ_iter) {
            
            b[j, 1] <- (K * initial_depletion) * exp(perr[j, 1])
            
            for (k in 2:ntime) {
                
                h[j, k - 1] <- harvest_rate(object, i)
                
                b[j, k] <- (b[j, k - 1] + r / p * b[j, k - 1] * (1 - (b[j, k - 1] / K)^p) - h[j, k - 1] * b[j, k - 1]) * exp(perr[j, k])  
            }
        }
        #}}}
        
        # spin spinner
        cli_progress_update()
        
        # update diagnostics
        # (catch)
        object@diagnostics$catch[i,] <- apply(b * h, 2, mean)
        # (depletion)
        object@diagnostics$depletion[i,] <- apply(b / K, 2, mean)
        # (harvest rate)
        object@diagnostics$harvest_rate[i,] <- apply(h, 2, mean)
        
        # spin spinner
        cli_progress_update()
    }

    # calculate objectives as the probability
    # of a desirable outcome
    p_higher <- function(x, y) ifelse(x > y, 1, ifelse(x < y, 0, 0.5))
    p_lower  <- function(x, y) ifelse(x < y, 1, ifelse(x > y, 0, 0.5))
    # (prob. that catch is less than that required to meet MNPL)
    object@objectives$catch        <- apply(sweep(object@diagnostics$catch,        1, object@targets$catch, p_lower),        1, mean, na.rm = TRUE)
    # (prob. that depletion is greater than the depletion at MNPL)
    object@objectives$depletion    <- apply(sweep(object@diagnostics$depletion,    1, object@targets$depletion, p_higher),   1, mean, na.rm = TRUE)
    # (prob. that harvest rate is less than that required to meet MNPL)
    object@objectives$harvest_rate <- apply(sweep(object@diagnostics$harvest_rate, 1, object@targets$harvest_rate, p_lower), 1, mean, na.rm = TRUE)
    
    # dimnames (after calculations)
    dimnames(n) <- list(age = ages, iter = 1:niter, time = time)
    
    # assign data
    object@.Data <- n
    
    return(object)
})
#}}}
