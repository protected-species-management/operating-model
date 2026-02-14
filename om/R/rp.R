#' @title Reference point calculation
#' @description 
#' Calculate the stochastic Maximum Net Productivity reference points.
#' 
#' @export
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R
#' @import RTMB
#' @import tmbstan
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
    get_dim(object, env = environment())
    
    # setup numbers array
    if (any(is.na(ages))) {
        n <- array(dim = c(1, ntime, niter))
    } else {
        n <- array(dim = c(nages, ntime, niter))
    }
    
    # reset targets
    # (catch)
    object@targets$catch <- c()
    # (depletion)
    object@targets$depletion <- c()
    # (harvest rate)
    object@targets$harvest_rate <- c()
    
    # error term
    sigmap <- sqrt(log(1 + object@data$cv_dynamics^2))
    
    # accessor functions
    get_K <- function() exp(get("pars", envir = ENV)[1])
    get_r <- function() exp(get("pars", envir = ENV)[2])
    
    get_process_error <- function() get("perr", envir = ENV)
    
    # initial values
    # (process error)
    perr <- matrix(rnorm(object@data$equ_time * object@data$equ_iter, 0 - (sigmap^2) / 2, sigmap), nrow = object@data$equ_iter, ncol = object@data$equ_time)
    # (parameter values - check order)
    pars <- unlist(lapply(object@pars, sample, n = 1))
    
    # AD objective function
    obj_fun <- function(x) { 
        
        # parameter 
        # value
        h <- exp(x)

        # inputs
        r <- DataEval(get_r)
        K <- DataEval(get_K)
        p <- object@data$shape
        e <- DataEval(get_process_error)
        
        # AD matrix
        b <- AD(matrix(nrow = object@data$equ_iter, ncol = object@data$equ_time))
        
        # dynamics
        for (i in 1:object@data$equ_iter) {
            b[i,1] <- (K * (1 / (p + 1))^(1 / p)) * exp(e[i,1])
            for (j in 2:object@data$equ_time) {
                b[i,j] <- (b[i, j - 1] + r / p * b[i, j - 1] * (1 - (b[i, j - 1] / K)^p) - h * b[i, j - 1]) * exp(e[i,j])  
            }
        }
        
        # mean equilibrium catch
        # over most recent 10%
        # of the projection period
        recent_time <- ceiling(0.9 * object@data$equ_time):object@data$equ_time
        catch <- mean(b[,recent_time] * h)
        
        # return catch with penalty if
        # harvest rate is greater than 
        # deterministic rate
        return(-1 * catch + max(x - log(r / (p + 1)), 0))
    }
    
    cli_progress_message("Compiling model...")
    
    # initialise with 
    # parameter value
    g <- MakeTape(obj_fun, object@pars$log_r@pars[1] - log(object@data$shape + 1))
    # function to 
    # estimate minimum
    # over first argument
    # (harvest rate)
    ff <- g$newton(1)

    msg <- ""
    cli_progress_step("Estimating the harvest rate at MNPL{msg}", spinner = TRUE, msg_done = "Estimated MNPL reference points")

    for (i in 1:object@iter) {
        
        # progress iteration
        msg <- glue(", iteration {i}/", object@iter)
        
        # spin spinner
        cli_progress_update()
        
        # sample
        perr <- matrix(rnorm(object@data$equ_time * object@data$equ_iter, 0 - (sigmap^2) / 2, sigmap), nrow = object@data$equ_iter, ncol = object@data$equ_time)
        pars <- unlist(lapply(object@pars, sample, n = 1))

        # minimise
        h_mnpl <- exp(ff(numeric()))
        
        # spin spinner
        cli_progress_update()
        
        # project under harvest rate
        # at MNPL
        # {{{
        b <- matrix(nrow = object@data$equ_iter, ncol = object@data$equ_time)
        
        # dynamics
        for (i in 1:object@data$equ_iter) {
            b[i,1] <- (exp(pars[1]) * (1 / (object@data$shape + 1))^(1 / object@data$shape)) * exp(perr[i,1])
            for (j in 2:object@data$equ_time) {
                b[i,j] <- (b[i, j - 1] + exp(pars[2]) / object@data$shape * b[i, j - 1] * (1 - (b[i, j - 1] / exp(pars[1]))^object@data$shape) - h_mnpl * b[i, j - 1]) * exp(perr[i,j])  
            }
        }
        #}}}
        
        # spin spinner
        cli_progress_update()
        
        # update targets
        recent_time <- ceiling(0.9 * object@data$equ_time):object@data$equ_time
        # (catch)
        object@targets$catch <- c(object@targets$catch, mean(b[,recent_time] * h_mnpl))
        # (depletion)
        object@targets$depletion <- c(object@targets$depletion, mean(b[,recent_time] / exp(pars[1]))) 
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
    object@targets$catch <- distribution(list(value = object@targets$catch, distribution = "lognormal"))
    # (depletion)
    object@targets$depletion <- distribution(list(value = object@targets$depletion, distribution = "lognormal"))
    # (harvest rate)
    object@targets$harvest_rate <- distribution(list(value = object@targets$harvest_rate, distribution = "lognormal"))
    
    # return
    return(object)
})
#}}}
