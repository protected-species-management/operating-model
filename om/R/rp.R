#' @title Stochastic reference point calculation
#' @description 
#' Calculate the Maximum Net Productivity reference points.
#' @param object \code{om} class object
#' @param stochastic Logical indicating whether stochastic dynamics are assumed
#' @param equilibrium_time Time horizon used for projection to assumed equilibrium
#' @param iterations Process error iterations used for stochastic projection
#' @note This function would typically be preceded by a call to [shape()], which estimates the shape parameter necessary for definition of the production function. 
#' @seealso [targets()]
#' @export
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R dot-pdyn.R dot-check.R dot-logit.R
#' @import RTMB
#' @import cli
#{{{ rp()
# wrapper for execution of function
setGeneric("rp", function(object, stochastic, equilibrium_time, iterations, ...) standardGeneric("rp"))
setMethod("rp", signature = "om", function(object, stochastic, equilibrium_time, iterations, ...) {
    
    # current environment
    ENV <- environment()
    
    # check and update object with
    # function arguments
    object <- .check_rp(object, stochastic, equilibrium_time, iterations)
    
    # get values
    STOCHASTIC <- object@stochastic$ref_points
    EQU_TIME   <- object@settings$equilibrium_time
    SITER      <- object@settings$stochastic_iterations
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    # get seeds
    get_seeds(object, env = ENV)
    
    # reset targets
    # (catch)
    object@targets$captures <- rep(NA_real_, niter)
    # (depletion)
    object@targets$depletion <- rep(NA_real_, niter)
    # (harvest rate)
    if (all(is.na(object@targets$harvest_rate))) {
        object@targets$harvest_rate <- rep(NA_real_, niter)
    } else {
        cli_alert_info("'object' already contains 'harvest_rate' reference point estimates (no estimation needed)")    
    }
    
    # PT model
    # {{{
    if (all(is.na(object@ages)) | !(length(object@ages) > 1)) {
        
    # }}}
    } else {
    # AGE-STRUCTURED MODEL    
    # {{{
        
        # check environment for function call is
        # consistent with current environment
        environment(.pdyn) <- ENV
        environment(.ff)   <- ENV
        
        # accessor function
        #get_pars <- function() get("pars_sample", envir = ENV)
        
        # set up objective
        # function to estimate
        # harvest rate at 
        # maximum sustainable 
        # catch
        # (deterministic)
        obj1 <- function(x) {
            
            h     <- 1 / (1 + exp(-x[1]))
            shape <- exp(x[2])
            
            # pars <- DataEval(get_pars_sample)
            
            # spin spinner
            cli_progress_update(.envir = ENV)
            
            # deterministic dynamics
            n <- do.call(".pdyn", list(h = h, shape = shape, time = EQU_TIME), envir = ENV)
            
            # objective function
            objective <- -1 * log(sum(n[, dim(n)[2]] * sel * h))
            
            # return
            return(objective)
        }
        
        if (STOCHASTIC) {
            
            # check environment for function call is
            # consistent with current environment
            environment(.pdyn2) <- ENV
            environment(.ff2)   <- ENV
            
            # set up objective
            # function to estimate
            # harvest rate at 
            # maximum sustainable 
            # catch
            obj2 <- function(x) {
                
                h     <- 1 / (1 + exp(-x[1]))
                shape <- exp(x[2])
                
                objective <- 0
                
                for (i in 1:SITER) {
                    
                    # spin spinner
                    cli_progress_update(.envir = ENV)
                    
                    # stochastic dynamics
                    n <- do.call(".pdyn2", list(h = h, shape = shape, error = perr[i,], time = EQU_TIME), envir = ENV)
                    
                    # recent time
                    loc <- ceiling((2 / 3) * dim(n)[2]):dim(n)[2]
                    
                    # log of the equilibrium catch
                    # per iteration
                    objective <- objective - log(sum(sweep(n[, loc], 1, sel, "*") * h) / length(loc))
                }
                
                # return
                return(objective)
            }
        
        }
        
        ###################
        # first iteration #
        ###################
        
        # set seed
        set.seed(rng_seed[1])
        
        # sample pars
        pars_sample <- lapply(object@pars, sample, n = 1)
        
        if (STOCHASTIC) {
            
            # log-normal process error term
            sigmap <- sqrt(log(1 + object@fixed$cv_dynamics^2))
            
            # sample process error
            perr <- matrix(rnorm(SITER * EQU_TIME, 0 - (sigmap^2) / 2, sigmap), nrow = SITER, ncol = EQU_TIME)
        }
        
        # check data present
        stopifnot(length(object@fixed) > 0)
        
        # setup (1)
        age_mat <- as.integer(pars_sample$a)
        age_pat <- age_mat + 1L
        age_sel <- as.integer(object@fixed$selectivity)
        
        # setup (2)
        r <- pars_sample$r
        M <- pars_sample$M
        
        # setup (3)
        mat    <- c(rep(0, age_mat), rep(1, nages - age_mat))
        pat    <- c(rep(0, age_pat), rep(1, nages - age_pat))
        sel    <- c(rep(0, age_sel), rep(1, nages - age_sel))
        M      <- c(rep(sqrt(M), age_mat), rep(M, nages - age_mat))
        S      <- exp(-M)
        lambda <- exp(r)
        
        # progress message
        if (all(is.na(object@targets$harvest_rate))) {
            if (STOCHASTIC) {
                cli_progress_step("Estimating stochastic reference points ...", spinner = TRUE, msg_done = "Estimated stochastic reference points", .envir = ENV)
            } else {
                cli_progress_step("Estimating deterministic reference points ...", spinner = TRUE, msg_done = "Estimated deterministic reference points", .envir = ENV)
            }
        }
        
        # check shape exists
        stopifnot(length(object@shape) > 0)
        
        # estimate h_mnpl only
        # if not already estimated
        if (is.na(object@targets$harvest_rate[1])) {
            
            # function to estimate h_mnpl
            # given shape
            h1 <- MakeTape(obj1, c(.logit(0.02), log(object@shape)))
            h2 <- h1$newton(1)
    
            # record initial 
            # deterministic estimates
            h_logit_init <- h2(c(log(object@shape)))
            
            if (STOCHASTIC) {
                
                # function to estimate
                # stochastic h_mnpl
                h1 <- MakeTape(obj2, c(h_logit_init, log(object@shape)))
                h2 <- h1$newton(1)
                
                # record stochastic estimate
                object@targets$harvest_rate[1] <- .ilogit(h2(c(log(object@shape))))
            
            } else {
                
                # record deterministic estimate
                object@targets$harvest_rate[1] <- .ilogit(h_logit_init)
            }
        } 
        
        # calculate depletion and captures
        # at h_mnpl
        if (STOCHASTIC) {
            
            object@targets$captures[1]  <- .ff2(object@targets$harvest_rate[1], shape = object@shape, error = perr, equilibrium_time = EQU_TIME, env = ENV)$captures
            object@targets$depletion[1] <- .ff2(object@targets$harvest_rate[1], shape = object@shape, error = perr, equilibrium_time = EQU_TIME, env = ENV)$depletion
        
        } else {

            object@targets$captures[1]  <- .ff(object@targets$harvest_rate[1], shape = object@shape, equilibrium_time = EQU_TIME, env = ENV)$captures
            object@targets$depletion[1] <- .ff(object@targets$harvest_rate[1], shape = object@shape, equilibrium_time = EQU_TIME, env = ENV)$depletion    
        }
        
        #######################
        # monte-carlo samples #
        # from life-history   #
        # distributions       #
        #######################
        if (niter > 1) {
            for (i in 2:niter) {
                
                # set seed
                set.seed(rng_seed[i])
                
                # sample pars
                pars_sample <- lapply(object@pars, sample, n = 1)
                
                # setup (1)
                age_mat <- as.integer(pars_sample$a)
                age_pat <- age_mat + 1L
                age_sel <- as.integer(object@fixed$selectivity)
                
                # setup (2)
                r <- pars_sample$r
                M <- pars_sample$M
                
                # setup (3)
                mat    <- c(rep(0, age_mat), rep(1, nages - age_mat))
                pat    <- c(rep(0, age_pat), rep(1, nages - age_pat))
                sel    <- c(rep(0, age_sel), rep(1, nages - age_sel))
                M      <- c(rep(sqrt(M), age_mat), rep(M, nages - age_mat))
                S      <- exp(-M)
                lambda <- exp(r)
                
                # re-compile function to estimate h_mnpl
                # given shape
                #h1 <- MakeTape(obj1, c(log(0.02), log(1)))
                #h2 <- h1$newton(1)
                
                # record estimate if
                # necessary
                if (is.na(object@targets$harvest_rate[i])) {
                    object@targets$harvest_rate[i] <- .ilogit(h2(log(object@shape)))
                }
                
                if (STOCHASTIC) {
                    
                    object@targets$captures[i]  <- .ff2(object@targets$harvest_rate[i], shape = object@shape, error = perr, equilibrium_time = equ_time, env = ENV)$captures
                    object@targets$depletion[i] <- .ff2(object@targets$harvest_rate[i], shape = object@shape, error = perr, equilibrium_time = equ_time, env = ENV)$depletion
                    
                } else {
                    
                    object@targets$captures[i]  <- .ff(object@targets$harvest_rate[i], shape = object@shape, equilibrium_time = equ_time, env = ENV)$captures
                    object@targets$depletion[i] <- .ff(object@targets$harvest_rate[i], shape = object@shape, equilibrium_time = equ_time, env = ENV)$depletion    
                }
            }
        }
    # }}}
    }
    
    # return
    return(object)
})
#}}}


