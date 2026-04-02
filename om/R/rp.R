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
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R dot-pdyn.R
#' @import RTMB
#' @import cli
#{{{ rp()
# wrapper for execution of function
setGeneric("rp", function(object, stochastic, equilibrium_time, iterations, ...) standardGeneric("rp"))
setMethod("rp", signature = "om", function(object, stochastic = FALSE, equilibrium_time = 200L, iterations = ifelse(stochastic, 300L, NA_integer_), ...) {
    
    # current environment
    ENV <- environment()
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    # get seeds
    get_seeds(object, env = ENV)
    
    # record stochasticity
    object@stochastic$ref_points <- as.logical(stochastic)
    
    # reset targets
    # (catch)
    object@targets$captures <- rep(NA_real_, object@iter)
    # (depletion)
    object@targets$depletion <- rep(NA_real_, object@iter)
    # (harvest rate)
    if (all(is.na(object@targets$harvest_rate))) {
        object@targets$harvest_rate <- rep(NA_real_, object@iter)
    } else {
        cli_alert_info("'object' already contains 'harvest_rate' reference point estimates (no estimation needed)")    
    }
    
    # settings
    if (missing(iterations)) {
        
        siter <- object@settings$stochastic_iterations
    
    } else {
        
        if (!is.na(object@settings$stochastic_iterations)) { if (iterations != object@settings$stochastic_iterations) {
            warning("'iterations' argument updates value in 'object@settings$stochastic_iterations'")
        }}
        
        siter <- object@settings$stochastic_iterations <- iterations
    }
    if (missing(equilibrium_time)) {
        
        equ_time <- object@settings$equilibrium_time
    
    } else {
        
        if (!is.na(object@settings$equilibrium_time)) { if (equilibrium_time != object@settings$equilibrium_time) {
            warning("'equilibrium_time' argument updates value in 'object@settings$equilibrium_time'")
        }}
        
        equ_time <- object@settings$equilibrium_time <- equilibrium_time
    }
    
    # checks
    if (is.na(siter) & stochastic) stop("process error 'iterations' argument required")
    if (is.na(equ_time))           stop("'equilibrium_time' argument required")
    
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
        
        # set up objective
        # function to estimate
        # harvest rate at 
        # maximum sustainable 
        # catch
        # (deterministic)
        obj1 <- function(x) {
            
            h     <- exp(x[1])
            shape <- exp(x[2])
            
            # spin spinner
            cli_progress_update(.envir = ENV)
            
            # deterministic dynamics
            n <- do.call(".pdyn", list(h = h, shape = shape, initial_depletion = 0.5, ntime = equ_time), envir = ENV)
            
            # objective function
            objective <- -1 * log(sum(n[, dim(n)[2]] * sel * h))
            
            # return
            return(objective)
        }
        
        if (stochastic) {
            
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
                
                h     <- exp(x[1])
                shape <- exp(x[2])
                
                objective <- 0
                
                for (i in 1:siter) {
                    
                    # spin spinner
                    cli_progress_update(.envir = ENV)
                    
                    # stochastic dynamics
                    n <- do.call(".pdyn2", list(h = h, shape = shape, error = perr[i,], initial_depletion = 0.5, ntime = equ_time), envir = ENV)
                    
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
        
        if (stochastic) {
            
            # log-normal process error term
            sigmap <- sqrt(log(1 + object@fixed$cv_dynamics^2))
            
            # sample process error
            perr <- matrix(rnorm(siter * equ_time, 0 - (sigmap^2) / 2, sigmap), nrow = siter, ncol = equ_time)
        }
        
        # check data present
        stopifnot(length(object@fixed) > 0)
        
        # setup (1)
        age_mat <- as.integer(pars_sample$a)
        age_pat <- age_mat + 1L
        age_sel <- as.integer(object@fixed$selectivity)
        
        # setup (2)
        mat    <- c(rep(0, age_mat), rep(1, nages - age_mat))
        pat    <- c(0, mat[-length(mat)])
        sel    <- c(rep(0, age_sel), rep(1, nages - age_sel))
        M      <- c(sqrt(pars_sample$M), rep(pars_sample$M, nages - 1))
        S      <- exp(-M)
        lambda <- exp(pars_sample$r)
        
        # progress message
        if (all(is.na(object@targets$harvest_rate))) {
            if (stochastic) {
                cli_progress_step("Estimating stochastic reference points ...", spinner = TRUE, msg_done = "Estimated stochastic reference points", .envir = ENV)
            } else {
                cli_progress_step("Estimating deterministic reference points ...", spinner = TRUE, msg_done = "Estimated deterministic reference points", .envir = ENV)
            }
        }
        
        # check shape exists
        stopifnot(length(object@shape) > 0)
        
        # re-estimate h_mnpl only
        # if necessary
        if (is.na(object@targets$harvest_rate[1])) {
            
            # function to estimate h_mnpl
            # given shape
            h1 <- MakeTape(obj1, c(log(0.02), log(1)))
            h2 <- h1$newton(1)
    
            # record initial 
            # deterministic estimates
            h_log_init <- h2(log(object@shape))
            
            if (stochastic) {
            
                h1 <- MakeTape(obj1, c(h_log_init, log(object@shape)))
                h2 <- h1$newton(1)
                
                # record estimate
                object@targets$harvest_rate[1] <- exp(h2(log(object@shape)))
            
            } else {
                
                object@targets$harvest_rate[1] <- exp(h_log_init)
            }
        }
        
        if (stochastic) {
        
            object@targets$captures[1]  <- .ff2(object@targets$harvest_rate[1], shape = object@shape, error = perr, equilibrium_time = equ_time, env = ENV)$captures
            object@targets$depletion[1] <- .ff2(object@targets$harvest_rate[1], shape = object@shape, error = perr, equilibrium_time = equ_time, env = ENV)$depletion
        
        } else {
            
            object@targets$captures[1]  <- .ff(object@targets$harvest_rate[1], shape = object@shape, equilibrium_time = equ_time, env = ENV)$captures
            object@targets$depletion[1] <- .ff(object@targets$harvest_rate[1], shape = object@shape, equilibrium_time = equ_time, env = ENV)$depletion    
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
                mat    <- c(rep(0, age_mat), rep(1, nages - age_mat))
                pat    <- c(0, mat[-length(mat)])
                sel    <- c(rep(0, age_sel), rep(1, nages - age_sel))
                M      <- c(sqrt(pars_sample$M), rep(pars_sample$M, nages - 1))
                S      <- exp(-M)
                lambda <- exp(pars_sample$r)
                
                # re-compile function to estimate h_mnpl
                # given shape
                #h1 <- MakeTape(obj1, c(log(0.02), log(1)))
                #h2 <- h1$newton(1)
                
                # record estimate if
                # necessary
                if (is.na(object@targets$harvest_rate[i])) {
                    object@targets$harvest_rate[i] <- exp(h2(log(object@shape)))
                }
                
                if (stochastic) {
                    
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
