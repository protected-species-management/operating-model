#' @title Stochastic reference point calculation
#' @description 
#' Calculate the Maximum Net Productivity reference points.
#' @param object \code{om} class object
#' @param stochastic Logical indicating whether stochastic dynamics are assumed
#' @param time Time horizon used for projection to assumed equilibrium
#' @param iterations Process error iterations used for stochastic projection
#' @note This function would typically be preceded by a call to [shape()], which estimates the shape parameter necessary for definition of the production function. 
#' @seealso [targets()]
#' @export
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R dot-pdyn.R dot-check.R dot-logit.R dot-survivorship.R
#' @import RTMB
#' @import cli
#{{{ rp()
# wrapper for execution of function
setGeneric("rp", function(object, ...) standardGeneric("rp"))
setMethod("rp", signature = "om", function(object, stochastic, time, iterations, verbose = TRUE, ...) {
    
    # current environment
    ENV <- environment()
    
    # check and update object with
    # function arguments
    object <- .check_rp(object, stochastic, time, iterations, verbose)
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, ref_points = TRUE, env = ENV)
    
    # get seeds
    get_seeds(object, env = ENV)
    
    # reset targets
    # (catch)
    object@targets$captures <- rep(NA_real_, NITER)
    # (depletion)
    object@targets$depletion <- rep(NA_real_, NITER)
    # (harvest rate)
    if (any(is.na(object@targets$harvest_rate))) {
        object@targets$harvest_rate <- rep(NA_real_, NITER)
		ESTIMATE_HMNPL <- TRUE
    } else { 
		ESTIMATE_HMNPL <- FALSE		
    }
    
    # PT model
    # {{{
    if (all(is.na(object@ages)) | !(length(object@ages) > 1)) {
        
    # }}}
    } else {
    # AGE-STRUCTURED MODEL    
    # {{{
        
		# function to extract real values
		# from advector-type
		getValues <- function(x) {
			.Call("_RTMB_getValues", x, PACKAGE = "RTMB")
		}

		# accessor functions
        get_a <- function() get("a", envir = ENV)
        get_r <- function() get("r", envir = ENV)
        get_s <- function() get("s", envir = ENV)
		get_e <- function() get("e", envir = ENV)
		
        # set up objective
        # function to estimate
        # harvest rate at 
        # maximum sustainable 
        # catch
        # (deterministic)
        obj1 <- function(x) {
            
            h     <- 1 / (1 + exp(-x[1]))
            shape <- exp(x[2])
            
            # get pars
            a <- DataEval(get_a)
            r <- DataEval(get_r)
            s <- DataEval(get_s)
			e <- DataEval(get_e)
			
			# get selectivity
			a <- as.integer(getValues(a))
			v <- a + 1L
			
            # spin spinner
            #cli_progress_update(.envir = ENV)
            
            # deterministic dynamics
            n <- do.call(".pdyn", list(h = h, shape = shape, survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV))
            
            # objective function
            objective <- -1 * log(sum(n[(v + 1):dim(n)[1], dim(n)[2]] * h))
            
            # return
            return(objective)
        }
		
		
        
        if (STOCHASTIC) {
            
            # set up objective
            # function to estimate
            # harvest rate at 
            # maximum sustainable 
            # catch
            obj2 <- function(x) {
                
                h     <- 1 / (1 + exp(-x[1]))
                shape <- exp(x[2])
                
				# get pars
				a <- DataEval(get_a)
				r <- DataEval(get_r)
				s <- DataEval(get_s)
				e <- DataEval(get_e)				
				
				# get selectivity
				a <- as.integer(getValues(a))
				v <- a + 1L
            
                objective <- 0
                
                for (i in 1:dim(s)[1]) {
                    
                    # spin spinner
                    cli_progress_update(.envir = ENV)
                    
                    # stochastic dynamics
                    n <- do.call(".pdyn2", list(h = h, shape = shape, survivorship = s[i,], epsilon = e[i,], maturity = a, selectivity = v, lambda = exp(r), env = ENV))
                    
                    # recent time
                    loc <- ceiling((2 / 3) * dim(n)[2]):dim(n)[2]
                    
                    # log of the equilibrium catch
                    # per iteration
                    objective <- objective - log(sum(n[(v + 1):dim(n)[1], loc] * h) / length(loc))
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
		
		# assign pars
        a <- pars_sample$a
        r <- pars_sample$r
        M <- pars_sample$M
        v <- a + 1L
        s <- .survivorship(M, env = ENV)
		e <- .epsilon(env = ENV)
        
        # progress message
        if (ESTIMATE_HMNPL) {
            if (STOCHASTIC) {
                cli_progress_step("Estimating stochastic reference points ...", spinner = TRUE, msg_done = "Estimated stochastic reference points", .envir = ENV)
            } else {
                cli_progress_step("Estimating deterministic reference points ...", spinner = FALSE, msg_done = "Estimated deterministic reference points", .envir = ENV)
            }
        }
        
        # check shape exists
        stopifnot(length(object@shape) > 0)
        
		if (ESTIMATE_HMNPL) {
		
			# function to estimate h_mnpl
			# given shape
			h1 <- MakeTape(obj1, c(.logit(0.03), log(object@shape[1])))
			h2 <- h1$newton(1)
			
			# record initial 
			# deterministic estimates
			h_logit_init <- h2(c(log(object@shape[1])))
        }
		
        if (STOCHASTIC) {
            
            s <- .survivorship(M, object@settings$cv$survivorship, env = ENV)
			e <- .epsilon(object@settings$cv$birth, env = ENV)
            
            # estimate h_mnpl only
            # if not already estimated
            if (ESTIMATE_HMNPL) {
				
				# tidy up
				rm(obj1, h1, h2)
                
				# function to estimate
                # stochastic h_mnpl
                h1 <- MakeTape(obj2, c(h_logit_init, log(object@shape[1])))
                h2 <- h1$newton(1)
                
                # record stochastic estimate
                object@targets$harvest_rate[1] <- .ilogit(h2(c(log(object@shape[1]))))
            }
            
            object@targets$captures[1]  <- .ff2(object@targets$harvest_rate[1], shape = object@shape[1], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$captures
            object@targets$depletion[1] <- .ff2(object@targets$harvest_rate[1], shape = object@shape[1], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$depletion
                
        } else {
            
            # record deterministic estimate only
            # if not already estimated
            if (ESTIMATE_HMNPL) {
                object@targets$harvest_rate[1] <- .ilogit(h_logit_init)
            }
        
            object@targets$captures[1]  <- .ff(object@targets$harvest_rate[1], shape = object@shape[1], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$captures
            object@targets$depletion[1] <- .ff(object@targets$harvest_rate[1], shape = object@shape[1], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$depletion  
        }
        
        #######################
        # monte-carlo samples #
        # from life-history   #
        # distributions       #
        #######################
        if (NITER > 1) {
            for (i in 2:NITER) {
                
                # set seed
                set.seed(rng_seed[i])
                
                # sample pars
                pars_sample <- lapply(object@pars, sample, n = 1)
				
				# assign pars
				a <- pars_sample$a
				r <- pars_sample$r
				M <- pars_sample$M
				v <- a + 1L
				            
				s <- .survivorship(M, ifelse(STOCHASTIC, object@settings$cv$survivorship, 0), env = ENV)
				e <- .epsilon(ifelse(STOCHASTIC, object@settings$cv$birth, 0), env = ENV)
				
                # record estimate if
                # necessary
                if (ESTIMATE_HMNPL) {
					h2$force.update()
                    object@targets$harvest_rate[i] <- .ilogit(h2(log(object@shape[i])))
                }
                
                if (STOCHASTIC) {
                    
                    object@targets$captures[i]  <- .ff2(object@targets$harvest_rate[i], shape = object@shape[i], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$captures
                    object@targets$depletion[i] <- .ff2(object@targets$harvest_rate[i], shape = object@shape[i], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$depletion
                    
                } else {
                    
                    object@targets$captures[i]  <- .ff(object@targets$harvest_rate[i], shape = object@shape[i], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$captures
                    object@targets$depletion[i] <- .ff(object@targets$harvest_rate[i], shape = object@shape[i], survivorship = s, epsilon = e, maturity = a, selectivity = v, lambda = exp(r), env = ENV)$depletion    
                }
            }
        }
    # }}}
    }
    
    # return
    return(object)
})
#}}}


