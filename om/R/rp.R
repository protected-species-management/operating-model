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
    
	.survivorship <- function(M, a, cv_survivorship = 0) {
		
		age_mat <- as.integer(a)
		mat     <- c(rep(0, age_mat), rep(1, NAGES - age_mat))
		S       <- exp(-c(rep(sqrt(M), age_mat), rep(M, NAGES - age_mat)))    
		
		if (cv_survivorship > 0) {
			
			# process error term
			sigma <- cv_survivorship * S
			
			# calculate mu given sigma
			mu_calc <- function(survivorship, sigma) uniroot(function(mu) survivorship - pnorm(mu / sqrt(1 + sigma^2)), interval = c(-10, 10))$root
			
			mu <- numeric(NAGES)
			for (a in 1:NAGES) {
				mu[a] <- mu_calc(S[a], sigma[a])
			}
			
			s <- array(dim = c(SITER, NAGES, NTIME))
			
			for (a in 1:NAGES) {
				
				e <- rnorm(SITER * NTIME, mu[a], sigma[a])
				
				s[,a,] <- pnorm(e)
				
				# first year is
				# equal to expectation
				s[,a,1] <- S[a]
			}
			
		} else {
		
			s <- array(dim = c(NAGES, NTIME))
			for (a in 1:NAGES) {
				s[a,] <- S[a]
			}    
		}
		
		return(s)
	}
	
    # current environment
    ENV <- environment()
    
    # check and update object with
    # function arguments
    object <- .check_rp(object, stochastic, equilibrium_time, iterations)
    
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
        #cli_alert_info("'object' already contains 'harvest_rate' reference point estimates (no estimation needed)")  
		ESTIMATE_HMNPL <- FALSE		
    }
    
    # PT model
    # {{{
    if (all(is.na(object@ages)) | !(length(object@ages) > 1)) {
        
    # }}}
    } else {
    # AGE-STRUCTURED MODEL    
    # {{{
                
		# accessor functions
        get_a <- function() get("a", envir = ENV)
        get_r <- function() get("r", envir = ENV)
        get_v <- function() get("v", envir = ENV)
        get_s <- function() get("s", envir = ENV)
		
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
			
			# get fixed values
            v <- get_v()
            
            # spin spinner
            #cli_progress_update(.envir = ENV)
            
            # deterministic dynamics
            n <- do.call(".pdyn", list(h = h, shape = shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r)))
            
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
				
				# get fixed values
				v <- get_v()
            
                objective <- 0
                
                for (i in 1:dim(s)[1]) {
                    
                    # spin spinner
                    cli_progress_update(.envir = ENV)
                    
                    # stochastic dynamics
                    n <- do.call(".pdyn2", list(h = h, shape = shape, survivorship = s[i,,], maturity = a, selectivity = v, lambda = exp(r)))
                    
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
        
        # check data present
        stopifnot(length(object@fixed) > 0)
        
        # setup (1)
        #age_mat <- as.integer(pars_sample$a)
        #age_pat <- age_mat + 1L
        #age_sel <- as.integer(object@fixed$selectivity)
        
        # setup (2)
        #r <- pars_sample$r
        #M <- pars_sample$M
        
        # setup (3)
        #mat    <- c(rep(0, age_mat), rep(1, NAGES - age_mat))
        #pat    <- c(rep(0, age_pat), rep(1, NAGES - age_pat))
        #sel    <- c(rep(0, age_sel), rep(1, NAGES - age_sel))
        #M      <- c(rep(sqrt(M), age_mat), rep(M, NAGES - age_mat))
        #S      <- exp(-M)
        #lambda <- exp(r)
		
		# assign pars
        a <- pars_sample$a
        r <- pars_sample$r
        M <- pars_sample$M
        v <- object@fixed$selectivity
        
        s <- .survivorship(M, a)
        
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
        
        # function to estimate h_mnpl
        # given shape
        h1 <- MakeTape(obj1, c(.logit(0.02), log(object@shape)))
        h2 <- h1$newton(1)
        
        # record initial 
        # deterministic estimates
        h_logit_init <- h2(c(log(object@shape)))
        
        if (STOCHASTIC) {
            
            s <- .survivorship(M, a, object@fixed$cv_survivorship)
            
            # estimate h_mnpl only
            # if not already estimated
            if (ESTIMATE_HMNPL) {
            
                # function to estimate
                # stochastic h_mnpl
                h1 <- MakeTape(obj2, c(h_logit_init, log(object@shape)))
                h2 <- h1$newton(1)
                
                # record stochastic estimate
                object@targets$harvest_rate[1] <- .ilogit(h2(c(log(object@shape))))
            }
            
            object@targets$captures[1]  <- .ff2(object@targets$harvest_rate[1], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$captures
            object@targets$depletion[1] <- .ff2(object@targets$harvest_rate[1], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$depletion
                
        } else {
            
            # record deterministic estimate only
            # if not already estimated
            if (ESTIMATE_HMNPL) {
                object@targets$harvest_rate[1] <- .ilogit(h_logit_init)
            }
        
            object@targets$captures[1]  <- .ff(object@targets$harvest_rate[1], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$captures
            object@targets$depletion[1] <- .ff(object@targets$harvest_rate[1], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$depletion  
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
                
                # setup (1)
                #age_mat <- as.integer(pars_sample$a)
                #age_pat <- age_mat + 1L
                #age_sel <- as.integer(object@fixed$selectivity)
                
                # setup (2)
                #r <- pars_sample$r
                #M <- pars_sample$M
                
                # setup (3)
                #mat    <- c(rep(0, age_mat), rep(1, NAGES - age_mat))
                #pat    <- c(rep(0, age_pat), rep(1, NAGES - age_pat))
                #sel    <- c(rep(0, age_sel), rep(1, NAGES - age_sel))
                #M      <- c(rep(sqrt(M), age_mat), rep(M, NAGES - age_mat))
                #S      <- exp(-M)
                #lambda <- exp(r)
				
				# assign pars
				#a <- pars_sample$a
				r <- pars_sample$r
				#M <- pars_sample$M
				#v <- object@fixed$selectivity
				
				#if (STOCHASTIC) {
				#
				#	s <- .survivorship(M, a, object@fixed$cv_survivorship)
				#} else {
				#
				#	s <- .survivorship(M, a)
				#}
                
                # re-compile function to estimate h_mnpl
                # given shape
                #h1 <- MakeTape(obj1, c(log(0.02), log(1)))
				#h2 <- h1$newton(1)
                
                # record estimate if
                # necessary
                if (ESTIMATE_HMNPL) {
					h2$force.update()
                    object@targets$harvest_rate[i] <- .ilogit(h2(log(object@shape)))
                }
                
                if (STOCHASTIC) {
                    
                    object@targets$captures[i]  <- .ff2(object@targets$harvest_rate[i], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$captures
                    object@targets$depletion[i] <- .ff2(object@targets$harvest_rate[i], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$depletion
                    
                } else {
                    
                    object@targets$captures[i]  <- .ff(object@targets$harvest_rate[i], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$captures
                    object@targets$depletion[i] <- .ff(object@targets$harvest_rate[i], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r))$depletion    
                }
            }
        }
    # }}}
    }
    
    # return
    return(object)
})
#}}}


