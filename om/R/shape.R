#' @title Calculate shape
#' 
#' @description Calculates shape parameter given assumed depletion at MNPL.
#' @param object \code{om} class object
#' @param depletion Assumed 1+ depletion at MNPL
#' @param stochastic Logical indicating whether stochastic dynamics are assumed
#' @param equilibrium_time Time horizon used for projection to assumed equilibrium
#' @param iterations Process error iterations used for stochastic projection
#' @seealso [rp()]
#' @export
#' @include om-class.R dot-pdyn.R dot-check.R dot-logit.R
#' @import RTMB
#' @import cli
#{{{ shape()
# wrapper for execution of population
# dynamics function
# -- executes object@pdyn for each monte-carlo sample
setGeneric("shape", function(object, depletion, stochastic, equilibrium_time, iterations, ...) standardGeneric("shape"))
setMethod("shape", signature = c(object = "om", depletion = "numeric"), function(object, depletion, stochastic, equilibrium_time, iterations, ...) {
    
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
    
    # create container(s)
    shape_values <- numeric(NITER)
    h_values     <- numeric(NITER)
	
	# accessor functions
	get_a <- function() get("a", envir = ENV)
	get_r <- function() get("r", envir = ENV)
	get_v <- function() get("v", envir = ENV)
	get_s <- function() get("s", envir = ENV)
    
	# set up objective
	# function
	# for harvest rate at
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
			
		# deterministic dynamics
        n <- do.call(".pdyn", list(h = h, shape = shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r)))
		
		# objective function
		objective <- -1 * log(sum(n[(v + 1):dim(n)[1], dim(n)[2]] * h))
		
		# return lambda
		return(objective)
	}
	
	# set up objective
	# function
	# for depletion at 
	# target 
	# (deterministic)
	obj2 <- function(x) {
		
		shape  <- exp(x[1])
		h      <- 1 / (1 + exp(-h2(x[1]))) # internal estimation of h_mnpl given shape
		target <- x[2]
		
		# get pars
		a <- DataEval(get_a)
		r <- DataEval(get_r)
		s <- DataEval(get_s)
		
		# get fixed values
		v <- get_v()
			
		# deterministic dynamics
        n <- do.call(".pdyn", list(h = h, shape = shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r)))
		
		# objective function
		objective <- -1 * dnorm(sum(n[-1, dim(n)[2]]), target, 0.01, log = TRUE)
		
		# return objective
		return(objective)
	}
	
    if (STOCHASTIC) {
        
        # progress message
        cli_progress_step("Estimating the stochastic shape parameter ...", spinner = TRUE, msg_done = "Estimated shape = {round(object@shape, 2)}, with max. harvest rate = {round(mean(h_values), 2)}")
        
        # set up objective
        # function and tape
        # for harvest rate at
        # maximum sustainable 
        # catch
        obj3 <- function(x) {
            
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
        
        # set up objective
        # function and tape
        # for depletion at 
        # target 
        obj4 <- function(x) {
            
            shape  <- exp(x[1])
            h      <- 1 / (1 + exp(-h2(x[1]))) # internal estimation of h_mnpl given shape
            target <- x[2]
            
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
                objective <- objective - dnorm(mean(apply(n[-1, loc], 2, sum)), target, 0.01, log = TRUE)
            }
            
            # return objective
            return(objective)
        }
        
    } else {
        
        # progress message
        cli_progress_step("Estimating the deterministic shape parameter ...", spinner = FALSE, msg_done = "Estimated shape = {round(object@shape, 2)}, with max. harvest rate = {round(mean(h_values), 2)}")
    }
    
    ###################
    # first iteration #
    ###################
    
    # set seed
    set.seed(rng_seed[1])
    
    # sample pars
    pars_sample <- lapply(object@pars, sample, n = 1)
    
    # check fixed inputs present
    stopifnot(length(object@fixed) > 0)
    
    # assign pars
	a <- pars_sample$a
	r <- pars_sample$r
	M <- pars_sample$M
	v <- object@fixed$selectivity

    s <- .survivorship(M, a)
    
    # function to estimate h_mnpl
    # given shape
    h1 <- MakeTape(obj1, c(.logit(0.02), log(1)))
    h2 <- h1$newton(1)

    # function to estimate
    # shape given depletion target
    i1 <- MakeTape(obj2, c(log(1), 0.5))
    i2 <- i1$newton(1)
    
    # record initial 
	# deterministic estimates
	shape_log_init <- i2(depletion)
	h_logit_init   <- h2(shape_log_init)
	
	if (STOCHASTIC) {
	
	    # 
	    s <- .survivorship(M, a, object@fixed$cv_survivorship)
	    
		# recompile with 
		# initial values
		h1 <- MakeTape(obj3, c(h_logit_init, shape_log_init))
		h2 <- h1$newton(1)

		i1 <- MakeTape(obj4, c(shape_log_init, depletion))
		i2 <- i1$newton(1)
		
		# record estimate
		shape_values[1] <- exp(i2(depletion))
		h_values[1]     <- .ilogit(h2(log(shape_values[1])))
		
	} else {
	
		shape_values[1] <- exp(shape_log_init)
		h_values[1]     <- .ilogit(h2(log(shape_values[1])))
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
			v <- object@fixed$selectivity

			#if (STOCHASTIC) {
            # 
			#	s <- .survivorship(M, a, object@fixed$cv_survivorship)
			#	
			#	# function to estimate h_mnpl
			#	# given shape
			#	h1 <- MakeTape(obj3, c(h_logit_init, shape_log_init))
			#	h2 <- h1$newton(1)
			#	
			#	# function to estimate
			#	# shape given target
			#	i1 <- MakeTape(obj4, c(shape_log_init, depletion))
			#	i2 <- i1$newton(1)
			#
			#} else {
			#
			#	s <- .survivorship(M, a)
			#}
			
			h2$force.update()
			i2$force.update()
            
            # record estimate
            shape_values[i] <- exp(i2(depletion))
            h_values[i]     <- .ilogit(h2(log(shape_values[i])))
        }
    }
    
    # average across
    # samples
    object@shape <- mean(shape_values)
    
    # record harvest rates
    object@targets$harvest_rate <- h_values
    
    # return
    return(object)
})
# accessor function
#' @rdname shape
setMethod("shape", signature = c(object = "om"), function(object, ...) {
    object@shape
})
# assignment function
#' @rdname shape
#' @export
setGeneric("shape<-", function(object, ..., value) standardGeneric("shape<-"))
#' @rdname shape
setMethod("shape<-",
          signature(object = "om", value = "numeric"),
          function(object, value) {
              
              if (value <= 0) {
                  stop('Assigned value must be >0')
              }

              object@shape <- value  
              
              return(object)
          }
)
#}}
