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
    
    # create container(s)
    shape_values <- numeric(niter)
    h_values     <- numeric(niter)
	
	# make sure dynamic
    # functions have correct
    # environment
	environment(.pdyn)  <- ENV
    environment(.pdyn2) <- ENV
    
	# set up objective
	# function
	# for harvest rate at
	# maximum sustainable 
	# catch
	# (deterministic)
	obj1 <- function(x) {
		
		h     <- 1 / (1 + exp(-x[1]))
		shape <- exp(x[2])
		
		n <- do.call(".pdyn", list(h = h, shape = shape, time = EQU_TIME), envir = ENV)
		
		# objective function
		objective <- -1 * log(sum(n[, dim(n)[2]] * sel * h))
		
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
		
		n <- do.call(".pdyn", list(h = h, shape = shape, time = EQU_TIME), envir = ENV)
		
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
        
        # set up objective
        # function and tape
        # for depletion at 
        # target 
        obj4 <- function(x) {
            
            shape  <- exp(x[1])
            h      <- 1 / (1 + exp(-h2(x[1]))) # internal estimation of h_mnpl given shape
            target <- x[2]
            
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
    
    if (STOCHASTIC) {
        
        # log-normal process error term
        sigmap <- sqrt(log(1 + object@fixed$cv_dynamics^2))
        
        # sample process error
        perr <- matrix(rnorm(SITER * EQU_TIME, 0 - (sigmap^2) / 2, sigmap), nrow = SITER, ncol = EQU_TIME)
    }
    
    # check fixed inputs present
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
        
            # function to estimate h_mnpl
            # given shape
            #h1 <- MakeTape(obj1, c(log(0.02), log(1)))
            #h2 <- h1$newton(1)
            
            # function to estimate
            # shape given target
            #i1 <- MakeTape(obj2, c(log(1), 0.5))
            #i2 <- i1$newton(1)
            
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
