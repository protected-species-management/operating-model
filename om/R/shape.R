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
#' @include om-class.R dot-pdyn.R
#' @import RTMB
#' @import cli
#{{{ shape()
# wrapper for execution of population
# dynamics function
# -- executes object@pdyn for each monte-carlo sample
setGeneric("shape", function(object, depletion, stochastic, equilibrium_time, iterations, ...) standardGeneric("shape"))
setMethod("shape", signature = c(object = "om", depletion = "numeric"), function(object, depletion, stochastic = FALSE, equilibrium_time = 200L, iterations = ifelse(stochastic, 300L, NA_integer_), ...) {
    
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
    
    # create container(s)
    shape_values <- numeric(object@iter)
    h_values     <- numeric(object@iter)
	
	# make sure dynamic
    # functions have correct
    # environment
	environment(.pdyn)  <- ENV
    environment(.pdyn2) <- ENV
    
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
    
	# set up objective
	# function and tape
	# for harvest rate at
	# maximum sustainable 
	# catch
	# (deterministic)
	obj1 <- function(x) {
		
		h     <- exp(x[1])
		shape <- exp(x[2])
		
		n <- do.call(".pdyn", list(h = h, shape = shape, initial_depletion = depletion, ntime = equ_time), envir = ENV)
		
		# objective function
		objective <- -1 * log(sum(n[, dim(n)[2]] * sel * h))
		
		# return lambda
		return(objective)
	}
	
	# set up objective
	# function and tape
	# for depletion at 
	# target 
	# (deterministic)
	obj2 <- function(x) {
		
		shape  <- exp(x[1])
		h      <- exp(h2(x[1])) # internal estimation of h_mnpl given shape
		target <- x[2]
		
		n <- do.call(".pdyn", list(h = h, shape = shape, initial_depletion = depletion, ntime = equ_time), envir = ENV)
		
		# objective function
		objective <- -1 * dnorm(sum(n[-1, dim(n)[2]]), depletion, 0.01, log = TRUE)
		
		# return objective
		return(objective)
	}
	
    if (stochastic) {
        
        # progress message
        cli_progress_step("Estimating the stochastic shape parameter ...", spinner = TRUE, msg_done = "Estimated shape = {round(object@shape, 2)}, with max. harvest rate = {round(mean(h_values), 2)}")
        
        # set up objective
        # function and tape
        # for harvest rate at
        # maximum sustainable 
        # catch
        obj3 <- function(x) {
            
            h     <- exp(x[1])
            shape <- exp(x[2])
            
            objective <- 0
            
            for (i in 1:siter) {
                
                # spin spinner
                cli_progress_update(.envir = ENV)
                
                # stochastic dynamics
                n <- do.call(".pdyn2", list(h = h, shape = shape, error = perr[i,], initial_depletion = depletion, ntime = equ_time), envir = ENV)
                
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
            h      <- exp(h2(x[1])) # internal estimation of h_mnpl given shape
            target <- x[2]
            
            objective <- 0
            
            for (i in 1:siter) {
                
                # spin spinner
                cli_progress_update(.envir = ENV)
                
                # stochastic dynamics
                n <- do.call(".pdyn2", list(h = h, shape = shape, error = perr[i,], initial_depletion = depletion, ntime = equ_time), envir = ENV)
                
                # recent time
                loc <- ceiling((2 / 3) * dim(n)[2]):dim(n)[2]
                
                # log of the equilibrium catch
                # per iteration
                objective <- objective - dnorm(mean(apply(n[-1, loc], 2, sum)), depletion, 0.01, log = TRUE)
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
    
    if (stochastic) {
        
        # log-normal process error term
        sigmap <- sqrt(log(1 + object@fixed$cv_dynamics^2))
        
        # sample process error
        perr <- matrix(rnorm(siter * equ_time, 0 - (sigmap^2) / 2, sigmap), nrow = siter, ncol = equ_time)
    }
    
    # check fixed inputs present
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
    
    # function to estimate h_mnpl
    # given shape
    h1 <- MakeTape(obj1, c(log(0.02), log(1)))
    h2 <- h1$newton(1)

    # function to estimate
    # shape given depletion target
    i1 <- MakeTape(obj2, c(log(1), 0.5))
    i2 <- i1$newton(1)
    
    # record initial 
	# deterministic estimates
	shape_log_init <- i2(depletion)
	h_log_init     <- h2(shape_log_init)
	
	if (stochastic) {
	
		# recompile with 
		# initial values
		h1 <- MakeTape(obj3, c(h_log_init, shape_log_init))
		h2 <- h1$newton(1)

		i1 <- MakeTape(obj4, c(shape_log_init, 0.5))
		i2 <- i1$newton(1)
		
		# record estimate
		shape_values[1] <- exp(i2(depletion))
		h_values[1]     <- exp(h2(log(shape_values[1])))
		
	} else {
	
		shape_values[1] <- exp(shape_log_init)
		h_values[1]     <- exp(h2(log(shape_values[1])))
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
            age_sel <- as.integer(object@data$selectivity)
            
            # setup (2)
            mat    <- c(rep(0, age_mat), rep(1, nages - age_mat))
            pat    <- c(0, mat[-length(mat)])
            sel    <- c(rep(0, age_sel), rep(1, nages - age_sel))
            M      <- c(sqrt(pars_sample$M), rep(pars_sample$M, nages - 1))
            S      <- exp(-M)
            lambda <- exp(pars_sample$r)
        
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
            h_values[i]     <- exp(h2(log(shape_values[i])))
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
