#' @title Population dynamics function
#' 
#' @description The population dynamics function is called per-iteration.
#' 
#' @export
#' @include om-class.R get_dim.R dot-survivorship.R
#' @import RTMB
#' @import cli
#' @import glue
#{{{ pdyn()
setGeneric("pdyn", function(object, ...) standardGeneric("pdyn"))
setMethod("pdyn", signature = "om", function(object, stochastic, iterations, time, initial_depletion = 1.0, verbose = FALSE, ...) {
    
    # current environment
    ENV <- environment()
    
    # make sure harvest rate
    # function has correct
    # environment
    environment(object@harvest_rate) <- ENV
    
    # check and update object with
    # function arguments
    object <- .check_pdyn(object, stochastic, time, iterations, verbose)
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, projection = TRUE, env = ENV)
    
    # get seeds
    get_seeds(object, env = ENV)
    
    # get shape
    get_shape(object, env = ENV)
    
    # check pars
    for (a in names(object@pars)) {
		if (isTRUE(is.na(object@pars[[a]]))) {
			stop("'", a, "' is missing from 'object@pars'")
		}
	}
	
    # setup numbers array
    # [life-history samples, process-error samples, ages, time]
    if (all(is.na(ages))) {
        N <- array(dim = c(NITER, SITER, 1, NTIME))
    } else {
        N <- array(dim = c(NITER, SITER, NAGES, NTIME))
    }
    
    # setup diagnostics
    # (catch)
    object@diagnostics$captures <- array(dim = c(NITER, SITER, NTIME - 1))
    # (depletion)
    object@diagnostics$depletion <- array(dim = c(NITER, SITER, NTIME)) 
    # (harvest rate)
    object@diagnostics$harvest_rate <- array(dim = c(NITER, SITER, NTIME - 1))
    
    # pst
    object@pst$value <- array(dim = c(NITER, SITER, NTIME))
        
	# define observation error function
	# using: cv, quantile (qn) and/or bias
	if (object@settings$cv$observation > 0) {
		if (object@settings$qn$observation[1] > 0) {
		    if (is.na(object@settings$qn$observation[2])) {
		        object@settings$qn$observation[2] <- object@settings$cv$observation
		    }
			if (object@settings$bias$observation != 1.0) {
				.obs_error <- function(a, cv = object@settings$cv$observation, qn = object@settings$qn$observation, bias = object@settings$bias$observation) {
					bias * exp(log(a / sqrt(1 + cv^2)) + rnorm(1) * sqrt(log(1 + cv^2))) / exp(abs(qnorm(qn[1])) * sqrt(log(1 + (qn[2])^2)))
				}
			} else {
				.obs_error <- function(a, cv = object@settings$cv$observation, qn = object@settings$qn$observation) {
					exp(log(a / sqrt(1 + cv^2)) + rnorm(1) * sqrt(log(1 + cv^2))) / exp(abs(qnorm(qn[1])) * sqrt(log(1 + (qn[2])^2)))
				}
			}
		} else {
			if (object@settings$bias$observation != 1.0) {
				.obs_error <- function(a, cv = object@settings$cv$observation, bias = object@settings$bias$observation) {
					bias * exp(log(a / sqrt(1 + cv^2)) + rnorm(1) * sqrt(log(1 + cv^2)))
				}
			} else {
				.obs_error <- function(a, cv = object@settings$cv$observation) {
					exp(log(a / sqrt(1 + cv^2)) + rnorm(1) * sqrt(log(1 + cv^2)))
				}
			}
		}
	} else {
		if (object@settings$bias$observation != 1.0) {
			.obs_error <- function(a, bias = object@settings$bias$observation) {
				bias * a
			}
		} else {
			.obs_error <- function(a) {
				a
			}
		}
	}
	
	# harvest rate error function
	if (object@settings$cv$mortality > 0) {
		if (object@settings$bias$mortality != 1.0) {
			.harvest_error <- function(a, cv = object@settings$cv$mortality, bias = object@settings$bias$mortality) {
				sigma <- cv * a
				mu    <- uniroot(function(x) a - pnorm(x / sqrt(1 + sigma^2)), interval = c(-10, 10))$root
				e     <- rnorm(1, mu, sigma)
				bias * pnorm(e)
			}
		} else {
			.harvest_error <- function(a, cv = object@settings$cv$mortality) {
				sigma <- cv * a
				mu    <- uniroot(function(x) a - pnorm(x / sqrt(1 + sigma^2)), interval = c(-10, 10))$root
				e     <- rnorm(1, mu, sigma)
				pnorm(e)
			}
		}
	} else {
		if (object@settings$bias$mortality != 1.0) {
			.harvest_error <- function(a, bias = object@settings$bias$mortality) {
				bias * a
			}
		} else {
			.harvest_error <- function(a) {
				a
			}
		}
	}
    
    if (verbose) {
        message("harvest rate function:")
        message(writeLines(deparse(object@harvest_rate)))
        message("captures error function:")
        message(writeLines(deparse(.harvest_error)))
        message("observation error function:")
        message(writeLines(deparse(.obs_error)))
    }
	
    # progress
    msg <- ""
    cli_progress_step("Projecting dynamics{msg}", spinner = TRUE, msg_done = "Projected dynamics")

    # {{{
    # PT model
    if (all(is.na(object@ages)) | !(length(object@ages) > 1)) {
        
    } else {
    # {{{
    # AGE-STRUCTURED MODEL
        
        # objective function for
        # estimation of h at
        # initial depletion
        obj_fun <- function(x, shape, target) {
            
            h <- 1 / (1 + exp(-x[1]))
            n <- matrix(k, nrow = NAGES, ncol = 2)
            
            # equilibrium age
            # structure
            for (l in 2:1e3) {
                
                n[, 1] <- n[, 2]
                for(a in 2:NAGES) {
                    n[a, 2] <- n[a - 1, 1] * S[a - 1] * (1 - sel[a - 1] * h)
                }
                n[a, 2] <- n[a, 2] + n[a, 1] * S[a] * (1 -  sel[a] * h)
                n[1, 2] <- 0.5 * sum(pat[-1] * n[-1, 2]) * (b_eq + (b_max - b_eq) * (1 - (sum(n[-1, 2] * pat[-1]) / sum(k[-1] * pat[-1]))^shape))
            }
                
            # log of the equilibrium depletion
            objective <- -1 * dnorm(sum(n[-1, 2] * pat[-1]) / sum(k[-1] * pat[-1]), target, 0.01, log = TRUE)
            
            # return
            return(objective)
        }
        
        # set-up arrays
        n   <- array(dim = c(NAGES, NTIME))
        p   <- vector("numeric", length = NAGES)
		pst <- vector("numeric", length = NTIME)
		h   <- vector("numeric", length = NTIME - 1)
        
        proj_n         <- array(dim = c(SITER, NAGES, NTIME))
        proj_h         <- array(dim = c(SITER, NTIME - 1))
        proj_catch     <- array(dim = c(SITER, NTIME - 1))
        proj_depletion <- array(dim = c(SITER, NTIME))
		proj_pst       <- array(dim = c(SITER, NTIME))
        
        # set-up birth function
        birth <- function(y) {
            0.5 * sum(pat[-1] * n[-1,y]) * (b_eq + (b_max - b_eq) * (1 - (sum(n[-1,y] * pat[-1]) / sum(k[-1] * pat[-1]))^shape[i])) 
        }
        
		# pst observation function
		pst_calc <- function(numbers) {
			(1 / 2) * object@pst$phi * sample(object@pst$rmax) * .obs_error(sum(numbers * object@pst$ogive))
		}
	
        #######################
        # monte-carlo samples #
        # from life-history   #
        # distributions       #
        #######################
        for (i in 1:NITER) {
            
            # set seed
            set.seed(rng_seed[i])
            
            # progress iteration
            msg <- ifelse(NITER > 1, glue(", sample {i}/", NITER), " ...")
            
            # sample
            pars_sample <- lapply(object@pars, sample, n = 1)
            rmax_sample <- sample(object@pst$rmax, n = 1)
                
            # spin spinner
            cli_progress_update()
            
            # assign pars
			a <- pars_sample$a
			r <- pars_sample$r
			M <- pars_sample$M
			v <- pars_sample$v
			o <- pars_sample$o
            K <- pars_sample$K
            
            # transcribe
			S <- c(rep((exp(-M)^2), a), rep(exp(-M), NAGES - a))
			
			age_mat <- as.integer(a)
			age_pat <- age_mat + 1L
			age_sel <- as.integer(v)
			age_obs <- as.integer(o)
		
			mat <- c(rep(0, age_mat), rep(1, NAGES - age_mat))
			pat <- c(rep(0, age_pat), rep(1, NAGES - age_pat))
			sel <- c(rep(0, age_sel), rep(1, NAGES - age_sel))
			obs <- c(rep(0, age_obs), rep(1, NAGES - age_obs))
			
			lambda <- exp(r)
			
			object@pst$ogive <- obs
            
            # set up unexploited 
            # equilibrium female
            # population
            p[1] <- 0.5
            for(a in 2:NAGES) {
                p[a] <- p[a - 1] * S[a - 1]
            }
            p[a] <- p[a] / (1 - S[a])
            
            # replacement birth rate
            # per female
            b_eq  <- 1 / sum(pat * p)
            
            # maximum birth rate
            # per female
            b_max <- 2 * (lambda^(age_mat + 1) - S[age_mat + 1] * lambda^(age_mat)) / prod(S[1:(age_mat + 1)])
            
            # initialise population
            # at equilibrium
            k_prime <- b_eq * p
            
            # initial conditions
            # (breeding+ depletion = K)
            k <- K * k_prime / sum(k_prime[-1] * pat[-1])
            
            # initial conditions
            if (initial_depletion < 1) {
                h_init <- .ilogit(optimise(obj_fun, interval = c(-10,-1), shape = shape[i], target = initial_depletion)$minimum)
            } else {
                h_init <- 0    
            }
            
            # equilibrium age
            # structure
            n_init <- matrix(k, nrow = NAGES, ncol = 2)
            for (l in 2:1e3) {
                
                n_init[, 1] <- n_init[, 2]
                for(a in 2:NAGES) {
                    n_init[a, 2] <- n_init[a - 1, 1] * S[a - 1] * (1 - sel[a - 1] * h_init)
                }
                n_init[a, 2] <- n_init[a, 2] + n_init[a, 1] * S[a] * (1 -  sel[a] * h_init)
                n_init[1, 2] <- 0.5 * sum(pat[-1] * n_init[-1, 2]) * (b_eq + (b_max - b_eq) * (1 - (sum(n_init[-1, 2] * pat[-1]) / sum(k[-1] * pat[-1]))^shape[i]))
            }
			
			# construct survivorship
			# array
            if (STOCHASTIC) {
				survivorship <- .survivorship(M, object@settings$cv$survivorship, env = ENV)
				epsilon      <- .epsilon(object@settings$cv$birth, env = ENV)
			} else {
				survivorship <- .survivorship(M, env = ENV)
				survivorship <- matrix(survivorship, nrow = SITER, ncol = NTIME, byrow = TRUE)
				epsilon      <- .epsilon(env = ENV)
				epsilon      <- matrix(epsilon, nrow = SITER, ncol = NTIME, byrow = TRUE)
			}
			
            # loop over stochastic
            # process error
            for (j in 1:SITER) {
                
                # initialise
                n[, 1] <- n_init[,2]
                
				# survivorship matrix
				s <- matrix(survivorship[j,], ncol = NTIME, nrow = NAGES, byrow = TRUE)
				s <- (sweep(s, 1, 1 - mat, "*")^2) + sweep(s, 1, mat, "*")
				
				# birth rate deviation
				e <- epsilon[j,]
				
				# apply
				n[, 1] <- n[, 1] * e[1]
				
				# observe initial pst
				pst[1] <- pst_calc(n[, 1])
					
                # project under harvest rate
                # function
                # {{{
                for (y in 2:NTIME) {
                    
					# calculate harvest rate
					h[y - 1] <- object@harvest_rate(numbers = n[, y - 1], selectivity = sel, pst = pst[y - 1], i, y)
					
					# apply harvest rate
					# and mortality
                    for (a in 2:NAGES) {
                        n[a, y] <- n[a - 1, y - 1] * s[a - 1, y - 1] * (1 - sel[a - 1] * h[y - 1]) 
                    }
                    
                    # plus group
                    n[a, y] <- n[a, y] + n[a, y - 1] * s[a, y - 1] * (1 - sel[a] * h[y - 1])
                    
                    # birth
                    n[1, y] <- birth(y) * e[y]
					
					# observe pst
					pst[y] <- pst_calc(n[, y])
                }
                
                # values per-year
				proj_h[j,]         <- h
                proj_catch[j,]     <- apply(sweep(n, 1, sel, "*"), 2, sum)[-NTIME] * h
                proj_depletion[j,] <- apply(sweep(n[-1,], 1, pat[-1], "*"), 2, sum) / sum(k[-1] * pat[-1])
                proj_n[j,,]        <- n
				proj_pst[j,]       <- pst
                
                # spin spinner
                cli_progress_update()
            }
            
            # update time series diagnostics
            # (catch)
            object@diagnostics$captures[i,,]     <- proj_catch
            # (depletion)
            object@diagnostics$depletion[i,,]    <- proj_depletion
            # (harvest rate)
            object@diagnostics$harvest_rate[i,,] <- proj_h
            
            # numbers
            N[i,,,] <- proj_n
            
			# pst
            object@pst$value[i,,] <- proj_pst
			
            # spin spinner
            cli_progress_update()
            
            # record values
            object@values$r[i] <- pars_sample$r
            object@values$M[i] <- pars_sample$M
			object@values$s[i] <- exp(-pars_sample$M)
            object@values$b[i] <- b_max
			object@values$A[i] <- (b_max - b_eq) / b_eq
            object@values$m[i] <- pars_sample$a
            object@values$o[i] <- pars_sample$o
            object@values$u[i] <- pars_sample$v
            object@values$K[i] <- pars_sample$K
        }
    }

    # calculate objectives as the probability
    # of a desirable outcome
    p_higher <- function(x, y) ifelse(x > y, 1, ifelse(x < y, 0, 0.5))
    p_lower  <- function(x, y) ifelse(x < y, 1, ifelse(x > y, 0, 0.5))
    
    # (prob. that catch is less than that required to meet MNPL)
    object@objectives$captures     <- array(dim = c(NITER, NTIME - 1))
    # (prob. that depletion is greater than the depletion at MNPL)
    object@objectives$depletion    <- array(dim = c(NITER, NTIME))
    # (prob. that harvest rate is less than that required to meet MNPL)
    object@objectives$harvest_rate <- array(dim = c(NITER, NTIME - 1))
    
    for (i in 1:NITER) {
        
        object@objectives$captures[i,]     <- apply(sweep(matrix(object@diagnostics$captures[i,,], nrow = SITER),     1, object@targets$captures[i], p_lower),     2, mean, na.rm = TRUE)
        object@objectives$depletion[i,]    <- apply(sweep(matrix(object@diagnostics$depletion[i,,], nrow = SITER),    1, object@targets$depletion[i], p_higher),   2, mean, na.rm = TRUE)
        object@objectives$harvest_rate[i,] <- apply(sweep(matrix(object@diagnostics$harvest_rate[i,,], nrow = SITER), 1, object@targets$harvest_rate[i], p_lower), 2, mean, na.rm = TRUE)
    }
    
    # dimnames (after calculations)
	# [life-history samples, process error iterations, ages, time]
    dimnames(N) <- list(sample = 1:NITER, iteration = 1:SITER, age = ages, time = time)
    
    # assign data
    object@.Data <- N
    
    # return
    return(object)
})
#}}}
