#' @title Surplus production function
#' @description Extracts data frame containing deterministic relationships between the depletion, sustainable captures and the harvest rate. Depletion is measured using the 1+ age classes.
#' @details This function is designed to facilitate the easy creation of plots of the production function, that can be used to validate operating model assumptions regarding the depletion at MNPL.
#' @return A data frame containing depletion, sustainable captures and the harvest rate, for each of the input harvest rate values. If life-history inputs are uncertain, iterations are sampled. These iterations do not represent any process error, only uncertainty in the operating model conditioning. 
#' @include dot-pdyn.R
#' @importFrom dplyr bind_rows
#' @export
sp <- function(object, harvest_rate, ...) UseMethod("sp")
#' @rdname sp
#' @export
sp.om <- function(object, harvest_rate, ...) {
    
    # current environment
    ENV <- environment()
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, ref_points = TRUE, env = ENV)
    
    # get seeds
    get_seeds(object, env = ENV)
    
    # output
    out <- list()
    
    #######################
    # monte-carlo samples #
    # from life-history   #
    # distributions       #
    #######################
    for (i in 1:NITER) {
        
        # set seed
        set.seed(rng_seed[i])
        
        # sample pars
        pars_sample <- lapply(object@pars, sample, n = 1)
        
        # assign pars
		a <- pars_sample$a
		r <- pars_sample$r
		M <- pars_sample$M
		v <- as.integer(object@fixed$selectivity)
        
        dvalue <- numeric(length(harvest_rate))
        cvalue <- numeric(length(harvest_rate))
        pvalue <- numeric(length(harvest_rate))
        
        if (STOCHASTIC) {
            
            cli_progress_step("Calculating stochastic surplus production function ...", spinner = TRUE, msg_done = "Calculated stochastic production function", .envir = ENV)
            
            # process error term
            sigmap <- object@fixed$cv_survivorship * S
            
            # calculate mu given sigmap
            mu_calc <- function(survivorship, sigma) uniroot(function(mu) survivorship - pnorm(mu / sqrt(1 + sigma^2)), interval = c(-10, 10))$root
            
            mu <- numeric(NAGES)
            for (a in 1:NAGES) {
                mu[a] <- mu_calc(S[a], sigmap[a])
            }
            
            s <- array(dim = c(SITER, NAGES, NTIME))
            
            for (a in 1:NAGES) {
                
                e <- rnorm(SITER * NTIME, mu[a], sigmap[a])
                
                s[,a,] <- pnorm(e)
                
                # first year is
                # equal to expectation
                s[,a,1] <- S[a]
            }
            
            for (j in 1:length(harvest_rate)) {
                
                tmp <- .ff2(harvest_rate[j], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r), env = ENV)
                
                cvalue[j] <- tmp$captures
                dvalue[j] <- tmp$depletion
                pvalue[j] <- tmp$production
                
                # spin spinner
                cli_progress_update(.envir = ENV)
            }
            
        } else {
            
            s <- array(dim = c(NAGES, NTIME))
            for (j in 1:NAGES) {
                s[j,] <- exp(-M)
            }
            
            for (k in 1:length(harvest_rate)) {
                
                tmp <- .ff(harvest_rate[k], shape = object@shape, survivorship = s, maturity = a, selectivity = v, lambda = exp(r), env = ENV)
                
                cvalue[k] <- tmp$captures
                dvalue[k] <- tmp$depletion
                pvalue[k] <- tmp$production
            }
        }
        
        out[[i]] <- data.frame(harvest_rate = harvest_rate, captures = cvalue, depletion = dvalue, productivity = pvalue)
    }
    
    # return
    return(bind_rows(out, .id = "iteration"))
}


