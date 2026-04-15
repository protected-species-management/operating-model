#' @title Surplus production function
#' @description Extracts data frame containing relationships between the depletion, sustainable captures and the harvest rate. Depletion is measured using the 1+ age classes.
#' @details This function is designed to facilitate the easy creation of plots of the production function, that can be used to validate operating model assumptions regarding the depletion and harvest rate at MNPL. The production function is calculated assuming either deterministic or stochastic reference point calculations, depending on the setting stored in \code{object@settings$ref_points}.
#' @return A data frame containing depletion, sustainable captures and the harvest rate, for each of the input harvest rate values. If life-history inputs are uncertain, iterations are sampled. These iterations do not represent any process error, only uncertainty in the operating model conditioning. 
#' @include dot-pdyn.R dot-survivorship.R
#' @importFrom dplyr bind_rows
#' @import cli
#' @import glue
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
	
	msg <- ""
    cli_progress_step("Calculating surplus production function{msg}", spinner = TRUE, msg_done = "Calculated production function", .envir = ENV)
			
    for (i in 1:NITER) {
        
        # set seed
        set.seed(rng_seed[i])
        
		# progress sample
        msg <- glue(", sample {i}/", NITER)
			
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
            
            s <- .survivorship(M, object@settings$cv$survivorship, env = ENV)
            
            for (j in 1:length(harvest_rate)) {
                
                tmp <- .ff2(harvest_rate[j], shape = object@shape[i], survivorship = s, maturity = a, selectivity = v, lambda = exp(r), env = ENV)
                
                cvalue[j] <- tmp$captures
                dvalue[j] <- tmp$depletion
                pvalue[j] <- tmp$production
                
                # spin spinner
                cli_progress_update(.envir = ENV)
            }
            
        } else {
            
            s <- .survivorship(M, env = ENV)
            
            for (k in 1:length(harvest_rate)) {
                
                tmp <- .ff(harvest_rate[k], shape = object@shape[i], survivorship = s, maturity = a, selectivity = v, lambda = exp(r), env = ENV)
                
                cvalue[k] <- tmp$captures
                dvalue[k] <- tmp$depletion
                pvalue[k] <- tmp$production
				
				# spin spinner
                cli_progress_update(.envir = ENV)
            }
        }
        
        out[[i]] <- data.frame(harvest_rate = harvest_rate, captures = cvalue, depletion = dvalue, productivity = pvalue)
    }
    
    # return
    return(bind_rows(out, .id = "iteration"))
}


