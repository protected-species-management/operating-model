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
    get_dim(object, env = ENV)
    
    # get seeds
    get_seeds(object, env = ENV)
    
    # get stochastic
    STOCHASTIC <- object@stochastic$ref_points
    
    # settings
    EQU_TIME <- object@settings$equilibrium_time
    SITER    <- object@settings$stochastic_iterations
    
    if (STOCHASTIC) {
        
        # set seed
        set.seed(rng_seed[1])
        
        # make sure
        # functions have correct
        # environment
        environment(.pdyn2) <- ENV
        environment(.ff2)   <- ENV
        
        # log-normal process error term
        sigmap <- sqrt(log(1 + object@fixed$cv_dynamics^2))
        
        # sample process error
        perr <- matrix(rnorm(SITER * EQU_TIME, 0 - (sigmap^2) / 2, sigmap), nrow = SITER, ncol = EQU_TIME)
        
    } else {
        
        # make sure
        # functions have correct
        # environment
        environment(.pdyn) <- ENV
        environment(.ff)   <- ENV
    }
    
    # output
    out <- list()
    
    #######################
    # monte-carlo samples #
    # from life-history   #
    # distributions       #
    #######################
    for (i in 1:niter) {
        
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
        
        dvalue <- numeric(length(harvest_rate))
        cvalue <- numeric(length(harvest_rate))
        pvalue <- numeric(length(harvest_rate))
        
        if (STOCHASTIC) {
            
            cli_progress_step("Calculating stochastic surplus production function ...", spinner = TRUE, msg_done = "Calculated stochastic production function", .envir = ENV)
            
            for (j in 1:length(harvest_rate)) {
                
                tmp <- .ff2(harvest_rate[j], shape = object@shape, error = perr, equilibrium_time = EQU_TIME, env = ENV)
                
                cvalue[j] <- tmp$captures
                dvalue[j] <- tmp$depletion
                pvalue[j] <- tmp$production
                
                # spin spinner
                cli_progress_update(.envir = ENV)
            }
            
        } else {
            
            for (j in 1:length(harvest_rate)) {
                
                tmp <- .ff(harvest_rate[j], shape = object@shape, equilibrium_time = EQU_TIME, env = ENV)
                
                cvalue[j] <- tmp$captures
                dvalue[j] <- tmp$depletion
                pvalue[j] <- tmp$production
            }
        }
        
        out[[i]] <- data.frame(harvest_rate = harvest_rate, captures = cvalue, depletion = dvalue, productivity = pvalue)
    }
    
    # return
    return(bind_rows(out, .id = "iteration"))
}


