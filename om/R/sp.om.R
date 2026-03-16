#' @title Surplus production function
#' @description Extracts data frame containing determinisitic relationships between the depletion, sustainable captures and the harvest rate. Depletion is measured using the 1+ age classes.
#' @details This function is designed to facilitate the easy creation of plots of the production function, that can be used to validate operating model assumptions regarding the depletion at MNPL.
#' @return A data frame containing depletion, sustainable captures and the harvest rate, for each of the input harvest rate values. If life-history inputs are uncertain, iterations are sampled. These iterations do not represent any process error, only uncertainty in the operating model conditioning. 
#' @include dot_pdyn.R
#' @importFrom dplyr bind_rows
#' @export
sp <- function(object, harvest_rate, ...) UseMethod("sp")
#' @rdname sp
#' @export
sp.om <- function(object, harvest_rate, ...) {
    
    # current environment
    ENV <- environment()
    
    # make sure .pdyn
    # function has correct
    # environment
    environment(.pdyn) <- ENV
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    # get data
    get_data(object, env = ENV)
    
    ff <- function(h, shape) {
        
        # run dynamics
        n <- do.call(".pdyn", list(h = h, shape = shape, ntime = 1e3), envir = ENV)
        
        # equilibrium captures
        captures <- sum(n[, equ_time] * sel * h)
        
        # equilibrium depletion
        depletion <- sum(n[-1, equ_time])
        
        # equilibrium per-capita birth
        production <- n[1, equ_time] / sum(n[-1, equ_time] * pat[-1])
        
        # return lambda
        return(list(captures = captures, depletion = depletion, production = production))
    }
    
    environment(ff) <- ENV
    
    # output
    out <- list()
    
    # monte-carlo samples
    for (i in 1:niter) {
        
        # sample pars
        pars <- lapply(object@pars, sample, n = 1)
        
        # setup (1)
        age_mat <- as.integer(pars$a)
        age_pat <- age_mat + 1L
        
        # setup (2)
        mat    <- c(rep(0, age_mat), rep(1, nages - age_mat))
        pat    <- c(0, mat[-length(mat)])
        sel    <- c(rep(0, age_sel), rep(1, nages - age_sel))
        M      <- c(sqrt(pars$M), rep(pars$M, nages - 1))
        S      <- exp(-M)
        lambda <- exp(pars$r)
        
        hseq   <- seq(0.00, max(object@pst$rmax), length = 1001)
        dvalue <- numeric(length(hseq))
        cvalue <- numeric(length(hseq))
        pvalue <- numeric(length(hseq))
        
        for (j in 1:length(harvest_rate)) {
            
            tmp <- ff(harvest_rate[j], shape = object@shape)
            
            cvalue[j] <- tmp$captures
            dvalue[j] <- tmp$depletion
            pvalue[j] <- tmp$production
        }
        
        out[[i]] <- data.frame(harvest_rate = harvest_rate, captures = cvalue, depletion = dvalue, productivity = pvalue)
    }
    
    # return
    return(bind_rows(out, .id = "iteration"))
}


