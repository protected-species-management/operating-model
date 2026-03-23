#' @title Stochastic reference point calculation
#' @description 
#' Calculate the stochastic Maximum Net Productivity reference points.
#' 
#' @export
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R dot-pdyn.R
#' @import RTMB
#' @import cli
#{{{ rps()
# wrapper for execution of function
setGeneric("rps", function(object, ...) standardGeneric("rps"))
setMethod("rps", signature = "om", function(object, ...) {
    
    # current environment
    ENV <- environment()
    
    # check environment for function call is
    # consistent with current environment
    environment(.pdyn2) <- ENV
    environment(.ff2)   <- ENV
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    # load data inputs stored
    # in object@data
    #get_data(object, env = ENV)
    
    # reset targets
    # (catch)
    object@targets$catch <- numeric(object@iter)
    # (depletion)
    object@targets$depletion <- numeric(object@iter)
    # (harvest rate)
    object@targets$harvest_rate <- numeric(object@iter)
    
    # PT model
    # {{{
    if (all(is.na(object@ages)) | !(length(object@ages) > 1)) {
        
    # }}}
    } else {
    # AGE-STRUCTURED MODEL    
    # {{{
        
        # set up objective
        # function to estimate
        # harvest rate at 
        # maximum sustainable 
        # catch
        obj1 <- function(x) {
            
            h     <- exp(x[1])
            shape <- exp(x[2])
            
            n <- do.call(".pdyn2", list(h = h, shape = shape, error = perr, ntime = 1e3), envir = ENV)
            
            # recent time
            loc <- ceiling((2 / 3) * dim(n)[3]):dim(n)[3]
            
            # objective function from
            # mean across stochastic
            # iterations
            objective <- -1 * mean(apply(sweep(n[,, loc], 2, sel, "*") * h, 1, sum) / length(loc))
            
            # return
            return(objective)
        }
        
        ###################
        # first iteration #
        ###################
        
        # sample pars
        pars_sample <- lapply(object@pars, sample, n = 1)
        
        # log-normal process error term
        sigmap <- sqrt(log(1 + object@data$cv_dynamics^2))
        
        # stochastic iterations
        siter <- object@data$stochastic_iterations
        
        # sample process error
        perr <- matrix(rnorm(siter * 1e3, 0 - (sigmap^2) / 2, sigmap), nrow = siter, ncol = 1e3)
        
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
        
        # progress message
        cli_progress_message("Compiling model...")
        
        # function to estimate h_mnpl
        # given shape
        h1 <- MakeTape(obj1, c(log(0.02), log(1)))
        h2 <- h1$newton(1)
        
        # record estimate
        object@targets$harvest_rate[1] <- exp(h2(log(object@shape)))
        object@targets$catch[1]        <- .ff2(object@targets$harvest_rate[1], shape = object@shape, error = perr, env = ENV)$captures
        object@targets$depletion[1]    <- .ff2(object@targets$harvest_rate[1], shape = object@shape, error = perr, env = ENV)$depletion
        
        #######################
        # monte-carlo samples #
        # from life-history   #
        # distributions       #
        #######################
        if (niter > 1) {
            for (i in 2:niter) {
                
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
                
                # record estimate
                object@targets$harvest_rate[i] <- exp(h2(log(object@shape)))
                object@targets$catch[i]        <- .ff2(object@targets$harvest_rate[i], shape = object@shape, error = perr, env = ENV)$captures
                object@targets$depletion[i]    <- .ff2(object@targets$harvest_rate[i], shape = object@shape, error = perr, env = ENV)$depletion
            }
        }
    # }}}
    }
    
    # return
    return(object)
})
#}}}
