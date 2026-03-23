#' @title Reference point calculation
#' @description 
#' Calculate the deterministic Maximum Net Productivity reference points.
#' 
#' @export
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R dot-pdyn.R
#' @import RTMB
#' @import cli
#{{{ rpd()
# wrapper for execution of function
setGeneric("rpd", function(object, ...) standardGeneric("rpd"))
setMethod("rpd", signature = "om", function(object, ...) {
    
    # current environment
    ENV <- environment()
    
    # check environment for function call is
    # consistent with current environment
    environment(.pdyn) <- ENV
    environment(.ff)   <- ENV
    
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
            
            n <- do.call(".pdyn", list(h = h, shape = shape, ntime = 1e3), envir = ENV)
            
            # objective function
            objective <- -1 * sum(n[, dim(n)[2]] * sel * h)
            
            # return
            return(objective)
        }
        
        ###################
        # first iteration #
        ###################
        
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
        h1 <- MakeTape(obj1, c(log(0.02), log(1)))
        h2 <- h1$newton(1)
        
        # record estimate
        object@targets$harvest_rate[1] <- exp(h2(log(object@shape)))
        object@targets$catch[1]        <- .ff(object@targets$harvest_rate[1], shape = object@shape, env = ENV)$captures
        object@targets$depletion[1]    <- .ff(object@targets$harvest_rate[1], shape = object@shape, env = ENV)$depletion
        
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
                object@targets$catch[i]        <- .ff(object@targets$harvest_rate[i], shape = object@shape, env = ENV)$captures
                object@targets$depletion[i]    <- .ff(object@targets$harvest_rate[i], shape = object@shape, env = ENV)$depletion
            }
        }
    # }}}
    }
    
    # return
    return(object)
})
#}}}
