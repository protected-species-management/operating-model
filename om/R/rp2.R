#' @title Reference point calculation
#' @description 
#' Calculate the stochastic Maximum Net Productivity reference points.
#' 
#' @export
#' @include om-class.R distribution-class.R distribution.R sample.distribution.R
#' @import RTMB
#' @import cli
#{{{ rp()
# wrapper for execution of function
setGeneric("rp2", function(object, ...) standardGeneric("rp2"))
setMethod("rp2", signature = "om", function(object, ...) {
    
    # current environment
    ENV <- environment()
    
    # check environment for function call is
    # consistent with current environment
    environment(.pdyn) <- ENV
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    # load data inputs stored
    # in object@data
    get_data(object, env = ENV)
    
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
        # function and tape
        obj1 <- function(x) {
            
            h     <- exp(x[1])
            shape <- exp(x[2])
            
            n <- do.call(".pdyn", list(h = h, shape = shape, ntime = 1e3), envir = ENV)
            
            # objective function
            objective <- -1 * sum(n[, dim(n)[2]] * sel * h)
            
            # return lambda
            return(objective)
        }
        
        environment(obj1) <- ENV
        
        ff <- function(h, shape) {
            
            # run dynamics
            n <- do.call(".pdyn", list(h = h, shape = shape, ntime = 1e3), envir = ENV)
            
            # equilibrium captures
            captures <- sum(n[, equ_time] * sel * h)
            
            # equilibrium depletion
            depletion <- sum(n[-1, equ_time])
            
            # return lambda
            return(list(captures = captures, depletion = depletion))
        }
        
        environment(ff) <- ENV
        
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
            
            # function to estimate h_mnpl
            # given shape
            h1 <- MakeTape(obj1, c(log(0.02), log(1)))
            h2 <- h1$newton(1)
            
            # record estimate
            object@targets$harvest_rate[i] <- exp(h2(log(object@shape)))
            object@targets$catch[i]        <- ff(object@targets$harvest_rate[i], shape = object@shape)$captures
            object@targets$depletion[i]    <- ff(object@targets$harvest_rate[i], shape = object@shape)$depletion
        }
    # }}}
    }
    
    # return
    return(object)
})
#}}}
