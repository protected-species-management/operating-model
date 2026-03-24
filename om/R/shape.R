#' @title Calculate shape
#' 
#' @description Calculates shape parameter given depeletion at MNPL.
#' 
#' @export
#' @include om-class.R dot-pdyn.R
#' @import RTMB
#' @import cli
#{{{ shape()
# wrapper for execution of population
# dynamics function
# -- executes object@pdyn for each monte-carlo sample
setGeneric("shape", function(object, depletion, ...) standardGeneric("shape"))
setMethod("shape", signature = "om", function(object, depletion = 0.5, ...) {
    
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
    #get_data(object, env = ENV)
    
    # create container
    shape_values <- numeric(object@iter)
    
    # progress message
    cli_progress_step("Estimating the shape parameter ...", spinner = FALSE, msg_done = "Estimated shape = {round(object@shape, 2)}")
    
    # set up objective
    # function and tape
    # for harvest rate at
    # maximum sustainable 
    # catch
    obj1 <- function(x) {
        
        h     <- exp(x[1])
        shape <- exp(x[2])
        
        n <- do.call(".pdyn", list(h = h, shape = shape, ntime = 1e3), envir = ENV)
        
        # objective function
        objective <- -1 * sum(n[, dim(n)[2]] * sel * h)
        
        # return lambda
        return(objective)
    }
    
    # set up objective
    # function and tape
    # for depletion at 
    # target 
    obj2 <- function(x) {
        
        shape  <- exp(x[1])
        h      <- exp(h2(x[1])) # internal estimation of h_mnpl given shape
        target <- x[2]
        
        n <- do.call(".pdyn", list(h = h, shape = shape, ntime = 1e3), envir = ENV)
        
        # objective function
        objective <- -1 * dnorm(sum(n[-1, dim(n)[2]]), depletion, 0.01, log = TRUE)
        
        # return objective
        return(objective)
    }
    
    # first iteration
    # sample pars
    pars_sample <- lapply(object@pars, sample, n = 1)
    
    # check data present
    stopifnot(length(object@data) > 0)
    
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
    
    #exp(h2(log(shape)))
    
    # function to estimate
    # shape given target
    i1 <- MakeTape(obj2, c(log(1), 0.5))
    i2 <- i1$newton(1)
    
    # record estimate
    shape_values[1] <- exp(i2(depletion))
    
    # monte-carlo sample
    # parameters
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
            
            #exp(h2(log(shape)))
            
            # function to estimate
            # shape given target
            #i1 <- MakeTape(obj2, c(log(1), 0.5))
            #i2 <- i1$newton(1)
            
            # record estimate
            shape_values[i] <- exp(i2(depletion))
        }
    }
    
    # average across
    # samples
    object@shape <- mean(shape_values)
    
    # return
    return(object)
})

