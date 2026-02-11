#' @title Reference point calculation
#' @description 
#' Calculate the stochastic Maximum Net Productivity reference points.
#' 
#' @export
#' @include om-class.R
#' @import RTMB
#' @import tmbstan
#{{{ rp()
# wrapper for execution of function
setGeneric("rp", function(object, ...) standardGeneric("rp"))
setMethod("rp", signature = "om", function(object, ...) {
    
    # check environment for function call is
    # consistent with current environment
    environment(object@population_dynamics) <- environment()
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = environment())
    
    # setup numbers array
    if (any(is.na(ages))) {
        n <- array(dim = c(1, ntime, niter))
    } else {
        n <- array(dim = c(nages, ntime, niter))
    }
    
    equ_time <- 200
    n_iter   <- 200
    
    # error term
    sigmap <- sqrt(log(1 + object@data$cv_dynamics^2))
    
    process_error <- matrix(rnorm(equ_time * n_iter, 0 - (sigmap^2) / 2, sigmap), nrow = n_iter, ncol = equ_time)
    get_process_error <- function() get("process_error", envir = environment())
    
    log_r <- log(0.04)
    get_r <- function() exp(get("log_r", envir = environment()))
    
    obj_fun <- function(x) { 
        
        # parameter 
        # inputs
        h <- exp(x)
        #r <- 0.04 
        K <- 1500
        p <- 1

        r <- DataEval(get_r)
        e <- DataEval(get_process_error)
        b <- AD(matrix(nrow = n_iter, ncol = equ_time))
        
        for (i in 1:n_iter) {
            b[i,1] <- (K * (1 / (p + 1))^(1 / p)) * exp(e[i,1])
            for (j in 2:equ_time) {
                b[i,j] <- (b[i, j - 1] + r / p * b[i, j - 1] * (1 - (b[i, j - 1] / K)^p) - h * b[i, j - 1]) * exp(e[i,j])  
            }
        }
        
        catch_max <- mean(b[,equ_time] * h)
        
        # return maximum catch with penalty if
        # harvest rate is greater than r / 2
        return(-1 * catch_max + max(x - log(r / 2), 0))
        
    }
    
    # initialise with 
    # parameter values
    #g <- MakeTape(obj_fun, c(object@pars$log_r - log(2), object@pars$log_r, object@pars$log_K, object@data$shape))
    g <- MakeTape(obj_fun, object@pars$log_r - log(2) - 1)
    # get minimum over
    # first argument
    # (harvest rate)
    ff <- g$newton(1)
    # return value at minimum
    #ff(numeric())
    
    
    
    #object_data   <- object@data
    #object_data$r <- exp(object@pars$log_r)
    #object_data$K <- exp(object@pars$log_K)
    #    
    #object_pars <- object@pars
    #object_pars$log_K <- NULL
    #object_pars$log_r <- NULL
    
    #input_vector <- c(exp(object@pars$log_r), exp(object@pars$log_K), object@data$shape, rnorm(equ_time * n_iter, 0 - (sigmap^2) / 2, sigmap))
    
    # iterate
    values1 <- c()
    values2 <- c()
    svalues <- seq(0.01, 0.2, length = 101)
    for (i in 1:101) {
        
        process_error <- matrix(rnorm(equ_time * n_iter, 0 - (svalues[i]^2) / 2, svalues[i]), nrow = n_iter, ncol = equ_time)
        
        log_r <- log(0.04)
        
        values1 <- c(values1, ff(numeric()))
        
        log_r <- log(0.03)
        
        values2 <- c(values2, ff(numeric()))
    }
    plot(svalues, values1, ylim = range(c(values1, values2))); abline(h = log(0.04 / 2))
    points(svalues, values2, col = 2); abline(h = log(0.03 / 2))
    
    
    
    # targets
    # (catch)
    object@targets$catch <- rep(NA_real_, length(values)) 
    # (depletion)
    object@targets$depletion <- rep(NA_real_, length(values)) 
    # (harvest rate)
    object@targets$harvest_rate <- values
    
    
    return(object)
})
#}}}
