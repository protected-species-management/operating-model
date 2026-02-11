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
    
    # current environment
    ENV <- environment()
    
    # check environment for function call is
    # consistent with current environment
    #environment(object@population_dynamics) <- environment()
    
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
    
    # error term
    sigmap <- sqrt(log(1 + object@data$cv_dynamics^2))
    
    # accessor functions
    get_r <- function() exp(get("pars", envir = ENV)[1])
    get_K <- function() exp(get("pars", envir = ENV)[2])
    get_p <- function()     get("pars", envir = ENV)[3]
    
    get_process_error <- function() get("perr", envir = ENV)
    
    # initial values
    perr <- matrix(rnorm(object@data$equ_time * object@data$n_iter, 0 - (sigmap^2) / 2, sigmap), nrow = object@data$n_iter, ncol = object@data$equ_time)
    pars <- c(object@pars$log_r$pars[1], mean(object@pars$log_K$pars), object@data$shape)
    
    # AD objective function
    obj_fun <- function(x) { 
        
        # parameter 
        # value
        h <- exp(x)

        # inputs
        r <- DataEval(get_r)
        K <- DataEval(get_K)
        p <- DataEval(get_p)
        e <- DataEval(get_process_error)
        
        # AD matrix
        b <- AD(matrix(nrow = object@data$n_iter, ncol = object@data$equ_time))
        
        # dynamics
        for (i in 1:object@data$n_iter) {
            b[i,1] <- (K * (1 / (p + 1))^(1 / p)) * exp(e[i,1])
            for (j in 2:object@data$equ_time) {
                b[i,j] <- (b[i, j - 1] + r / p * b[i, j - 1] * (1 - (b[i, j - 1] / K)^p) - h * b[i, j - 1]) * exp(e[i,j])  
            }
        }
        
        # catch
        catch <- mean(b[,object@data$equ_time] * h)
        
        # return catch with penalty if
        # harvest rate is greater than r / 2
        return(-1 * catch + max(x - log(r / 2), 0))
    }
    
    # initialise with 
    # parameter values
    #g <- MakeTape(obj_fun, c(object@pars$log_r - log(2), object@pars$log_r, object@pars$log_K, object@data$shape))
    message("Compiling model...")
    g <- MakeTape(obj_fun, object@pars$log_r$pars[1] - log(2) - 1)
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
    #values1 <- c()
    #values2 <- c()
    #svalues <- seq(0.01, 0.2, length = 101)
    message("Estimating the harvest rate at MNPL")
    pb <- txtProgressBar(min = 0, max = object@data$n_iter, style = 3, width = 50, char = "=")
    
    iter <- 0
    
    values <- c()
    
    for (i in 1:object@data$n_iter) {
        
        # sample
        perr    <- matrix(rnorm(object@data$equ_time * object@data$n_iter, 0 - (sigmap^2) / 2, sigmap), nrow = object@data$n_iter, ncol = object@data$equ_time)
        pars[1] <- rnorm(1, object@pars$log_r$pars[1] - (object@pars$log_r$pars[2]^2) / 2, object@pars$log_r$pars[2])
        
        values <- c(values, ff(numeric()))
        
        # progress bar
        iter <- iter + 1
        setTxtProgressBar(pb, iter)
    }
    close(pb)
    
    windows()
    hist(values); abline(v = c(mean(values), object@pars$log_r$pars[1] - log(2)), lty = c(1,2))
    
    message("Done!")
    
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
