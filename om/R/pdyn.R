#' @title Population dynamics function
#' 
#' @description The population dynamics function is called per-iteration.
#' 
#' @export
#' @include om-class.R
#' @import RTMB
#{{{ pdyn()
# wrapper for execution of population
# dynamics function
# -- executes object@pdyn for each monte-carlo sample
setGeneric("pdyn", function(object, ...) standardGeneric("pdyn"))
setMethod("pdyn", signature = "om", function(object, ...) {
    
    # current environment
    ENV <- environment()
    
    # make sure harvest rate
    # function has correct
    # environment
    environment(object@harvest_rate) <- ENV
    
    # load time, age and
    # iteration dimensions
    # into function environment
    get_dim(object, env = ENV)
    
    # get data
    get_data(object, env = ENV)
    
    # setup numbers array
    if (all(is.na(ages))) {
        N <- array(dim = c(niter, 1, ntime))
    } else {
        N <- array(dim = c(niter, nages, ntime))
    }
    
    # error term
    sigmap <- sqrt(log(1 + cv_dynamics^2))
    
    # initial values
    # (process error)
    perr <- matrix(rnorm(equ_time * equ_iter, 0 - (sigmap^2) / 2, sigmap), nrow = equ_iter, ncol = equ_time)
    # (parameter values - check order)
    pars <- unlist(lapply(object@pars, sample, n = 1))
    
    # accessor functions
    get_K    <- function() exp(get("pars", envir = ENV)[1])
    get_r    <- function() exp(get("pars", envir = ENV)[2])
    get_perr <- function() get("perr", envir = ENV)
    
    # setup diagnostics
    # (catch)
    object@diagnostics$catch <- matrix(NA_real_, nrow = iter, ncol = ntime - 1)
    # (depletion)
    object@diagnostics$depletion <- matrix(NA_real_, nrow = iter, ncol = ntime)
    # (harvest rate)
    object@diagnostics$harvest_rate <- matrix(NA_real_, nrow = iter, ncol = ntime - 1)
    
    # pst
    object@pst$value <- matrix(NA_real_, nrow = iter, ncol = ntime)
    
    # progress
    msg <- ""
    cli_progress_step("Projecting dynamics{msg}", spinner = TRUE, msg_done = "Projected dynamics")
    
    # {{{
    # PT model
    if (all(is.na(object@ages)) | !(length(object@ages) > 1)) {
        
        for (i in 1:niter) {
            
            # spin spinner
            cli_progress_update()
            
            # set seed
            set.seed(rng_seed[i])
            
            # progress iteration
            msg <- glue(", iteration {i}/", niter)
            
            # spin spinner
            cli_progress_update()
            
            # sample
            perr <- rnorm(ntime, 0 - (sigmap^2) / 2, sigmap)
            pars <- unlist(lapply(object@pars, sample, n = 1))
            rmax <- sample(object@pst$rmax, n = 1)
            
            # record pars sample
            # value if missing or 
            # overwrite
            invisible(lapply(1:length(pars), function(j) {
                if (is.na(object@pars[[j]]@.Data[i])) {
                    object@pars[[j]]@.Data[i] <<- pars[j]
                } else {
                    pars[j] <<- object@pars[[j]]@.Data[i] 
                }
            }))
            
            # record rmax sample or
            # overwrite
            if (is.na(object@pst$rmax@.Data[i])) {
                object@pst$rmax@.Data[i] <- rmax
            } else {
                rmax <- object@pst$rmax@.Data[i] 
            }
            
            # spin spinner
            cli_progress_update()
            
            # project under harvest rate
            # function
            # {{{
            b <- numeric(ntime)
            K <- exp(pars['log_K'])
            r <- exp(pars['log_r'])
            e <- perr
            h <- numeric(ntime - 1)
            
            # dynamics
            b[1] <- (K * initial_depletion) * exp(e[1])
            
            for (j in 2:ntime) {
                
                h[j - 1] <- object@harvest_rate(object, i)
                
                b[j] <- (b[j - 1] + r / shape * b[j - 1] * (1 - (b[j - 1] / K)^shape) - h[j - 1] * b[j - 1]) * exp(e[j])  
            }
            #}}}
            
            # spin spinner
            cli_progress_update()
            
            # update diagnostics
            # (catch)
            object@diagnostics$catch[i,] <- b[-ntime] * h
            # (depletion)
            object@diagnostics$depletion[i,] <- b / K
            # (harvest rate)
            object@diagnostics$harvest_rate[i,] <- h
            
            # spin spinner
            cli_progress_update()
            
            # population
            N[i, 1, ] <- apply(b, 2, mean)
            
            # pst
            object@pst$value[i, ] <- (1 / 2) * object@pst$phi * object@pst$rmax[i] * N[i, 1, ]
        }
    } else {
    # {{{
    # AGE-STRUCTURED MODEL
        
        # vectors from age = 0 to age = nages - 1
        mat    <- c(rep(0, age_mat + 1), rep(1, nages - age_mat - 1))
        pat    <- c(rep(0, age_pat + 1), rep(1, nages - age_pat - 1))
        sel    <- c(rep(0, age_sel + 1), rep(1, nages - age_sel - 1))
        S      <- c(S0, rep(S1, nages - 1))
        M      <- -log(S)
        
        # dummy values
        b_eq  <- 0
        b_max <- 0
        k     <- numeric(nages)
        
        # accessor functions
        get_beq  <- function() get("b_eq",  envir = ENV)
        get_bmax <- function() get("b_max", envir = ENV)
        get_k    <- function() get("k",     envir = ENV)
        
        # objective function
        obj_fun <- function(x) {
            
            h <- exp(x)
            
            p_init <- AD(vector("numeric", length = nages))
            
            # monte-carlo
            # inputs
            b_eq  <- DataEval(get_beq)
            b_max <- DataEval(get_bmax)
            k     <- DataEval(get_k)
                
            # equilibrium age
            # structure
            p_init[1] <- 0.5 * sum(pat[-1] * k[-1] * initial_depletion) * (b_eq + (b_max - b_eq) * (1 - initial_depletion^shape))
            for(a in 2:nages) {
                p_init[a] <- (p_init[a-1] * exp(-M[a - 1]) * (1 - sel[a - 1] * h))
            }
            p_init[nages] <- p_init[nages] + (p_init[nages] * exp(-1 * M[nages]) * (1 - sel[nages] * h))
                
            # log of the equilibrium depletion
            objective <- -1 * dnorm(sum(p_init[-1]) / sum(k[-1]), initial_depletion, 0.01, log = TRUE)
            
            # return
            return(objective)
        }
        
        # initialise with 
        # parameter value
        g <- MakeTape(obj_fun, object@pars$log_r@pars[1] - log(object@data$shape + 1))
        # function to 
        # estimate minimum
        # over first argument
        # (harvest rate)
        suppressWarnings({
            ff <- g$newton(1)
        })
        
        # set-up arrays
        n      <- array(dim = c(nages, ntime))
        p      <- vector("numeric", length = nages)
        p_init <- vector("numeric", length = nages)
        proj_h <- vector("numeric", length = ntime - 1)
        
        # set-up birth function
        birth <- function(y) {
            0.5 * sum(pat[-1] * n[-1,y]) * (b_eq + (b_max - b_eq) * (1 - (sum(n[-1,y]) / sum(k[-1]))^shape)) 
        }
        
        # set up unexploited 
        # equilibrium female
        # population
        p[1] <- 0.5
        for(a in 2:nages) {
            p[a] <- p[a-1] * exp(-M[a - 1])
        }
        p[nages] <- p[nages] / (1 - exp(-M[nages]))
        
        # add dimensions to pars
        object@pars <- lapply(object@pars, function(x) {
            if (x@iter == 0 | length(x@.Data) <= 1) {
                x@iter  <- object@iter 
                x@.Data <- rep(NA_real_, object@iter)
            }
            return(x) 
        })
        
        # add dimensions to rmax
        if (object@pst$rmax@iter == 0 | length(object@pst$rmax@.Data) <= 1) {
            object@pst$rmax@iter  <- object@iter 
            object@pst$rmax@.Data <- rep(NA_real_, object@iter)
        }
        
        # loop over monte-carlo
        # samples from 'pars'
        for (i in 1:niter) {
            
            # set seed
            set.seed(rng_seed[i])
            
            # progress iteration
            msg <- glue(", iteration {i}/", niter)
            
            # spin spinner
            cli_progress_update()
            
            # sample
            perr <- rnorm(ntime, 0 - (sigmap^2) / 2, sigmap)
            pars <- unlist(lapply(object@pars, sample, n = 1))
            rmax <- sample(object@pst$rmax, n = 1)
            
            # record pars sample
            # value if missing or 
            # overwrite
            invisible(lapply(1:length(pars), function(j) {
                if (is.na(object@pars[[j]]@.Data[i])) {
                    object@pars[[j]]@.Data[i] <<- pars[j]
                } else {
                    pars[j] <<- object@pars[[j]]@.Data[i] 
                }
            }))
            
            # record rmax sample or
            # overwrite
            if (is.na(object@pst$rmax@.Data[i])) {
                object@pst$rmax@.Data[i] <- rmax
            } else {
                rmax <- object@pst$rmax@.Data[i] 
            }
            
            # monte-carlo
            # inputs
            K <- exp(pars['log_K'])
            r <- exp(pars['log_r'])
            e <- perr
            
            # productivity for estimation
            # of b_max
            lambda <- exp(r)
            
            # replacement birth rate
            # per female
            b_eq  <- 1 / sum(pat * p)
            
            # maximum birth rate
            # per female
            b_max <- 2 * (lambda^(age_pat) - S1 * lambda^(age_pat - 1)) / (S0 * S1^(age_pat - 1))
            
            # initialise population
            # at equilibrium
            n_init <- b_eq * p
            
            # initial conditions
            # (1+ depletion = 1)
            k <- K * n_init / sum(n_init[-1])
            
            # initial conditions
            h_init <- exp(ff(numeric()))
            
            # equilibrium age
            # structure
            p_init[1] <- 0.5 * sum(pat[-1] * k[-1] * initial_depletion) * (b_eq + (b_max - b_eq) * (1 - initial_depletion^shape))
            for(a in 2:nages) {
                p_init[a] <- (p_init[a-1] * exp(-M[a - 1]) * (1 - sel[a - 1] * h_init))
            }
            p_init[nages] <- p_init[nages] + (p_init[nages] * exp(-1 * M[nages]) * (1 - sel[nages] * h_init))
                
            # initialise
            n[, 1] <- p_init * exp(e[1]) 
            
            # project under harvest rate
            # function
            # {{{
            for (y in 2:ntime) {
                
                proj_h[y - 1] <- object@harvest_rate(object, i)
                
                for (a in 2:nages) {
                    
                    n[a, y] <- (n[a - 1, y - 1] * exp(-1 * M[a - 1]) * (1 - sel[a - 1] * proj_h[y - 1])) * exp(e[y])  
                }
                
                # plus group
                n[nages, y] <- n[nages, y] + (n[nages, y - 1] * exp(-1 * M[nages]) * (1 - sel[nages] * proj_h[y - 1])) * exp(e[y]) 
                
                # birth
                n[1, y] <- birth(y)
            }
            
            # values per-year
            proj_catch     <- apply(sweep(n, 1, sel, "*"), 2, sum)[-ntime] * proj_h
            proj_depletion <- apply(n[-1,], 2, sum) / sum(k[-1])
            proj_n         <- n
            
            # spin spinner
            cli_progress_update()
            
            # update time series diagnostics
            # (catch)
            object@diagnostics$catch[i,] <- proj_catch
            # (depletion)
            object@diagnostics$depletion[i,] <- proj_depletion
            # (harvest rate)
            object@diagnostics$harvest_rate[i,] <- proj_h
            
            # numbers
            N[i,,] <- proj_n
            
            # pst
            object@pst$value[i,] <- (1 / 2) * object@pst$phi * object@pst$rmax[i] * apply(sweep(N[i,,], 1, mat, "*"), 2, sum)
            
            # spin spinner
            cli_progress_update()
        }
    }

    # calculate objectives as the probability
    # of a desirable outcome
    p_higher <- function(x, y) ifelse(x > y, 1, ifelse(x < y, 0, 0.5))
    p_lower  <- function(x, y) ifelse(x < y, 1, ifelse(x > y, 0, 0.5))
    # (prob. that catch is less than that required to meet MNPL)
    object@objectives$catch        <- apply(sweep(object@diagnostics$catch,        1, object@targets$catch, p_lower),        1, mean, na.rm = TRUE)
    # (prob. that depletion is greater than the depletion at MNPL)
    object@objectives$depletion    <- apply(sweep(object@diagnostics$depletion,    1, object@targets$depletion, p_higher),   1, mean, na.rm = TRUE)
    # (prob. that harvest rate is less than that required to meet MNPL)
    object@objectives$harvest_rate <- apply(sweep(object@diagnostics$harvest_rate, 1, object@targets$harvest_rate, p_lower), 1, mean, na.rm = TRUE)
    
    # dimnames (after calculations)
    dimnames(object@diagnostics$catch)        <- list(iter = 1:niter, time = time[-ntime])
    dimnames(object@diagnostics$depletion)    <- list(iter = 1:niter, time = time)
    dimnames(object@diagnostics$harvest_rate) <- list(iter = 1:niter, time = time[-ntime])
    dimnames(N)                               <- list(iter = 1:niter, age = ages, time = time)
    
    # assign data
    object@.Data <- N
    
    # return
    return(object)
})
#}}}
