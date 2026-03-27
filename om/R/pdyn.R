#' @title Population dynamics function
#' 
#' @description The population dynamics function is called per-iteration.
#' 
#' @export
#' @include om-class.R
#' @import RTMB
#{{{ pdyn()
setGeneric("pdyn", function(object, stochastic, ...) standardGeneric("pdyn"))
setMethod("pdyn", signature = "om", function(object, stochastic = FALSE, ...) {
    
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
    
    # get seeds
    get_seeds(object, env = ENV)
    
    # set seed
    set.seed(rng_seed[1])
    
    # define process error
    if (stochastic) {
        
        # log-normal process error term
        sigmap <- sqrt(log(1 + object@data$cv_dynamics^2))
        
        # stochastic iterations
        siter <- object@data$stochastic_iterations
        
        # sample process error
        perr <- matrix(rnorm(siter * ntime, 0 - (sigmap^2) / 2, sigmap), nrow = siter, ncol = ntime)    
    
    } else {
        
        # dummy process error term
        sigmap <- 0.0
        
        # dummy iterations
        siter <- 1L
        
        # dummy process error
        perr <- matrix(0, nrow = siter, ncol = ntime) 
    }
    
    # setup numbers array
    # [life-history samples, process-error samples, ages, time]
    if (all(is.na(ages))) {
        N <- array(dim = c(niter, siter, 1, ntime))
    } else {
        N <- array(dim = c(niter, siter, nages, ntime))
    }
    
    # setup diagnostics
    # (catch)
    object@diagnostics$catch <- array(dim = c(niter, siter, ntime - 1)) #matrix(NA_real_, nrow = iter, ncol = ntime - 1)
    # (depletion)
    object@diagnostics$depletion <- array(dim = c(niter, siter, ntime)) #matrix(NA_real_, nrow = iter, ncol = ntime)
    # (harvest rate)
    object@diagnostics$harvest_rate <- array(dim = c(niter, siter, ntime - 1)) #matrix(NA_real_, nrow = iter, ncol = ntime - 1)
    
    # pst
    object@pst$value <- array(dim = c(niter, siter, ntime)) #matrix(NA_real_, nrow = iter, ncol = ntime)
    
    # progress
    msg <- ""
    cli_progress_step("Projecting dynamics{msg}", spinner = TRUE, msg_done = "Projected dynamics")
    
    # {{{
    # PT model
    if (all(is.na(object@ages)) | !(length(object@ages) > 1)) {
        
    } else {
    # {{{
    # AGE-STRUCTURED MODEL
        
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
        K      <- object@data$K
        
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
        g <- MakeTape(obj_fun, log(0.02))
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
        
        proj_n         <- array(dim = c(siter, nages, time))
        proj_h         <- array(dim = c(siter, ntime - 1))
        proj_catch     <- array(dim = c(siter, ntime - 1))
        proj_depletion <- array(dim = c(siter, ntime))
        
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
        
        #######################
        # monte-carlo samples #
        # from life-history   #
        # distributions       #
        #######################
        for (i in 1:niter) {
            
            # set seed
            set.seed(rng_seed[i])
            
            # progress iteration
            msg <- glue(", iteration {i}/", niter)
            
            # sample
            pars_sample <- lapply(object@pars, sample, n = 1)
            rmax_sample <- sample(object@pst$rmax, n = 1)
                
            # spin spinner
            cli_progress_update()
            
            # record pars sample
            # value if missing or 
            # overwrite
            invisible(lapply(1:length(pars_sample), function(j) {
                if (is.na(object@pars[[j]]@.Data[i])) {
                    object@pars[[j]]@.Data[i] <<- pars_sample[[j]]
                } else {
                    pars_sample[[j]] <<- object@pars[[j]]@.Data[i] 
                }
            }))
            
            # record rmax sample or
            # overwrite
            if (is.na(object@pst$rmax@.Data[i])) {
                object@pst$rmax@.Data[i] <- rmax_sample
            } else {
                rmax_sample <- object@pst$rmax@.Data[i] 
            }
            
            # productivity for estimation
            # of b_max
            lambda <- exp(pars_sample$r)
            
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
            # (1+ depletion = K)
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
            
            # loop over stochastic
            # process error
            for (j in 1:siter) {
                
                # initialise
                n[, 1] <- p_init
                
                # project under harvest rate
                # function
                # {{{
                for (y in 2:ntime) {
                    
                    proj_h[j, y - 1] <- object@harvest_rate(object, i)
                    
                    for (a in 2:nages) {
                        
                        m <- M[a - 1] + perr[j, y - 1]
                        
                        n[a, y] <- n[a - 1, y - 1] * exp(-1 * m) * (1 - sel[a - 1] * proj_h[j, y - 1]) 
                    }
                    
                    # plus group
                    m <- M[nages] + perr[j, y]
                        
                    n[nages, y] <- n[nages, y] + n[nages, y - 1] * exp(-1 * m) * (1 - sel[nages] * proj_h[j, y - 1])
                    
                    # birth
                    n[1, y] <- birth(y)
                }
                
                # values per-year
                proj_catch[j,]     <- apply(sweep(n, 1, sel, "*"), 2, sum)[-ntime] * proj_h[j,] + n[nages,] * sel[nages] * proj_h[j,] 
                proj_depletion[j,] <- apply(n[-1,], 2, sum) / sum(k[-1])
                proj_n[j,,]        <- n
                
                # spin spinner
                cli_progress_update()
            
            }
            
            # update time series diagnostics
            # (catch)
            object@diagnostics$catch[i,,] <- proj_catch
            # (depletion)
            object@diagnostics$depletion[i,,] <- proj_depletion
            # (harvest rate)
            object@diagnostics$harvest_rate[i,,] <- proj_h
            
            # numbers
            N[i,,,] <- proj_n
            
            # pst
            for (j in 1:siter) {
                object@pst$value[i,j,] <- (1 / 2) * object@pst$phi * sample(object@pst$rmax, n = 1) * apply(sweep(N[i,j,,], 1, mat, "*"), 2, sum)
            }
            
            # spin spinner
            cli_progress_update()
        }
    }

    # calculate objectives as the probability
    # of a desirable outcome
    p_higher <- function(x, y) ifelse(x > y, 1, ifelse(x < y, 0, 0.5))
    p_lower  <- function(x, y) ifelse(x < y, 1, ifelse(x > y, 0, 0.5))
    
    # (prob. that catch is less than that required to meet MNPL)
    object@objectives$catch        <- array(dim = c(niter, ntime - 1))
    # (prob. that depletion is greater than the depletion at MNPL)
    object@objectives$depletion    <- array(dim = c(niter, ntime))
    # (prob. that harvest rate is less than that required to meet MNPL)
    object@objectives$harvest_rate <- array(dim = c(niter, ntime - 1))
    
    for (i in 1:niter) {
        
        object@objectives$catch[i,]        <- apply(sweep(object@diagnostics$catch[i,,],        1, object@targets$catch, p_lower),        1, mean, na.rm = TRUE)
        object@objectives$depletion[i,]    <- apply(sweep(object@diagnostics$depletion[i,,],    1, object@targets$depletion, p_higher),   1, mean, na.rm = TRUE)
        object@objectives$harvest_rate[i,] <- apply(sweep(object@diagnostics$harvest_rate[i,,], 1, object@targets$harvest_rate, p_lower), 1, mean, na.rm = TRUE)
    }
    
    # dimnames (after calculations)
    #dimnames(object@diagnostics$catch)        <- list(iter = 1:niter, time = time[-ntime])
    #dimnames(object@diagnostics$depletion)    <- list(iter = 1:niter, time = time)
    #dimnames(object@diagnostics$harvest_rate) <- list(iter = 1:niter, time = time[-ntime])
    dimnames(N)                               <- list(iter = 1:niter, stochastic_iter = 1:siter, age = ages, time = time)
    
    # assign data
    object@.Data <- N
    
    # return
    return(object)
})
#}}}
