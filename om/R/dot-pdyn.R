#' @importFrom RTMB AD
# global inputs:
# - nages
# - pat
# - age_pat
# - sel
# - lambda
# - M
# - 
.pdyn <- function(h, shape, ntime, initial_depletion = 1) {
    
    n <- AD(array(dim = c(nages, ntime)))
    p <- vector("numeric", length = nages)
    
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
    
    # replacement birth rate
    # per female
    b_eq <- 1 / sum(pat * p)
    
    # maximum fecundity
    b_max <- 2 * (lambda^(age_pat) - S[2] * lambda^(age_pat - 1)) / (S[1] * S[2]^(age_pat - 1))
    
    # initilise population
    # at equilibrium
    n_init <- b_eq * p
    
    # check equilibrium number
    # of female pups
    isTRUE(all.equal(sum(pat * n_init * b_eq) / 2, n_init[1]))
    
    # initial conditions
    # (1+ depletion = 1)
    k <- n_init / sum(n_init[-1])
    
    # initialise
    n[, 1] <- k * initial_depletion
    
    # check birth function
    #isTRUE(all.equal(birth(1), n[1,1]))
    
    for (y in 2:ntime) {
        
        for (a in 2:nages) {
            
            m <- M[a - 1]
            
            n[a, y] <- n[a - 1, y - 1] * exp(-1 * m) * (1 - sel[a - 1] * h) 
        }
        
        # plus group
        m <- M[nages]
        
        n[nages, y] <- n[nages, y] + n[nages, y - 1] * exp(-1 * m) * (1 - sel[nages] * h)
        
        # birth
        n[1, y] <- birth(y)
    }
    
    return(n)
}

.pdyn2 <- function(h, shape, error, ntime, initial_depletion = 1) {
    
    # progress message
    cli_progress_step("Stochastic projection", spinner = TRUE, msg_done = "Done")
    
    n <- AD(array(dim = c(siter, nages, ntime)))
    p <- vector("numeric", length = nages)
    e <- error
    
    birth <- function(i, y) {
        0.5 * sum(pat[-1] * n[i,-1,y]) * (b_eq + (b_max - b_eq) * (1 - (sum(n[i,-1,y]) / sum(k[-1]))^shape))
    }
    
    # set up unexploited 
    # equilibrium female
    # population
    p[1] <- 0.5
    for(a in 2:nages) {
        p[a] <- p[a-1] * exp(-M[a - 1])
    }
    p[nages] <- p[nages] / (1 - exp(-M[nages]))
    
    # replacement birth rate
    # per female
    b_eq <- 1 / sum(pat * p)
    
    # maximum fecundity
    b_max <- 2 * (lambda^(age_pat) - S[2] * lambda^(age_pat - 1)) / (S[1] * S[2]^(age_pat - 1))
    
    # initilise population
    # at equilibrium
    n_init <- b_eq * p
    
    # check equilibrium number
    # of female pups
    isTRUE(all.equal(sum(pat * n_init * b_eq) / 2, n_init[1]))
    
    # initial conditions
    # (1+ depletion = 1)
    k <- n_init / sum(n_init[-1])
    
    # loop over stochastic
    # iterations
    for (i in 1:siter) {
        
        # spin spinner
        cli_progress_update()
        
        # initialise
        n[i,, 1] <- k * initial_depletion
        
        # check birth function
        #isTRUE(all.equal(birth(i,1), n[i,1,1]))
        
        for (y in 2:ntime) {
            
            for (a in 2:nages) {
                
                m <- M[a - 1] + e[i, y - 1]
                
                n[i, a, y] <- n[i, a - 1, y - 1] * exp(-1 * m) * (1 - sel[a - 1] * h) 
            }
            
            # plus group
            m <- M[nages] + e[i, y]
            
            n[i, nages, y] <- n[i, nages, y] + n[i, nages, y - 1] * exp(-1 * m) * (1 - sel[nages] * h)
            
            # birth
            n[i, 1, y] <- birth(i, y)
        }
    }
    
    return(n)
}

.ff <- function(h, shape, equilibrium_time = 1e3, env) {
    
    # run dynamics
    n <- do.call(".pdyn", list(h = h, shape = shape, ntime = equilibrium_time), envir = env)
    
    # equilibrium captures
    captures <- sum(n[, equilibrium_time] * sel * h)
    
    # equilibrium depletion
    depletion <- sum(n[-1, equilibrium_time])
    
    # equilibrium per-capita birth
    production <- n[1, equilibrium_time] / sum(n[-1, equilibrium_time] * pat[-1])
    
    # return lambda
    return(list(captures = captures, depletion = depletion, production = production))
}

.ff2 <- function(h, shape, error, equilibrium_time = 1e3, env) {
    
    # run dynamics
    n <- do.call(".pdyn2", list(h = h, shape = shape, error = error, ntime = equilibrium_time), envir = env)
    
    # recent time
    recent_time <- ceiling((2 / 3) * equilibrium_time):equilibrium_time
    
    # equilibrium captures
    captures <- mean(apply(sweep(n[,, recent_time], 2, sel, "*") * h, 1, sum) / length(recent_time))
    
    # equilibrium depletion
    depletion <- mean(apply(n[, -1, recent_time], 1, sum) / length(recent_time))
    
    # equilibrium per-capita birth
    production <- mean(apply(n[,1,recent_time], 1, sum) / apply(sweep(n[,-1, recent_time], 2, pat[-1], "*"), 1, sum))
    
    # return lambda
    return(list(captures = captures, depletion = depletion, production = production))
}


