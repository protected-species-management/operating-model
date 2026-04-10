#' @importFrom RTMB AD
.pdyn <- function(h, shape, survivorship) {
    
    n <- AD(array(dim = c(NAGES, NTIME)))
    p <- vector("numeric", length = NAGES)
    
    birth <- function(y) {
        0.5 * sum(pat[-1] * n[-1,y]) * (b_eq + (b_max - b_eq) * (1 - (sum(n[-1,y]) / sum(k[-1]))^shape))
    }
    
    # set up unexploited 
    # equilibrium female
    # population
    p[1] <- 0.5
    for(a in 2:NAGES) {
        p[a] <- p[a-1] * S[a - 1]
    }
    p[a] <- p[a] / (1 - S[a])
    
    # replacement birth rate
    # per female
    b_eq <- 1 / sum(pat * p)
    
    # maximum fecundity
    b_max <- 2 * (lambda^(age_mat + 1) - S[age_mat + 1] * lambda^(age_mat)) / prod(S[1:(age_mat + 1)])
    
    # population
    # at equilibrium
    k_prime <- b_eq * p
    
    # initial conditions
    # (1+ depletion = 1)
    k <- k_prime / sum(k_prime[-1])
    
    # use iteration to calculate
    # initial age structure
    # and depletion
    n_init <- AD(matrix(k, nrow = NAGES, ncol = 2))
    for (l in 2:1e3) {
        
        n_init[, 1] <- n_init[, 2]
        for(a in 2:NAGES) {
            n_init[a, 2] <- n_init[a - 1, 1] * S[a - 1] * (1 - sel[a - 1] * h)
        }
        n_init[a, 2] <- n_init[a, 2] + n_init[a, 1] * S[a] * (1 -  sel[a] * h)
        n_init[1, 2] <- 0.5 * sum(pat[-1] * n_init[-1, 2]) * (b_eq + (b_max - b_eq) * (1 - (sum(n_init[-1, 2]) / sum(k[-1]))^shape))
    }
    
    # initialise
    n[, 1] <- n_init[, 2]
    
    # project
    for (y in 2:NTIME) {
        
        for (a in 2:NAGES) {
            n[a, y] <- n[a - 1, y - 1] * survivorship[a - 1, y - 1] * (1 - sel[a - 1] * h) 
        }
        
        # plus group
        n[a, y] <- n[a, y] + n[a, y - 1] * survivorship[a, y - 1] * (1 - sel[a] * h)
        
        # birth
        n[1, y] <- birth(y)
    }
    
    return(n)
}

.pdyn2 <- function(h, shape, survivorship) {
    
    n <- AD(array(dim = c(NAGES, NTIME)))
    p <- vector("numeric", length = NAGES)
    
    birth <- function(y) {
        0.5 * sum(pat[-1] * n[-1,y]) * (b_eq + (b_max - b_eq) * (1 - (sum(n[-1,y]) / sum(k[-1]))^shape))
    }
    
    # set up unexploited 
    # equilibrium female
    # population
    p[1] <- 0.5
    for(a in 2:NAGES) {
        p[a] <- p[a-1] * S[a - 1]
    }
    p[a] <- p[a] / (1 - S[a])
    
    # replacement birth rate
    # per female
    b_eq <- 1 / sum(pat * p)
    
    # maximum fecundity
    b_max <- 2 * (lambda^(age_mat + 1) - S[age_mat + 1] * lambda^(age_mat)) / (S[1]^age_mat * S[age_mat + 1])
    
    # population
    # at equilibrium
    k_prime <- b_eq * p
    
    # initial conditions
    # (1+ depletion = 1)
    k <- k_prime / sum(k_prime[-1])
    
    # use iteration to calculate
    # initial age structure
    # and depletion
    n_init <- AD(matrix(k, nrow = NAGES, ncol = 2))
    for (l in 2:1e3) {
        
        n_init[, 1] <- n_init[, 2]
        for(a in 2:NAGES) {
            n_init[a, 2] <- n_init[a - 1, 1] * S[a - 1] * (1 - sel[a - 1] * h)
        }
        n_init[a, 2] <- n_init[a, 2] + n_init[a, 1] * S[a] * (1 -  sel[a] * h)
        n_init[1, 2] <- 0.5 * sum(pat[-1] * n_init[-1, 2]) * (b_eq + (b_max - b_eq) * (1 - (sum(n_init[-1, 2]) / sum(k[-1]))^shape))
    }
    
    # initialise
    n[, 1] <- n_init[, 2]
    
    # project
    for (y in 2:NTIME) {
        
        for (a in 2:NAGES) {
            n[a, y] <- n[a - 1, y - 1] * survivorship[a - 1, y - 1] * (1 - sel[a - 1] * h) 
        }
        
        # plus group
        n[a, y] <- n[a, y] + n[a, y - 1] * survivorship[a, y - 1] * (1 - sel[a] * h)
        
        # birth
        n[1, y] <- birth(y)
    }
    
    return(n)
}

.ff <- function(h, shape, survivorship, env) {
    
    # run dynamics
    N <- do.call(".pdyn", list(h = h, shape = shape, survivorship = survivorship), envir = env)
    
    # equilibrium female captures
    captures <- sum(N[, NTIME] * sel * h)
    
    # equilibrium depletion
    depletion <- sum(N[-1, NTIME])
    
    # equilibrium per-capita birth
    production <- N[1, NTIME] / sum(N[-1, NTIME] * pat[-1])
    
    # return dynamics
    return(list(captures = captures, depletion = depletion, production = production))
}

.ff2 <- function(h, shape, survivorship, env) {
    
    # setup
    N <- array(dim = c(dim(survivorship)[1], NAGES, NTIME))
    
    # run dynamics
    for (i in 1:dim(survivorship)[1]) {
        N[i,,] <- do.call(".pdyn2", list(h = h, shape = shape, survivorship = survivorship[i,,]), envir = env)
    }
    
    # recent time
    recent_time <- ceiling((2 / 3) * NTIME):NTIME
    
    # equilibrium female captures
    captures <- mean(apply(sweep(N[,, recent_time], 2, sel, "*") * h, 1, sum) / length(recent_time))
    
    # equilibrium depletion
    depletion <- mean(apply(N[, -1, recent_time], 1, sum) / length(recent_time))
    
    # equilibrium per-capita birth
    production <- mean(apply(N[,1,recent_time], 1, sum) / apply(sweep(N[,-1, recent_time], 2, pat[-1], "*"), 1, sum))
    
    # return dynamics
    return(list(captures = captures, depletion = depletion, production = production))
}


