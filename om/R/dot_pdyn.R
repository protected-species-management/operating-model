#' @importFrom RTMB AD
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
