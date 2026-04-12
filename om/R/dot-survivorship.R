
.survivorship <- function(M, a, cv_survivorship = 0) {
    
    age_mat <- as.integer(a)
    mat     <- c(rep(0, age_mat), rep(1, NAGES - age_mat))
    S       <- exp(-c(rep(sqrt(M), age_mat), rep(M, NAGES - age_mat)))    
    
    if (cv_survivorship > 0) {
        
        # process error term
        sigma <- cv_survivorship * S
        
        # calculate mu given sigma
        mu_calc <- function(survivorship, sigma) uniroot(function(mu) survivorship - pnorm(mu / sqrt(1 + sigma^2)), interval = c(-10, 10))$root
        
        mu <- numeric(NAGES)
        for (a in 1:NAGES) {
            mu[a] <- mu_calc(S[a], sigma[a])
        }
        
        s <- array(dim = c(SITER, NAGES, NTIME))
        
        for (a in 1:NAGES) {
            
            e <- rnorm(SITER * NTIME, mu[a], sigma[a])
            
            s[,a,] <- pnorm(e)
            
            # first year is
            # equal to expectation
            s[,a,1] <- S[a]
        }
        
    } else {
    
        s <- array(dim = c(NAGES, NTIME))
        for (a in 1:NAGES) {
            s[a,] <- S[a]
        }    
    }
    
    return(s)
}
