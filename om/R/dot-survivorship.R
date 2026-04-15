
.survivorship <- function(M, cv_survivorship = 0, env) {
		
	NTIME <- get("NTIME", envir = env)
	
	S <- exp(-M)
	
	if (cv_survivorship > 0) {
		
		SITER <- get("SITER", envir = env)
		
		# process error term
		sigma <- cv_survivorship * S
		
		# calculate mu given sigma
		mu <- uniroot(function(x) S - pnorm(x / sqrt(1 + sigma^2)), interval = c(-10, 10))$root
		
		s <- array(dim = c(SITER, NTIME))
		e <- rnorm(SITER * NTIME, mu, sigma)
			
		s[] <- pnorm(e)
			
		# first year is
		# equal to expectation
		s[,1] <- S
		
	} else {
	
		s   <- array(dim = c(NTIME))
		s[] <- S
	}
	
	return(s)
}
