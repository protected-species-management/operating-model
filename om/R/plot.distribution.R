#' @title Plot function for distribution class
#' @exportS3Method base::plot
plot.distribution <- function(x, y = "missing", ...) {
    
    pars   <- x@pars
    dist   <- x@distribution
    values <- x@.Data
    
    stopifnot(length(x@.Data) > 0)
    
    if (grepl("^uniform", dist)) curve(dunif(x, pars[1], pars[2]), from = min(values), to = max(values), col = 2, yaxt = "n", xlab = "", ylab = "uniform density", lwd = 2)
    if (grepl("^normal", dist)) curve(dnorm(x, pars[1], pars[2]), from = min(values), to = max(values), col = 2, yaxt = "n", xlab = "", ylab = "normal density", lwd = 2)
    if (grepl("^log?normal", dist)) curve(dlnorm(x, pars[1], pars[2]), from = min(values), to = max(values), col = 2, yaxt = "n", xlab = "", ylab = "log-normal density", lwd = 2)
    if (grepl("^gamma", dist)) curve(dgamma(x, shape = pars[1], scale = pars[2]), from = min(values), to = max(values), col = 2, yaxt = "n", xlab = "", ylab = "Gamma density", lwd = 2)
    
    hist(values, probability = TRUE, add = TRUE, col = NA)
}

