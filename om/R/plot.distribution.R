#' @title Plot function for distribution class
#' @exportS3Method base::plot
plot.distribution <- function(x, y = "missing", ...) {
    
    pars   <- x@pars
    dist   <- x@distribution
    values <- x@.Data
    
    if (grepl("^uniform", dist)) curve(dunif(x, pars[1], pars[2]), from = pars[1], to = pars[2], col = 2, yaxt = "n", xlab = "", ylab = "uniform density", lwd = 2)
    if (grepl("^normal", dist)) curve(dnorm(x, pars[1], pars[2]), from = pars[1] - 3 * pars[2], to = pars[1] + 3 * pars[2], col = 2, yaxt = "n", xlab = "", ylab = "normal density", lwd = 2)
    if (grepl("^log?normal", dist)) curve(dlnorm(x, pars[1], pars[2]), from = exp(pars[1] - 3 * pars[2]), to = exp(pars[1] + 3 * pars[2]), col = 2, yaxt = "n", xlab = "", ylab = "log-normal density", lwd = 2)
    if (grepl("^gamma", dist)) curve(dgamma(x, shape = pars[1], scale = pars[2]), from = max(0, pars[1] * pars[2] - 3 * sqrt(pars[1] * pars[2]^2)), to = pars[1] * pars[2] + 3 * sqrt(pars[1] * pars[2]^2), col = 2, yaxt = "n", xlab = "", ylab = "Gamma density", lwd = 2)
    
    if (length(values[!is.na(values)]) > 0) {
        hist(values[!is.na(values)], probability = TRUE, add = TRUE, col = NA)
    }
}

