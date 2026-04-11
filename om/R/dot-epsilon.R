
.epsilon <- function(x = 1, cv) {
    exp(log(x / sqrt(1 + cv^2)) + rnorm(1) * sqrt(log(1 + cv^2)))
}
