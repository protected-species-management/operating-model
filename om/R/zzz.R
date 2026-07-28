.onAttach <- function(libname, pkgname) {
    packageStartupMessage("om version 0.2.0 (28-Jul-2026)")
}
 
.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("rlang", "dplyr"),
        requireNamespace, quietly = TRUE)
  ))
}
