.onAttach <- function(libname, pkgname) {
    packageStartupMessage("om version 0.0.1 (25-Feb-2026)")
}
 
.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("rlang", "dplyr"),
        requireNamespace, quietly = TRUE)
  ))
}
