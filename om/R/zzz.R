.onAttach <- function(libname, pkgname) {
    packageStartupMessage("om version 0.1.8 (28-May-2026)")
}
 
.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("rlang", "dplyr"),
        requireNamespace, quietly = TRUE)
  ))
}
