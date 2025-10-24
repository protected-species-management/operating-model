.onAttach <- function(libname, pkgname) {
    packageStartupMessage("om version 0.0.1 (24-Oct-2025)")
}
 
.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("rlang", "dplyr"),
        requireNamespace, quietly = TRUE)
  ))
}
