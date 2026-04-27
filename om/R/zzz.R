.onAttach <- function(libname, pkgname) {
    packageStartupMessage("om version 0.1.5 (27-Apr-2026)")
}
 
.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("rlang", "dplyr"),
        requireNamespace, quietly = TRUE)
  ))
}
