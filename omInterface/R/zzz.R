.onAttach <- function(libname, pkgname) {
    packageStartupMessage("omInterface version 0.0.1 (16-Apr-2026)")
}
 
.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("rlang", "dplyr", "shiny", "bslib"),
        requireNamespace, quietly = TRUE)
  ))
}
