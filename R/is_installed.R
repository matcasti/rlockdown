#' Check if a package is installed (in the terminal)
#'
#' #' `r lifecycle::badge("experimental")`
#'
#' @param pkg Character indicating the package to search.
#'
#' @examples
#' is_installed("npm")
#' is_installed("brew")
#'
#' @return Logical of length one (i.e., TRUE or FALSE).
#'
#' @export

is_installed <- function(pkg = NULL) {
  if (missing(pkg)) stop("`pkg` MUST be specified", call. = FALSE)

  ## Valid system() syntax
  pkg <- paste("which", pkg)

  ## Call terminal to check if package is installed
  call <- suppressWarnings(system(command = pkg, intern = TRUE))

  is_installed <- isTRUE(length(call) == 1)

  return(is_installed)
}

