#' Encrypt HTML files
#'
#' `r lifecycle::badge("experimental")`
#'
#' Add a layer of protection to your HTML files. `protect_html()`
#' adds a password request layer before entering the final html file.
#'
#' @param html The address to an HTML file.
#' @param password The password used to protect the file.
#' @param output The address to an HTML file. If exists, the function will overwrite it.
#' @param title Title for the password box.
#' @param instructions Instructions for end user. Could be guidelines related to the password it self.
#'
#' @importFrom packer npm_install
#'
#' @section About the algorith:
#'
#' This uses under the hood the package staticrypt ([see GitHub repo](https://github.com/robinmoisson/staticrypt)),
#' from [Robin Moisson](https://github.com/robinmoisson). For extra documentation about the JavaScript code see the
#' [repository](https://github.com/robinmoisson) from his GitHub
#'
#' @export

protect_html <- function(html, password, output = NULL, title = NULL, instructions = NULL) {

  if (missing(html)) stop("`html` MUST be specified", call. = FALSE)
  if (missing(password)) stop("`password` MUST be specified", call. = FALSE)

  ## Check if file exist
  if (!file.exists(html)) stop("`html` MUST be a valid file path", call. = FALSE)
  if (!is.null(output) && !file.exists(output)) stop("`output` MUST be a valid file path", call. = FALSE)

  ## Check if staticrypt is installed, if not it can be installed using npm
  if (!is_installed("staticrypt")) {
    warning("staticrypt will be installed", call. = FALSE)
    packer::npm_install("staticrypt")
  }

  html <- paste0("\"", html, "\"")

  ## Make call for terminal
  call <- paste("staticrypt", html, password)

  ## Modify output directory and/or filename
  if (!is.null(output)) {
    output <- paste0("\"", output, "\"")
    call <- paste(call, "-o", output)
  }

  ## Modify output directory and/or filename
  if (!is.null(title)) {
    title <- paste0("\"", title, "\"")
    call <- paste(call, "-t", title)
  }

  ## Add instructions to user side
  if (!is.null(instructions)) {
    instructions <- paste0("\"", instructions, "\"")
    call <- paste(call, "-i", instructions)
  }

  ## Run command
  invisible(x = system(call, intern = TRUE))
}
