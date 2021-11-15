#' Encrypt HTML files
#'
#' `r lifecycle::badge("experimental")`
#'
#' Add a layer of protection to your HTML files. `protect_html()`
#' adds a password request layer before entering the final html file.
#'
#'
#' @param html The address to an HTML file.
#' @param password The password used to protect the file.
#' @param output The address to an HTML file. If exists, the function will overwrite it.
#' @param title Title for the password box.
#' @param instructions Instructions for end user. Could be guidelines related to the password it self.
#'
#' @importFrom processx run
#'
#' @section About the algorithm:
#'
#' This uses under the hood the package staticrypt ([see GitHub repo](https://github.com/robinmoisson/staticrypt)),
#' from [Robin Moisson](https://github.com/robinmoisson). For extra documentation about the JavaScript code see the
#' [repository](https://github.com/robinmoisson) from his GitHub.
#'
#' @export

protect_html <- function(html, password, output = NULL, title = NULL, instructions = NULL) {


  if (missing(html)) stop("`html` MUST be specified", call. = FALSE)
  if (missing(password)) stop("`password` MUST be specified", call. = FALSE)


  if (!file.exists(html)) stop("`html` MUST be a valid file path", call. = FALSE)


  if (!is_installed("staticrypt")) {

    ## Instalamos staticrypt, generando sus dependencias
    warning("staticrypt will be installed", call. = FALSE)
    processx::run(
      command = "npm",
      args = c("install", "staticrypt")
    )
  }

  html <- fs::path_abs(html)

  ## Make call for terminal
  call <- paste(html, password)

  ## Modify output directory and/or filename
  if (!is.null(output)) {
    output <- fs::path_abs(output)
    output <- c("-o", output)
  }

  ## Modify output directory and/or filename
  if (!is.null(title)) {
    title <- c("-t", title)
  }

  ## Add instructions to user side
  if (!is.null(instructions)) {
    instructions <- c("-i", instructions)
  }

  ## Run command
  out <- processx::run(
    command = "staticrypt",
    args = c(html, password, output, title, instructions)
  )

  return(out$status)
}
