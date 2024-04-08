# cli_alert* with wrap=TRUE
cli_info <- function(text, .envir = parent.frame()) {
  cli_alert_info(text, wrap = TRUE, .envir = .envir)
}
#' @importFrom cli cli_alert_warning
cli_warning <- function(text, .envir = parent.frame()) {
  cli_alert_warning(text, wrap = TRUE, .envir = .envir)
}
cli_success <- function(text, .envir = parent.frame()) {
  cli_alert_success(text, wrap = TRUE, .envir = .envir)
}
suppm <- function(x) {
  suppressMessages(x)
}
