env64 <- rlang::env()

#' Configure sixtyfour settings
#'
#' @export
#' @param redacted (logical) Redact secrets? Default: `FALSE`. If `TRUE`,
#' secret values are redacted (replaced with `*****`) only in printed
#' messages to the console, and are not redacted in the output from
#' running functions.
#' @param redact_str (character) String to use to replace redacted values.
#' Default: `"*****"`
#' @param verbose (logical) Print verbose output? Default: `TRUE`.
aws_configure <- function(
    redacted = FALSE, redact_str = "*****",
    verbose = TRUE) {
  env64$redacted <- redacted
  env64$redact_str <- redact_str
  env64$verbose <- verbose
  structure(env64, class = "aws_settings")
}

#' @export
print.aws_settings <- function(x, ...) {
  cat("aws_settings:\n")
  print(glue("  Redacted: {x$redacted}"))
  print(glue("  Redact string: {x$redact_str}"))
  print(glue("  Verbose: {x$verbose}"))
}

#' Without verbose output
#' @export
#' @importFrom rlang env_poke
#' @importFrom withr defer
#' @param code (expression) Code to run without verbose output.
without_verbose <- function(code) {
  rlang::env_poke(env64, "verbose", FALSE)
  withr::defer(rlang::env_poke(env64, "verbose", TRUE))
  force(code)
}

#' Without secrets redacted
#' @export
#' @param code (expression) Code to run with secrets redacted
with_redacted <- function(code) {
  rlang::env_poke(env64, "redacted", TRUE)
  withr::defer(rlang::env_poke(env64, "redacted", FALSE))
  force(code)
}
