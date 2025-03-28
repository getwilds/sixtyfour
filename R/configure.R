env64 <- rlang::env()

#' Configure sixtyfour settings
#'
#' @export
#' @param redacted (logical) Redact secrets? Default: `FALSE`. If `TRUE`,
#' secret values are redacted (replaced with `redact_str`) in certain
#' messages and output from functions. See *What is Redacted* below.
#' @param redact_str (character) String to use to replace redacted values.
#' Default: `"*****"`
#' @param verbose (logical) Print verbose output? Default: `TRUE`. Applies
#' only to `cli::cli_alert_info()`, `cli::cli_alert_warning()`, and
#' `cli::cli_alert_success()` functions that are used throughout this package.
#' There's still a few places where `verbose` may not be respected.
#' @return S3 class `aws_settings`
#' @section What is Redacted:
#' What's redacted is currently hard-coded in the package. There's only
#' certain functions and certain elements in the output of those functions
#' that are redacted. The following is what's redacted with
#' `aws_configure(redacted = TRUE)` or `with_redacted()`:
#'
#' - `aws_whoami()`: AWS Account ID via `account_id()`
#' - `six_user_creds()`: Access Key ID
#' - groups functions:
#'   - functions: `aws_groups()`, `aws_group()`, `aws_group_create()`
#'   - attribute: `Arn` (includes AWS Account ID)
#' - roles functions:
#'   - functions: `aws_roles()`, `aws_role()`, `aws_role_create()`
#'   - attribute: `Arn` (includes AWS Account ID)
#' - user functions:
#'   - functions: `aws_users()`, `aws_user()`, `aws_user_create()`,
#' `aws_user_add_to_group()`, `aws_user_remove_from_group()`
#'   - attribute: `Arn` (includes AWS Account ID)
#' - `aws_user_access_key_delete()`: Access Key ID
aws_configure <- function(
  redacted = FALSE,
  redact_str = "*****",
  verbose = TRUE
) {
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
#' @return The results of the evaluation of the code argument
without_verbose <- function(code) {
  rlang::env_poke(env64, "verbose", FALSE)
  withr::defer(rlang::env_poke(env64, "verbose", TRUE))
  force(code)
}

#' With secrets redacted
#' @export
#' @param code (expression) Code to run with secrets redacted
#' @return The results of the evaluation of the code argument
with_redacted <- function(code) {
  rlang::env_poke(env64, "redacted", TRUE)
  withr::defer(rlang::env_poke(env64, "redacted", FALSE))
  force(code)
}
