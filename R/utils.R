paws_handlr <- function(...) {
  tryCatch(..., error = function(e) e)
}

# Check for an env var; stop with message if not found
env_var <- function(env_name) {
  x <- Sys.getenv(env_name, "")
  stop_msg <- sprintf("Environment variable '%s' not found", env_name)
  if (identical(x, "")) stop(stop_msg)
  return(x)
}

# lifted directly from the devtools package within the file (MIT licensed):
# https://github.com/r-lib/devtools/blob/main/R/release.R
yesno <- function(msg, .envir = parent.frame()) {
  yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", "Of course", "Absolutely")
  nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")

  cli::cli_inform(msg, .envir = .envir)
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  utils::menu(qs[rand]) != which(rand == 1)
}

#' Get the last element of a vector
#' @keywords internal
#' @param x a vector
#' @return the last element of the vector
last <- function(x) x[length(x)]

#' Parse s3 paths
#'
#' @keywords internal
#' @param paths (character) one or more s3 paths
#' @return an unnamed list with each slot a named list with bucket, path, and file
#' @examplesIf interactive()
#' path_s3_parser("s3://s64-test-2/DESCRIPTION")
#' path_s3_parser("s3://s64-test-2/some/other/path/things.csv")
#' paths <- c(
#'  "s3://s64-test-2/DESCRIPTION",
#'  "s3://s64-test-2/stuff.txt",
#'  "s3://s64-test-2/some/other/path/things.csv"
#' )
#' path_s3_parser(paths)
#'
#' # if a path is not an s3 path
#' paths <- c(
#'  "s3://s64-test-2/DESCRIPTION",
#'  "s3://s64-test-2/stuff.txt",
#'  "s64-test-2/some/other/path/things.csv"
#' )
#' path_s3_parser(paths)
path_s3_parser <- function(paths) {
  list_names <- c("bucket", "path", "file")
  stopifnot("One or more paths are not s3 paths" = all(grepl("^s3://", paths)))
  paths <- gsub("s3://", "", paths)
  paths <- strsplit(paths, "/")
  Map(function(x) {
    if (length(x) > 2) {
      x <- c(x[1], paste(x[-c(1, length(x))], collapse="/"), last(x))
      as.list(stats::setNames(x, list_names))
    } else {
      as.list(stats::setNames(c(x[1], "", last(x)), list_names))
    }
  }, paths)
}

check_for_pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop(sprintf("Please install '%s'", x), call. = FALSE)
  } else {
    invisible(TRUE)
  }
}
