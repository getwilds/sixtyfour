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
  yeses <- c(
    "Yes", "Definitely", "For sure", "Yup",
    "Yeah", "Of course", "Absolutely"
  )
  nos <- c(
    "No way", "Not yet", "I forget", "No",
    "Nope", "Uhhhh... Maybe?"
  )

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
#' @return an unnamed list with each slot a named list with bucket, path,
#' and file
#' @examplesIf interactive()
#' path_s3_parser("s3://s64-test-2/DESCRIPTION")
#' path_s3_parser("s3://s64-test-2/some/other/path/things.csv")
#' paths <- c(
#'   "s3://s64-test-2/DESCRIPTION",
#'   "s3://s64-test-2/stuff.txt",
#'   "s3://s64-test-2/some/other/path/things.csv"
#' )
#' path_s3_parser(paths)
#'
#' # if a path is not an s3 path
#' paths <- c(
#'   "s3://s64-test-2/DESCRIPTION",
#'   "s3://s64-test-2/stuff.txt",
#'   "s64-test-2/some/other/path/things.csv"
#' )
#' path_s3_parser(paths)
path_s3_parser <- function(paths) {
  list_names <- c("bucket", "path", "file")
  stopifnot("One or more paths are not s3 paths" = all(grepl("^s3://", paths)))
  paths <- gsub("s3://", "", paths)
  paths <- strsplit(paths, "/")
  Map(function(x) {
    if (length(x) > 2) {
      x <- c(x[1], paste(x[-c(1, length(x))], collapse = "/"), last(x))
      as.list(stats::setNames(x, list_names))
    } else {
      as.list(stats::setNames(c(x[1], "", last(x)), list_names))
    }
  }, paths)
}

#' Paginate over list_* methods
#'
#' @importFrom purrr map flatten
#' @importFrom rlang has_name
#' @param fun (function) a function to call
#' @param target (character) a list element to get
#' @param ... named args passed on to `fun`
#' @keywords internal
#' @examples \dontrun{
#' # FIXME: could remove target param and poach the name of the fun
#' # e.g,. from list_roles we can get Roles
#' paginate_aws(fun = env64$iam$list_roles, target = "Roles")
#' paginate_aws(fun = env64$iam$list_policies, target = "Policies")
#' }
paginate_aws <- function(fun, target, ...) {
  res <- fun(...)
  if (!rlang::has_name(res, "IsTruncated")) return(res[[target]])
  if (!res$IsTruncated) return(res[[target]])

  all_results <- list(res)
  more_results <- TRUE
  while (more_results) {
    res <- fun(Marker = res$Marker)
    all_results <- c(all_results, list(res))
    if (!res$IsTruncated) more_results <- FALSE
  }
  purrr::map(all_results, \(x) x[[target]]) %>% purrr::flatten()
}

#' Tidy list to tibble generator
#'
#' For the functions user_list_tidy, group_list_tidy, etc.
#'
#' @importFrom purrr map list_rbind
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom lubridate as_datetime
#' @autoglobal
#' @noRd
#' @param vars (character) vector of list names to get
#' @keywords internal
tidy_generator <- function(vars) {
  function(x) {
    x %>%
      map(~ .x[vars]) %>%
      map(\(x) map(x, \(y) ifelse(length(y) < 1, NA, y))) %>%
      map(as_tibble) %>%
      list_rbind() %>%
      mutate(CreateDate = as_datetime(CreateDate))
  }
}