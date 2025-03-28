paws_handlr <- function(...) {
  tryCatch(..., error = function(e) e)
}

# Check for an env var; stop with message if not found
env_var <- function(env_name) {
  x <- Sys.getenv(env_name, "")
  stop_msg <- sprintf("Environment variable '%s' not found", env_name)
  if (identical(x, "")) stop(stop_msg)
  x
}

#' lifted directly from the devtools package within the file (MIT licensed):
#' https://github.com/r-lib/devtools/blob/main/R/release.R
#' @importFrom cli cli_inform
#' @noRd
#' @keywords internal
yesno <- function(msg, .envir = parent.frame()) {
  yeses <- c(
    "Yes",
    "Definitely",
    "For sure",
    "Yup",
    "Yeah",
    "Of course",
    "Absolutely"
  )
  nos <- c(
    "No way",
    "Not yet",
    "I forget",
    "No",
    "Nope",
    "Uhhhh... Maybe?"
  )

  cli::cli_inform(msg, .envir = .envir)
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  utils::menu(qs[rand]) != which(rand == 1)
}

#' Get the first element of a vector
#' @keywords internal
#' @param x a vector
#' @return the first element of the vector
first <- function(x) x[1]

#' Get the last element of a vector
#' @keywords internal
#' @param x a vector
#' @return the last element of the vector
last <- function(x) x[length(x)]

check_for_pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop(sprintf("Please install '%s'", x), call. = FALSE)
  } else {
    invisible(TRUE)
  }
}

#' Parse s3 paths
#'
#' @keywords internal
#' @param paths (character) one or more s3 paths
#' @return an unnamed list with each slot a named list with bucket, path,
#' and file
path_s3_parse <- function(paths) {
  list_names <- c("bucket", "path", "file")
  stopifnot("One or more paths are not s3 paths" = all(grepl("^s3://", paths)))
  paths <- gsub("s3://", "", paths)
  paths <- strsplit(paths, "/")
  Map(
    function(x) {
      if (length(x) > 2) {
        x <- c(x[1], paste(x[-c(1, length(x))], collapse = "/"), last(x))
        as.list(stats::setNames(x, list_names))
      } else {
        as.list(stats::setNames(c(x[1], "", last(x)), list_names))
      }
    },
    paths
  )
}

#' Build s3 paths
#'
#' @keywords internal
#' @param x unnamed list of parsed paths, from [path_s3_parse()]
path_s3_build <- function(x) {
  purrr::map_vec(x, function(w) {
    path <- if (nzchar(w$path)) paste0(w$path, "/") else ""
    prefix <- if (grepl("s3://", w$bucket)) "" else "s3://"
    glue::glue("{prefix}{w$bucket}/{path}{w$file}")
  }) %>%
    as.character()
}

#' Convert a s3 like path to a single format
#'
#' @keywords internal
#' @inheritParams path_s3_parse
#' @return vector of s3 paths (character), Of the form:
#' `s3://<bucket>/<path>/<file>`
path_as_s3 <- function(paths) {
  paths <- gsub("https?://", "", paths)
  paths <- gsub("\\.s3.+", "", paths)
  sprintf("s3://%s", paths)
}

#' Paginate over list_* methods with Marker/IsTruncated
#'
#' Currently works for IAM only - i.e., IAM is hard-coded internally
#'
#' @importFrom purrr map flatten
#' @importFrom rlang has_name
#' @param fun (character) the name of a function to call - not the function
#' itself
#' @param target (character) a list element to get
#' @param ... named args passed on to `fun`
#' @keywords internal
paginate_aws_marker <- function(fun, target, ...) {
  con <- con_iam()
  res <- con[[fun]](...)
  if (!rlang::has_name(res, "IsTruncated")) {
    return(res[[target]])
  }
  if (!res$IsTruncated) {
    return(res[[target]])
  }

  all_results <- list(res)
  more_results <- TRUE
  while (more_results) {
    res <- con[[fun]](Marker = res$Marker, ...)
    all_results <- c(all_results, list(res))
    if (!res$IsTruncated) more_results <- FALSE
  }
  purrr::map(all_results, \(x) x[[target]]) %>% purrr::flatten()
}

#' Paginate over list_* methods with NextToken
#'
#' @importFrom rlang is_empty
#' @inheritParams paginate_aws_marker
#' @keywords internal
paginate_aws_token <- function(fun, target, ...) {
  con <- con_sm()
  res <- con[[fun]](...)
  if (!rlang::has_name(res, "NextToken")) {
    return(res[[target]])
  }
  if (rlang::is_empty(res$NextToken)) {
    return(res[[target]])
  }

  all_results <- list(res)
  more_results <- TRUE
  while (more_results) {
    res <- con[[fun]](NextToken = res$NextToken)
    all_results <- c(all_results, list(res))
    if (rlang::is_empty(res$NextToken)) more_results <- FALSE
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
      mutate(CreateDate = .as_datetime(CreateDate))
  }
}

# replaces lubridate::as_datetime
.as_datetime <- function(x) {
  as.POSIXct(x, origin = "1970-01-01 UTC", tz = "UTC")
}

is_class <- function(x, class) {
  if (is.null(x)) {
    return(invisible())
  }
  if (!inherits(x, class)) {
    stop(glue("`{substitute(x)}` should be class {class}"), call. = FALSE)
  }
}

stop_if_not <- function(cond, msg, .envir = parent.frame()) {
  if (!cond) cli::cli_abort(msg, .envir = .envir)
}
stop_if <- function(cond, msg, .envir = parent.frame()) {
  if (cond) cli::cli_abort(msg, .envir = .envir)
}

#' Check if appropriate AWS credentials are available
#' @export
#' @importFrom paws.common locate_credentials
#' @return single boolean
#' @examples
#' aws_has_creds()
aws_has_creds <- function() {
  res <- tryCatch(
    paws.common::locate_credentials(),
    error = \(e) e
  )
  !inherits(res, "error")
}
