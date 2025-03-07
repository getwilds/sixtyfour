capfirst <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Get a random user or role
#'
#' @export
#' @return (character) a username or role name with a random adjective plus a
#' random noun combined into one string, shortened to no longer than 16
#' characters if longer than 16
#' @examples
#' random_user()
#' random_role()
#' replicate(10, random_user())
random_user <- function() {
  sample_upcase <- function(x) capfirst(sample(x, size = 1))
  substring(paste0(sample_upcase(adjectives), sample_upcase(nouns)), 1, 16)
}
#' @export
#' @rdname random_user
random_role <- random_user

#' Get a random bucket name
#'
#' @export
#' @inheritParams random_string
#' @return (character) a bucket name prefixed with `prefix`
#' (default: "bucket-")
#' @examples
#' random_bucket()
#' replicate(10, random_bucket())
random_bucket <- function(prefix = "bucket-", length = 16) {
  random_string(prefix, length)
}

#' Get a random string with prefix
#'
#' @export
#' @param prefix (character) any string. required.
#' @param size (character) length of the random part (not including
#' `prefix`)
#' @return (character) a string with `prefix` at beginning
#' @examples
#' random_string("bucket")
#' replicate(10, random_string("bucket"))
random_string <- function(prefix, size = 8) {
  glue::glue(
    "{prefix}{paste0(sample(letters, size = size), collapse = '')}"
  )
}
