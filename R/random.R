capfirst <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Get a random string, bucket name, user name or role name
#'
#' @export
#' @param prefix (character) any string. required.
#' @param size (character) length of the random part (not including
#' `prefix`)
#' @return
#' - `random_string`: (character) a string with `prefix` at beginning
#' - `random_bucket`: (character) a bucket name prefixed with `prefix`
#' (default: "bucket-")
#' - `random_user`/`random_role`: (character) a user or role name with
#' a random adjective plus a random noun combined into one string, shortened
#' to no longer than 16 characters, if longer than 16
#' @examples
#' random_string("group-")
#' replicate(10, random_string("group-"))
#' random_bucket()
#' replicate(10, random_bucket())
#' random_user()
#' replicate(10, random_user())
#' random_role()
#' replicate(10, random_role())
random_string <- function(prefix, size = 8) {
  glue::glue(
    "{prefix}{paste0(sample(letters, size = size), collapse = '')}"
  )
}

#' @export
#' @rdname random_string
random_bucket <- function(prefix = "bucket-", size = 16) {
  random_string(prefix, size)
}

#' @export
#' @rdname random_string
random_user <- function() {
  sample_upcase <- function(x) capfirst(sample(x, size = 1))
  substring(paste0(sample_upcase(adjectives), sample_upcase(nouns)), 1, 16)
}

#' @export
#' @rdname random_string
random_role <- random_user
