capfirst <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Get a random user
#'
#' @export
#' @return (character) a username with a random adjective plus a
#' random noun combined into one string, shortened to no longer than 16
#' characters if longer than 16
#' @examples
#' random_user()
#' replicate(10, random_user())
random_user <- function() {
  sample_upcase <- function(x) capfirst(sample(x, size = 1))
  substring(paste0(sample_upcase(adjectives), sample_upcase(nouns)), 1, 16)
}
