purge_secrets <- function() {
  x <- aws_secrets_list()
  if (!is_empty(x$SecretList)) {
    x$SecretList %>%
      purrr::map_vec("Name") %>%
      purrr::map(aws_secrets_delete, ForceDeleteWithoutRecovery = TRUE)
  }
}

random_string <- function(prefix, size = 8) {
  glue::glue(
    "{prefix}{paste0(sample(letters, size = size), collapse = '')}"
  )
}

running_local_only_tests <- function() {
  as.logical(Sys.getenv("SIXTYFOUR_RUN_LOCAL_ONLY_TESTS", FALSE))
}
