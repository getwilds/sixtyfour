purge_secrets <- function() {
  x <- aws_secrets_list()
  if (!is_empty(x$SecretList)) {
    x$SecretList %>%
      purrr::map_vec("Name") %>%
      purrr::map(aws_secrets_delete, ForceDeleteWithoutRecovery = TRUE)
  }
}

running_local_only_tests <- function() {
  as.logical(Sys.getenv("SIXTYFOUR_RUN_LOCAL_ONLY_TESTS", FALSE))
}
