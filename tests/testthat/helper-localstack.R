LOCALSTACK_ENDPOINT <- "http://localhost:4566" # nolint

localstack_available <- function() {
  curl_check <- purrr::safely(curl::curl_fetch_memory, FALSE)
  is.null(curl_check(LOCALSTACK_ENDPOINT)$error)
}
