LOCALSTACK_ENDPOINT <- "http://localhost.localstack.cloud:4566"

localstack_available <- function() {
  curl_check <- purrr::safely(curl::curl_fetch_memory, FALSE)
  is.null(curl_check(LOCALSTACK_ENDPOINT)$error)
}

# iam_ls = paws::iam(endpoint = endpoint_url)
