LOCALSTACK_ENDPOINT <- "http://localhost.localstack.cloud:4566"

#' Set client
#' @param testing (logical) whether to set iam client for testing.
#' default: `FALSE`
#' @keywords internal
set_iam_client <- function(testing = FALSE) {
	env64$iam <- if (testing) {
		Sys.setenv("AWS_ACCESS_KEY_ID" = "foobar")
		Sys.setenv("AWS_SECRET_ACCESS_KEY" = "foobar")
		# Sys.unsetenv("AWS_REGION")
		paws::iam(endpoint = LOCALSTACK_ENDPOINT)
	} else {
		paws::iam()
	}
}
