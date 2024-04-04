LOCALSTACK_ENDPOINT <- "http://localhost.localstack.cloud:4566" # nolint

#' Get the `paws` Identity and Access Management (IAM) client
#' @return a list with methods for interfacing with IAM;
#' see <https://www.paws-r-sdk.com/docs/iam/>
#' @keywords internal
con_iam <- function() {
  profile <- Sys.getenv("AWS_PROFILE")
  if (profile == "localstack") {
    Sys.unsetenv("AWS_ACCESS_KEY_ID")
    Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
    paws::iam(
      credentials = list(
        creds = list(
          access_key_id = "NOTAREALKEY",
          secret_access_key = "AREALLYFAKETOKEN"
        )
      ),
      endpoint = LOCALSTACK_ENDPOINT
    )
  } else {
    paws::iam()
  }
}
