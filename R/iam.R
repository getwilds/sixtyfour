LOCALSTACK_ENDPOINT <- "http://localhost.localstack.cloud:4566"

#' Get the `paws` Identity and Access Management (IAM) client
#'
#' @export
#' @param testing (logical) whether to set iam client for testing
#' default: `FALSE`
#' @return a list with methods for interfacing with IAM;
#' see <https://www.paws-r-sdk.com/docs/iam/>
#' @examples aws_iam_client()
aws_iam_client <- function(testing = Sys.getenv("AWS_PROFILE", "default")) {
  # testing <- as.logical(testing)
  if (is.null(env64$iam)) env64$iam <- paws::iam()
  if (testing == "localstack") {
    print("Using testing")
    # Sys.setenv("AWS_ACCESS_KEY_ID" = "foobar")
    # Sys.setenv("AWS_SECRET_ACCESS_KEY" = "foobar")
    # Sys.unsetenv("AWS_ACCESS_KEY_ID")
    # Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
    # Sys.unsetenv("AWS_REGION")
    # env64$iam <- paws::iam(endpoint = LOCALSTACK_ENDPOINT)
    Sys.setenv(AWS_PROFILE = "localstack")

    # env64$iam <-
    #   paws.common::set_config(
    #     env64$iam,
    #     list(endpoint = LOCALSTACK_ENDPOINT)
    #   )
    # env64$iam <-
    #   paws.common::set_config(
    #     env64$iam,
    #     list(credentials = list(profile = "/Users/schambe3/github/getwilds/sixtyfour/tests/testthat/aws_ini"))
    #   )
    env64$iam <- paws::iam(
      config = list(
        credentials = list(
          profile = "localstack"
        )
      )
    )
  }
  env64$iam
}

get_iam <- function() {
  profile <- Sys.getenv("AWS_PROFILE")
  if (profile == "localstack") {
    # paws::iam(
    #   config = list(
    #     credentials = list(
    #       profile = "localstack"
    #     )
    #   )
    # )
    Sys.unsetenv("AWS_ACCESS_KEY_ID")
    Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
    paws::iam(
      credentials = list(
        creds = list(
          access_key_id = "NOTAREALKEY",
          secret_access_key = "faketoken"
        )
      ),
      endpoint = LOCALSTACK_ENDPOINT
    )
  } else {
    paws::iam()
  }
}
