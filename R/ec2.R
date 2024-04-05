#' Get the `paws` EC2 client
#'
#' Primarily for usage of VPC security groups
#'
#' @family security groups
#' @return a list with methods for interfacing with EC2;
#' see <https://www.paws-r-sdk.com/docs/ec2/>
#' @keywords internal
con_ec2 <- function() {
  profile <- Sys.getenv("AWS_PROFILE")
  if (profile == "localstack") {
    Sys.unsetenv("AWS_ACCESS_KEY_ID")
    Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
    paws::ec2(
      credentials = list(
        creds = list(
          access_key_id = "NOTAREALKEY",
          secret_access_key = "AREALLYFAKETOKEN"
        )
      ),
      endpoint = LOCALSTACK_ENDPOINT
    )
  } else {
    paws::ec2()
  }
}
