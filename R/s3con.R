LOCALSTACK_ENDPOINT <- "http://localhost.localstack.cloud:4566" # nolint

#' Get the `paws` S3 client
#' @return a list with methods for interfacing with IAM;
#' see <https://www.paws-r-sdk.com/docs/s3/>
#' @keywords internal
con_s3 <- function() {
  set_s3_interface(Sys.getenv("AWS_PROFILE", "aws"))
}

#' s3fs connection
#' @examplesIf interactive()
#' con <- con_s3fs()
#' file_copy <- con_s3fs()$file_copy
con_s3fs <- function() {
  profile <- Sys.getenv("AWS_PROFILE")
  if (profile == "minio") {
    s3fs::s3_file_system(
      aws_access_key_id = Sys.getenv("MINIO_USER"),
      aws_secret_access_key = Sys.getenv("MINIO_PWD"),
      endpoint = Sys.getenv("MINIO_ENDPOINT"),
      refresh = TRUE
    )
  } else if (profile == "localstack") {
    s3fs::s3_file_system(
      aws_access_key_id = "NOTAREALKEY",
      aws_secret_access_key = "AREALLYFAKETOKEN",
      endpoint = LOCALSTACK_ENDPOINT,
      refresh = TRUE
    )
  } else {
    s3fs::s3_file_system(
      aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      region_name = Sys.getenv("AWS_REGION"),
      refresh = TRUE
    )
  }
}
