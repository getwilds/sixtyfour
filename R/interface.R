#' Set the S3 compatible interface to use
#'
#' @export
#' @param interface the s3 compatible interface to use.
#' options: "aws" (default), "minio"
#' @return a `paws` s3 client object of class `list`
#' @keywords internal
#' @details
#' This function sets the connection details for both `paws` and `s3fs`
#' packages, but only returns something for `paws`
#'
#' This function expects to have access to the following environment
#' variables:
#' - `AWS_ACCESS_KEY_ID`
#' - `AWS_SECRET_ACCESS_KEY`
#' - `AWS_REGION`
#' - `MINIO_USER`
#' - `MINIO_PWD`
#' - `MINIO_ENDPOINT`
#'
#' For [s3fs::s3_file_system()] we set `refresh=TRUE` so that
#' you can change the s3 interface within an R session.
set_s3_interface <- function(interface = "aws") {
  interfaces <- c("aws", "minio")
  if (!interface %in% interfaces) {
    msg <- "'interface' must be one of"
    stop(glue::glue("{msg} {paste(interfaces, collapse=', ')}"))
  }

  # package paws
  if (interface == "aws") {
    s3con <- paws::s3()
  } else {
    s3con <- paws::s3(config = list(
      credentials = list(
        creds = list(
          access_key_id = Sys.getenv("MINIO_USER"),
          secret_access_key = Sys.getenv("MINIO_PWD")
        )
      ),
      endpoint = Sys.getenv("MINIO_ENDPOINT")
    ))
  }

  # package s3fs
  if (interface == "aws") {
    s3fs::s3_file_system(
      aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      region_name = Sys.getenv("AWS_REGION"),
      refresh = TRUE
    )
  } else {
    s3fs::s3_file_system(
      aws_access_key_id = Sys.getenv("MINIO_USER"),
      aws_secret_access_key = Sys.getenv("MINIO_PWD"),
      endpoint = Sys.getenv("MINIO_ENDPOINT"),
      refresh = TRUE
    )
  }

  return(s3con)
}
