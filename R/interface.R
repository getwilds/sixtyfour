#' Set the S3 compatible interface to use
#'
#' @export
#' @param interface the s3 compatible interface to use.
#' options: "aws" (default), "minio", or "localstack"
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
  interfaces <- c("aws", "minio", "localstack")
  if (!interface %in% interfaces) {
    msg <- "'interface' must be one of"
    stop(glue::glue("{msg} {paste(interfaces, collapse=', ')}"))
  }

  # package paws
  if (interface == "minio") {
    paws::s3(config = list(
      credentials = list(
        creds = list(
          access_key_id = Sys.getenv("MINIO_USER"),
          secret_access_key = Sys.getenv("MINIO_PWD")
        )
      ),
      endpoint = Sys.getenv("MINIO_ENDPOINT")
    ))
  } else if (interface == "localstack") {
    paws::s3(
      credentials = list(
        creds = list(
          access_key_id = "NOTAREALKEY",
          secret_access_key = "AREALLYFAKETOKEN"
        )
      ),
      endpoint = LOCALSTACK_ENDPOINT
    )
  } else {
    paws::s3()
  }
}

#' Copy of `testthat::is_testing`
#' @noRd
#' @return single boolean
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' Refresh creds for the s3fs package
#'
#' Refreshes only if not running tests
#'
#' @keywords internal
#' @details utility function to update creds for use with any
#' `s3fs` functions. We do load creds for `s3fs` on package load
#' but if creds are changed mid-R session, then we would still be
#' using the creds used at package load time
#' @return nothing, updates creds with [s3fs::s3_file_system()]
s3fs_creds_refresh <- function() {
  if (!is_testing()) s3fs::s3_file_system(refresh = TRUE)
}
