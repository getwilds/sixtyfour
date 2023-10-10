#' Upload a file
#'
#' @export
#' @importFrom fs file_exists
#' @param bucket (character) an S3 bucket. required
#' @param path (character) a file path to read from or write to. required
#' @param key (character) a key for an object in an S3 `bucket`. required
#' @param ... named parameters passed on to [put_object](https://www.paws-r-sdk.com/docs/s3_put_object/)
#' @details Wraps [put_object](https://www.paws-r-sdk.com/docs/s3_put_object/)
#' @return a tibble with two columns and many rows
#' @details `bucket` parameter:
#' - For upload: if it does exist it will be created
#' - For download: if it does not exist, function will return an error
#' @examples \dontrun{
#' desc_file <- file.path(system.file(), "DESCRIPTION")
#' aws_file_upload(bucket = "s64-test-2", path = desc_file)
#'
#' # supply a different key
#' aws_file_upload(bucket = "s64-test-2", path = desc_file, key = "d_file")
#'
#' # set expiration, expire 1 minute from now
#' aws_file_upload(bucket = "s64-test-2", path = desc_file, key = "ddd",
#' Expires = Sys.time() + 60)
#'
#' # bucket doesn't exist
#' aws_file_upload(bucket = "not-a-bucket", path = desc_file)
#' # path doesn't exist
#' aws_file_upload(bucket = "s64-test-2", path = "file_doesnt_exist.txt")
#' }
aws_file_upload <- function(bucket, path, key = basename(path), ...) {
  stopifnot(fs::file_exists(path))
  if (!aws_bucket_exists(bucket)) aws_bucket_create(bucket)
  env64$s3$put_object(Body = path, Bucket = bucket, Key = key, ...) %>%
    tibble_transpose()
}

#' Download a file
#'
#' @export
#' @inheritParams aws_file_upload
#' @param ... named parameters passed on to [download_file](https://www.paws-r-sdk.com/docs/s3_download_file/)
#' @details Wraps [download_file](https://www.paws-r-sdk.com/docs/s3_download_file/)
#' @return `list` of length 0
#' @examples \dontrun{
#' temp_path <- tempfile()
#' aws_file_download(bucket = "s64-test-2", key = "DESCRIPTION",
#' path = temp_path)
#'
#' # S3 key doesn't exist
#' aws_file_download(bucket = "s64-test-2", key = "TESTING123",
#' path = temp_path)
#' }
aws_file_download <- function(bucket, key, path, ...) {
  env64$s3$download_file(Bucket = bucket, Key = key, Filename = path, ...)
}

#' File attributes
#'
#' @export
#' @inheritParams aws_file_upload
#' @param ... named parameters passed on to [head_object](https://www.paws-r-sdk.com/docs/s3_head_object/)
#' @return `list` of length 0
#' @examples \dontrun{
#' aws_file_attr(bucket = "s64-test-2", key = "DESCRIPTION")
#' aws_file_attr(bucket = "s64-test-2", key = "ddd")
#' aws_file_attr(bucket = "s64-test-2", key = "doesntexist")
#' }
aws_file_attr <- function(bucket, key, ...) {
  env64$s3$head_object(Bucket = bucket, Key = key, ...)
}

#' Check if a file exists
#'
#' @export
#' @inheritParams aws_file_upload
#' @return TRUE or FALSE
#' @examples \dontrun{
#' aws_file_exists(bucket = "s64-test-2", key = "DESCRIPTION")
#' aws_file_exists(bucket = "s64-test-2", key = "doesntexist")
#' }
aws_file_exists <- function(bucket, key, ...) {
  res <- paws_handlr(aws_file_attr(bucket, key, ...))
  !inherits(res, c("error", "error_response"))
}
