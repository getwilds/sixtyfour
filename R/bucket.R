#' Create an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @note internally uses [head_bucket](https://www.paws-r-sdk.com/docs/s3_head_bucket/)
#' @examples \dontrun{
#' # exists
#' aws_bucket_exists(bucket="s64-test-2")
#' # does not exist
#' aws_bucket_exists(bucket="no-bucket")
#' }
aws_bucket_exists <- function(bucket) {
  res <- tryCatch({
    env64$s3$head_bucket(Bucket = bucket)
  }, error = function(e) e)
  !inherits(res, c("error", "error_response"))
}

#' Create an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to [list_objects](https://www.paws-r-sdk.com/docs/s3_create_bucket/)
#' @note Requires the env var `AWS_REGION`
#' @examples \dontrun{
#' aws_bucket_create(bucket="s64-test-2")
#' }
aws_bucket_create <- function(bucket, ...) {
  env64$s3$create_bucket(Bucket = bucket,
    CreateBucketConfiguration = list(LocationConstraint = env_var("AWS_REGION")), ...)
}

#' List objects in an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to [list_objects](https://www.paws-r-sdk.com/docs/s3_list_objects/)
#' @examples \dontrun{
#' aws_bucket_list_objects(bucket="s64-test-2")
#' }
aws_bucket_list_objects <- function(bucket, ...) {
  env64$s3$list_objects(Bucket = bucket, ...)
}

#' List S3 buckets
#'
#' @export
#' @param ... named parameters passed on to [list_buckets](https://www.paws-r-sdk.com/docs/s3_list_buckets/)
#' @return tibble with zero or more rows (each an S3 bucket), with two columns:
#' * Name (character)
#' * CreationDate (dttm)
#' @autoglobal
#' @examples \dontrun{
#' aws_buckets()
#' }
aws_buckets <- function(...) {
  env64$s3$list_buckets(...) %>% .$Buckets %>% map(., as_tibble) %>% list_rbind()
}
