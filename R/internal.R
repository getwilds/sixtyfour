#' Get account ID of current user
#' @keywords internal
#' @details If env var `AWS_PROFILE` == "localstack",
#' return `"000000000000"`
#' @return list with 3 elements:
#' - UserId: the ID for the user
#' - Account: account ID the user is in
#' - Arn: arn for the user
account_id <- memoise::memoise(function() {
  if (Sys.getenv("AWS_PROFILE") == "localstack") {
    return("000000000000")
  }
  paws::sts()$get_caller_identity()$Account
})

#' Get bucket region
#' @keywords internal
#' @return character string of bucket region; NULL if bucket not found
bucket_region <- function(bucket) {
  res <- tryCatch(
    con_s3()$get_bucket_location(bucket),
    error = function(e) e
  )
  if (rlang::is_error(res)) NULL else res$LocationConstraint
}

#' Get bucket ARN
#' @export
#' @param bucket (character) a bucket name. required.
#' @param objects (character) path for object(s). default: `""`
#' @return character string of bucket arn
#' @examples
#' bucket_arn("somebucket")
#' bucket_arn("somebucket", objects = "*")
#' bucket_arn("somebucket", objects = "data.csv")
#' bucket_arn("somebucket", objects = "myfolder/subset/data.csv")
#' bucket_arn("somebucket", objects = "myfolder/subset/*")
bucket_arn <- function(bucket, objects = "") {
  stop_if_not(rlang::is_character(bucket), "bucket must be character")
  stop_if_not(rlang::is_character(objects), "objects must be character")
  glue("arn:aws:s3:::{bucket}",
    "{ifelse(nzchar(objects), paste0('/', objects), '')}")
}
