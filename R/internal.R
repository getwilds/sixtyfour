#' Get account ID of current user
#' @keywords internal
#' @return list with 3 elements:
#' - UserId: the ID for the user
#' - Account: account ID the user is in
#' - Arn: arn for the user
account_id <- function() {
  paws::sts()$get_caller_identity()$Account
}

#' Get bucket region
#' @keywords internal
#' @return character string of bucket region; NULL if bucket not found
bucket_region <- function(bucket) {
  res <- tryCatch(
    env64$s3$get_bucket_location(bucket),
    error = function(e) e
  )
  if (rlang::is_error(res)) NULL else res$LocationConstraint
}

#' Get bucket ARN
#' @keywords internal
#' @return character string of bucket arn
bucket_arn <- function(bucket) {
  glue("arn:aws:s3:::{bucket}")
}