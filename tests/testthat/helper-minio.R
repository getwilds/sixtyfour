bucket_delete <- function(bucket, force = FALSE) {
  if (!aws_bucket_exists(bucket)) return()
  list_obs <- purrr::safely(aws_bucket_list_objects)
  objects <- list_obs(bucket)
  if (NROW(objects$result) > 0) {
    purrr::map(objects$result$uri, \(x) aws_file_delete(x))
  }
  aws_bucket_delete(bucket, force = force)
}

buckets_empty <- function() {
  buckets <- aws_buckets()
  if (NROW(buckets) > 0) {
    invisible(purrr::map(buckets$bucket_name, bucket_delete, force = TRUE))
  }
}

minio_available <- function() {
  curl_check <- purrr::safely(curl::curl_fetch_memory, FALSE)
  is.null(curl_check("http://127.0.0.1:9000")$error)
}
