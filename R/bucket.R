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

#' Delete an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to [delete_bucket](https://www.paws-r-sdk.com/docs/s3_delete_bucket/)
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @return an empty list
#' @examples \dontrun{
#' aws_bucket_create(bucket="bucket-to-delete-111")
#' aws_buckets()
#' aws_bucket_delete(bucket="bucket-to-delete-111")
#' aws_buckets()
#' }
aws_bucket_delete <- function(bucket, ...) {
  # TODO: add a package level option to override the prompt for adv. users
  if (yesno("Are you sure you want to delete {.strong {bucket}}?")) {
    return(invisible())
  }
  env64$s3$delete_bucket(Bucket = bucket, ...)
}

#' Download an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param dest_path (character) destination directory to store files. required
#' @param ... named parameters passed on to [s3fs::s3_dir_download()]
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @examples \dontrun{
#' aws_bucket_create(bucket="tmp-bucket-369")
#' desc_file <- file.path(system.file(), "DESCRIPTION")
#' aws_file_upload(bucket = "tmp-bucket-369", path = desc_file)
#' aws_file_upload(bucket = "tmp-bucket-369", path = desc_file, key = "d_file")
#' temp_dir <- file.path(tempdir(), "tmp-bucket-369")
#' aws_bucket_download(bucket="tmp-bucket-369", dest_path=temp_dir)
#'
#' # cleanup
#' aws_bucket_delete("tmp-bucket-369")
#' }
aws_bucket_download <- function(bucket, dest_path, ...) {
  s3fs::s3_dir_download(path = bucket, new_path = dest_path, ...)
}

#' Upload a folder of files to create an S3 bucket
#'
#' @export
#' @importFrom fs fs_bytes
#' @param path (character) local path to a directory. required
#' @param bucket (character) bucket name. required
#' @param max_batch (fs_bytes) maximum batch size being uploaded with each
#' multipart
#' @param ... named parameters passed on to [s3fs::s3_dir_upload()]
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @examples \dontrun{
#' library(fs)
#' tdir <- path(tempdir(), "apples")
#' dir.create(tdir)
#' tfiles <- replicate(n=10, file_temp(tmp_dir = tdir, ext=".txt"))
#' invisible(lapply(tfiles, function(x) write.csv(mtcars, x)))
#'
#' aws_bucket_upload(path=tdir, bucket="a-new-bucket-345")
#' aws_bucket_list_objects("a-new-bucket-345")
#'
#' # cleanup
#' objs <- aws_bucket_list_objects("a-new-bucket-345")
#' aws_file_delete(objs$uri)
#' aws_bucket_delete("a-new-bucket-345")
#' }
aws_bucket_upload <- function(path, bucket, max_batch = fs::fs_bytes("100MB"),
  ...) {

  if (!aws_bucket_exists(bucket)) {
    if (yesno("{.strong {bucket}} does not exist. Create it?")) {
      cli::cli_inform("Exiting without uploading {.strong {basename(path)}}")
      return(invisible())
    }
    aws_bucket_create(bucket)
  }
  s3fs::s3_dir_upload(path = path, new_path = bucket,
    max_batch = max_batch)
}

#' List objects in an S3 bucket
#'
#' @export
#' @importFrom s3fs s3_dir_info
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to [s3fs::s3_dir_info()]
#' @return if no objects found, an empty tibble. if tibble has rows each
#' is an S3 bucket, with 8 columns:
#' * bucket_name (character)
#' * key (character)
#' * uri (character)
#' * size (fs::bytes)
#' * type (character)
#' * owner (character)
#' * etag (character)
#' * last_modified (dttm)
#' @examples \dontrun{
#' aws_bucket_list_objects(bucket="s64-test-2")
#' }
aws_bucket_list_objects <- function(bucket, ...) {
  out <- s3fs::s3_dir_info(bucket, ...)
  if (is.data.frame(out) && NROW(out) > 0) {
    as_tibble(out)
  } else {
    tibble()
  }
}

#' List S3 buckets
#'
#' @export
#' @importFrom s3fs s3_dir_info
#' @inherit aws_bucket_list_objects
#' @note we set `refresh=TRUE` internally to make sure we return up to date
#' information about your buckets rather than what's cached locally
#' @examples \dontrun{
#' aws_buckets()
#' }
aws_buckets <- function(...) {
  out <- s3fs::s3_dir_info(refresh = TRUE, ...)
  if (is.data.frame(out) && NROW(out) > 0) {
    as_tibble(out)
  } else {
    tibble()
  }
}

#' Print a tree of the objects in a bucket
#'
#' @export
#' @importFrom s3fs s3_dir_tree
#' @inheritParams aws_bucket_exists
#' @param recurse (logical): Returns all AWS S3 objects in lower sub directories
#' @param ... Additional arguments passed to [s3fs::s3_dir_ls()]
#' @return character vector of objects/files within the bucket,
#' printed as a tree
#' @examples \dontrun{
#' aws_bucket_tree("s3://s64-test-2")
#' }
aws_bucket_tree <- function(bucket, recurse = TRUE, ...) {
  s3fs::s3_dir_tree(bucket, recurse = recurse, ...)
}
