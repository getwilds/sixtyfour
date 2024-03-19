#' Create an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @note internally uses
#' [head_bucket](https://www.paws-r-sdk.com/docs/s3_head_bucket/)
#' @family buckets
#' @examples \dontrun{
#' # exists
#' aws_bucket_exists(bucket = "s64-test-2")
#' # does not exist
#' aws_bucket_exists(bucket = "no-bucket")
#' }
aws_bucket_exists <- function(bucket) {
  res <- tryCatch(
    {
      env64$s3$head_bucket(Bucket = bucket)
    },
    error = function(e) e
  )
  !inherits(res, c("error", "error_response"))
}

#' Create an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to
#' [list_objects](https://www.paws-r-sdk.com/docs/s3_create_bucket/)
#' @note Requires the env var `AWS_REGION`
#' @return the bucket path (character)
#' @family buckets
#' @examples \dontrun{
#' aws_bucket_create(bucket = "s64-test-2")
#' }
aws_bucket_create <- function(bucket, ...) {
  env64$s3$create_bucket(
    Bucket = bucket,
    CreateBucketConfiguration =
      list(LocationConstraint = env_var("AWS_REGION")), ...
  )$Location
}

#' Create a bucket if it does not exist
#' @inheritParams aws_bucket_delete
#' @keywords internal
bucket_create_if_not <- function(bucket, force = FALSE) {
  if (!aws_bucket_exists(bucket)) {
    if (!force) {
      if (yesno("{.strong {bucket}} does not exist. Create it?")) {
        cli::cli_inform("Exiting without creating bucket {.strong {bucket}}")
        cli::cli_inform("Run again & respond affirmatively
          or use `aws_bucket_create`")
        return(invisible())
      }
    }
    aws_bucket_create(bucket)
  }
}

#' Delete an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param force (logical) force deletion without going through the prompt.
#' default: `FALSE`. Should only be set to `TRUE` when required for
#' non-interactive use.
#' @param ... named parameters passed on to
#' [delete_bucket](https://www.paws-r-sdk.com/docs/s3_delete_bucket/)
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @family buckets
#' @return `NULL`, invisibly
#' @examplesIf interactive()
#' bucket_name <- "bucket-to-delete-113"
#' if (!aws_bucket_exists(bucket_name)) {
#'   aws_bucket_create(bucket = bucket_name)
#'   aws_buckets()
#'   aws_bucket_delete(bucket = bucket_name)
#'   aws_buckets()
#' }
aws_bucket_delete <- function(bucket, force = FALSE, ...) {
  # TODO: add a package level option to override the prompt for adv. users
  if (!force) {
    if (yesno("Are you sure you want to delete {.strong {bucket}}?")) {
      return(invisible())
    }
  }
  env64$s3$delete_bucket(Bucket = bucket, ...)
  return(invisible())
}

#' Download an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param dest_path (character) destination directory to store files. required
#' @param ... named parameters passed on to [s3fs::s3_dir_download()]
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @family buckets
#' @examplesIf interactive()
#' aws_bucket_create(bucket = "tmp-bucket-369")
#' desc_file <- file.path(system.file(), "DESCRIPTION")
#' aws_file_upload(bucket = "tmp-bucket-369", path = desc_file)
#' aws_file_upload(bucket = "tmp-bucket-369", path = desc_file, key = "d_file")
#' temp_dir <- file.path(tempdir(), "tmp-bucket-369")
#' aws_bucket_download(bucket = "tmp-bucket-369", dest_path = temp_dir)
#'
#' # cleanup
#' aws_bucket_delete("tmp-bucket-369")
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
#' @family buckets
#' @details To upload individual files see [aws_file_upload()]
#' @examplesIf interactive()
#' library(fs)
#' tdir <- path(tempdir(), "apples")
#' dir.create(tdir)
#' tfiles <- replicate(n = 10, file_temp(tmp_dir = tdir, ext = ".txt"))
#' invisible(lapply(tfiles, function(x) write.csv(mtcars, x)))
#'
#' bucket_name <- "a-new-bucket-345"
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' aws_bucket_upload(path = tdir, bucket = bucket_name)
#' aws_bucket_list_objects(bucket_name)
#'
#' # cleanup
#' objs <- aws_bucket_list_objects(bucket_name)
#' aws_file_delete(objs$uri)
#' aws_bucket_delete(bucket_name, force = TRUE)
#' aws_bucket_exists(bucket_name)
aws_bucket_upload <- function(
    path, bucket, max_batch = fs::fs_bytes("100MB"),
    ...) {
  if (!aws_bucket_exists(bucket)) {
    if (yesno("{.strong {bucket}} does not exist. Create it?")) {
      cli::cli_inform("Exiting without uploading {.strong {basename(path)}}")
      return(invisible())
    }
    aws_bucket_create(bucket)
  }
  s3fs::s3_dir_upload(
    path = path, new_path = bucket,
    max_batch = max_batch
  )
}

#' List objects in an S3 bucket
#'
#' @export
#' @importFrom s3fs s3_dir_info
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to [s3fs::s3_dir_info()]
#' @family buckets
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
#' @examplesIf interactive()
#' bucket_name <- "s64-test-67"
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' aws_file_upload(
#'   links_file,
#'   s3_path(bucket_name, basename(links_file))
#' )
#' aws_bucket_list_objects(bucket = bucket_name)
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
#' @family buckets
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
#' @param recurse (logical) returns all AWS S3 objects in lower sub
#' directories, default: `TRUE`
#' @param ... Additional arguments passed to [s3fs::s3_dir_tree()]
#' @family buckets
#' @return character vector of objects/files within the bucket,
#' printed as a tree
#' @examplesIf interactive()
#' bucket_name <- "s64-test-69"
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' pkgs_file <- file.path(system.file(), "Meta/package.rds")
#' demo_file <- file.path(system.file(), "Meta/demo.rds")
#' aws_file_upload(
#'   c(links_file, pkgs_file, demo_file),
#'   s3_path(
#'     bucket_name,
#'     c(
#'       basename(links_file),
#'       basename(pkgs_file),
#'       basename(demo_file)
#'     )
#'   )
#' )
#' aws_bucket_tree(bucket_name)
#'
#' # cleanup
#' objs <- aws_bucket_list_objects(bucket_name)
#' aws_file_delete(objs$uri)
#' aws_bucket_delete(bucket_name, force = TRUE)
#' aws_bucket_exists(bucket_name)
aws_bucket_tree <- function(bucket, recurse = TRUE, ...) {
  s3fs::s3_dir_tree(s3_path(bucket), recurse = recurse, ...)
}
