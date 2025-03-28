bucket_checks <- function(bucket) {
  stop_if_not(rlang::is_character(bucket), "bucket must be character")
  stop_if_not(length(bucket) == 1, "length(bucket) != 1")
}

#' Check if an S3 bucket exists
#'
#' @export
#' @param bucket (character) bucket name; must be length 1. required
#' @note internally uses
#' [head_bucket](https://www.paws-r-sdk.com/docs/s3_head_bucket/)
#' @family buckets
#' @return a single boolean (logical)
#' @examplesIf aws_has_creds()
#' bucket1 <- random_bucket()
#' aws_bucket_create(bucket1)
#'
#' # exists
#' aws_bucket_exists(bucket = bucket1)
#' # does not exist
#' aws_bucket_exists(bucket = "no-bucket")
#'
#' # cleanup
#' six_bucket_delete(bucket1, force = TRUE)
aws_bucket_exists <- function(bucket) {
  bucket_checks(bucket)
  res <- tryCatch(
    {
      con_s3()$head_bucket(Bucket = bucket)
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
#' [create_bucket](https://www.paws-r-sdk.com/docs/s3_create_bucket/)
#' @note Requires the env var `AWS_REGION`
#' @return the bucket path (character)
#' @family buckets
#' @examplesIf aws_has_creds()
#' bucket2 <- random_bucket()
#' aws_bucket_create(bucket2)
#'
#' # cleanup
#' six_bucket_delete(bucket2, force = TRUE)
aws_bucket_create <- function(bucket, ...) {
  bucket_checks(bucket)
  con_s3()$create_bucket(
    Bucket = bucket,
    CreateBucketConfiguration = list(
      LocationConstraint = env_var("AWS_REGION")
    ),
    ...
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
        cli::cli_inform(
          "Run again & respond affirmatively
          or use `aws_bucket_create`"
        )
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
#' @examplesIf aws_has_creds()
#' bucket_name <- random_bucket()
#' if (!aws_bucket_exists(bucket_name)) {
#'   aws_bucket_create(bucket = bucket_name)
#'   aws_buckets()
#'   aws_bucket_delete(bucket = bucket_name, force = TRUE)
#'   aws_buckets()
#' }
aws_bucket_delete <- function(bucket, force = FALSE, ...) {
  bucket_checks(bucket)
  if (!force) {
    if (yesno("Are you sure you want to delete {.strong {bucket}}?")) {
      return(invisible())
    }
  }
  con_s3()$delete_bucket(Bucket = bucket, ...)
  return(invisible())
}

#' Delete an S3 bucket
#'
#' Takes care of deleting bucket objects, so that the bucket itself
#' can be deleted cleanly
#'
#' @export
#' @importFrom purrr safely
#' @inheritParams aws_bucket_delete
#' @section What is magical:
#' - Exits early if bucket does not exist
#' - Checks for any objects in the bucket and deletes any present
#' - Deletes bucket after deleting objects
#' @family buckets
#' @family magicians
#' @return `NULL`, invisibly
#' @examplesIf aws_has_creds()
#' # bucket does not exist
#' six_bucket_delete("notabucket")
#'
#' # bucket exists w/o objects
#' bucket <- random_bucket()
#' aws_bucket_create(bucket)
#' six_bucket_delete(bucket, force = TRUE)
#'
#' # bucket exists w/ objects (files and directories with files)
#' bucket <- random_bucket()
#' aws_bucket_create(bucket)
#' demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' aws_file_upload(
#'   c(demo_rds_file, links_file),
#'   s3_path(bucket, c(basename(demo_rds_file), basename(links_file)))
#' )
#' aws_file_upload(
#'   c(demo_rds_file, links_file),
#'   s3_path(
#'     bucket, "newfolder",
#'     c(basename(demo_rds_file), basename(links_file))
#'   )
#' )
#' aws_bucket_list_objects(bucket)
#' six_bucket_delete(bucket, force = TRUE)
six_bucket_delete <- function(bucket, force = FALSE, ...) {
  msg_no_objects <- c(
    "Are you sure you want to delete {.strong {bucket}}?"
  )
  msg_with_objects <- c(
    "Are you sure you want to delete {.strong {bucket}} ",
    "and its {.strong {length(objects$result$uri)}} objects?"
  )
  bucket_checks(bucket)
  if (!aws_bucket_exists(bucket)) {
    cli_warning("bucket {.strong {bucket}} does not exist; exiting")
    return(invisible())
  }
  list_obs <- safely(aws_bucket_list_objects)
  objects <- list_obs(bucket)
  if (is_empty(objects$result)) {
    cli_info("bucket {.strong {bucket}} has no objects")
    if (!force) {
      if (yesno(msg_no_objects)) {
        return(invisible())
      }
    }
  } else {
    if (!force) {
      if (yesno(msg_with_objects)) {
        return(invisible())
      }
    }
    map(objects$result$uri, \(x) aws_file_delete(x))

    # check for empty folders & delete thoes too
    empties <- list_obs(bucket)
    if (!is_empty(empties$result)) {
      map(empties$result$uri, \(x) aws_file_delete(x))
    }
  }
  cli_info("deleting bucket {.strong {bucket}}")
  aws_bucket_delete(bucket, force = TRUE, ...)
  invisible()
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
#' @return path (character) to downloaded file(s)/directory
#' @examplesIf aws_has_creds()
#' bucket <- random_bucket()
#' aws_bucket_create(bucket = bucket)
#' desc_file <- file.path(system.file(), "DESCRIPTION")
#' aws_file_upload(desc_file, s3_path(bucket, "DESCRIPTION.txt"))
#' aws_file_upload(desc_file, s3_path(bucket, "d_file.txt"))
#' temp_dir <- file.path(tempdir(), bucket)
#' aws_bucket_download(bucket = bucket, dest_path = temp_dir)
#' fs::dir_ls(temp_dir)
#'
#' # cleanup
#' six_bucket_delete(bucket, force = TRUE)
aws_bucket_download <- function(bucket, dest_path, ...) {
  con_s3fs()$dir_download(path = bucket, new_path = dest_path, ...)
}

#' Upload a folder of files to create an S3 bucket
#'
#' @export
#' @importFrom fs fs_bytes
#' @param path (character) local path to a directory. required
#' @param max_batch (fs_bytes) maximum batch size being uploaded with each
#' multipart
#' @param ... named parameters passed on to [s3fs::s3_dir_upload()]
#' @inheritParams aws_bucket_delete
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @family buckets
#' @details To upload individual files see [aws_file_upload()]
#' @return the s3 format path of the bucket uploaded to
#' @examplesIf aws_has_creds()
#' library(fs)
#' tdir <- path(tempdir(), "apples")
#' dir.create(tdir)
#' tfiles <- replicate(n = 10, file_temp(tmp_dir = tdir, ext = ".txt"))
#' invisible(lapply(tfiles, function(x) write.csv(mtcars, x)))
#'
#' bucket_name <- random_bucket()
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' aws_bucket_upload(path = tdir, bucket = bucket_name)
#' aws_bucket_list_objects(bucket_name)
#'
#' # cleanup
#' objs <- aws_bucket_list_objects(bucket_name)
#' aws_file_delete(objs$uri)
#' aws_bucket_list_objects(bucket_name)
#' aws_bucket_delete(bucket_name, force = TRUE)
#' aws_bucket_exists(bucket_name)
aws_bucket_upload <- function(
  path,
  bucket,
  max_batch = fs::fs_bytes("100MB"),
  force = FALSE,
  ...
) {
  stop_if(rlang::is_missing(path), "{.strong path} is required")
  stop_if(rlang::is_missing(bucket), "{.strong bucket} is required")
  if (!aws_bucket_exists(bucket)) {
    if (!force) {
      if (yesno("{.strong {bucket}} does not exist. Create it?")) {
        cli::cli_inform("Exiting without uploading {.strong {basename(path)}}")
        return(invisible())
      }
    }
    aws_bucket_create(bucket)
  }
  con_s3fs()$dir_upload(
    path = path,
    new_path = bucket,
    max_batch = max_batch,
    ...
  )
  s3_path(bucket)
}

bucket_name <- function(x) {
  first(fs::path_split(first(x))[[1]])
}

#' Get file path starting at a certain path component
#' @importFrom fs path_join path_split
#' @export
#' @keywords internal
#' @return a single file path of class `fs_path`/`character`
#' @examples
#' path_from(path = "Rtmpxsqth0/apples/mcintosh/orange.csv", from = "apples")
path_from <- function(path, from) {
  parts <- fs::path_split(path)[[1]]
  kept_parts <- parts[which(parts == from):length(parts)]
  fs::path_join(kept_parts)
}

#' @importFrom fs is_dir dir_ls
#' @importFrom purrr list_rbind
explode_file_paths <- function(path) {
  if (any(is_dir(path))) {
    paths <- map(path, \(p) {
      if (is_dir(p)) {
        map(
          dir_ls(p, recurse = TRUE, type = "file"),
          \(z) {
            tibble(key = path_from(z, basename(p)), path = unname(z))
          }
        ) %>%
          list_rbind()
      } else {
        tibble(key = basename(p), path = p)
      }
    })
  } else {
    paths <- list(tibble(key = basename(path), path = path))
  }
  list_rbind(paths)
}

#' Magically upload a mix of files and directories into a bucket
#'
#' @importFrom fs path
#' @export
#' @param path (character) one or more file paths to add to
#' the `bucket`. required. can include directories or files
#' @param remote (character/scalar) a character string to use to upload
#' files in `path`. the first component of the path will be used as the
#' bucket name. any subsequent path components will be used as a
#' key prefix for all objects created in the bucket
#' @inheritParams aws_file_copy
#' @param ... named params passed on to
#' [put_object](https://www.paws-r-sdk.com/docs/s3_put_object/)
#' @section What is magical:
#' - Exits early if folder or files do not exist
#' - Creates the bucket if it does not exist
#' - Adds files to the bucket at the top level with key as the file name
#' - Adds directories to the bucket, reconstructing the exact directory
#' structure in the S3 bucket
#' @family buckets
#' @family magicians
#' @return (character) a vector of remote s3 paths where your
#' files are located
#' @examplesIf aws_has_creds()
#' # single file, single remote path
#' bucket1 <- random_bucket()
#' demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
#' six_bucket_upload(path = demo_rds_file, remote = bucket1, force = TRUE)
#'
#' ## a file and a directory - with a single remote path
#' bucket2 <- random_bucket()
#' library(fs)
#' tdir <- path(path_temp(), "mytmp")
#' dir_create(tdir)
#' invisible(purrr::map(letters, \(l) file_create(path(tdir, l))))
#' dir_tree(tdir)
#' six_bucket_upload(path = c(demo_rds_file, tdir), remote = bucket2,
#' force = TRUE)
#'
#' ## a directory with nested dirs - with a single remote path
#' bucket3 <- random_bucket()
#' library(fs)
#' tdir <- path(path_temp(), "apples")
#' dir_create(tdir)
#' dir_create(path(tdir, "mcintosh"))
#' dir_create(path(tdir, "pink-lady"))
#' cat("Some text in a readme", file = path(tdir, "README.md"))
#' write.csv(Orange, file = path(tdir, "mcintosh", "orange.csv"))
#' write.csv(iris, file = path(tdir, "pink-lady", "iris.csv"))
#' dir_tree(tdir)
#' six_bucket_upload(path = tdir, remote = path(bucket3, "fruit/basket"),
#' force = TRUE)
#'
#' # cleanup
#' six_bucket_delete(bucket1, force = TRUE)
#' six_bucket_delete(bucket2, force = TRUE)
#' six_bucket_delete(bucket3, force = TRUE)
six_bucket_upload <- function(path, remote, force = FALSE, ...) {
  stop_if_not(is_character(path), "{.strong path} must be character")
  stop_if_not(is_character(remote), "{.strong remote} must be character")
  stop_if_not(length(remote) == 1, "{.strong remote} must be length 1")

  path <- explode_file_paths(path)
  stop_if_not(
    all(file_exists(path$path)),
    "one or more of {.strong path} don't exist"
  )

  bucket <- bucket_name(remote)
  bucket_create_if_not(bucket, force)
  if (!aws_bucket_exists(bucket)) {
    cli_warning("bucket {.strong {bucket}} not created; exiting")
    return(invisible())
  }

  # if remote has more than bucket name, use folder for keys
  remote_parts <- path_split(remote)[[1]]
  if (length(remote_parts) > 1) {
    key_prefix <- path_join(remote_parts[-1])
    cli_info("using key prefix {.strong {key_prefix}}")
    path$key <- path(key_prefix, path$key)
  }

  map(apply(path, 1, as.list), \(row) {
    con_s3()$put_object(
      Bucket = bucket,
      Key = row$key,
      Body = row$path,
      ...
    )
  })
  s3_path(bucket, path$key)
}

#' List objects in an S3 bucket
#'
#' @export
#' @autoglobal
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to
#' [list_objects](https://www.paws-r-sdk.com/docs/s3_list_objects/)
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
#' @examplesIf aws_has_creds()
#' bucket_name <- random_bucket()
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' aws_file_upload(
#'   links_file,
#'   s3_path(bucket_name, basename(links_file))
#' )
#' aws_bucket_list_objects(bucket = bucket_name)
#' # cleanup
#' six_bucket_delete(bucket_name, force = TRUE)
aws_bucket_list_objects <- function(bucket, ...) {
  out <- con_s3()$list_objects(bucket, ...)
  if (rlang::is_empty(out$Contents)) {
    return(tibble())
  }
  as_tibble(jsonlite::fromJSON(
    jsonlite::toJSON(out$Contents, auto_unbox = TRUE),
    flatten = TRUE
  )) %>%
    mutate(
      bucket = bucket,
      uri = glue("s3://{bucket}/{Key}"),
      Size = fs::as_fs_bytes(Size),
      LastModified = .as_datetime(LastModified)
    ) %>%
    rowwise() %>%
    mutate(
      type = if (endsWith(Key, "/")) "directory" else "file"
    ) %>%
    ungroup() %>%
    rename_with(tolower) %>%
    select(bucket, key, uri, size, type, etag, lastmodified, storageclass)
}

#' List S3 buckets
#'
#' @export
#' @inherit aws_bucket_list_objects
#' @details internally uses `s3fs::s3_dir_info()`
#' @note we set `refresh=TRUE` internally to make sure we return up to date
#' information about your buckets rather than what's cached locally
#' @family buckets
#' @examplesIf aws_has_creds()
#' aws_buckets()
aws_buckets <- function(...) {
  out <- con_s3fs()$dir_info(refresh = TRUE, ...)
  if (is.data.frame(out) && NROW(out) > 0) {
    as_tibble(out)
  } else {
    tibble()
  }
}

#' Print a tree of the objects in a bucket
#'
#' @export
#' @inheritParams aws_bucket_exists
#' @param recurse (logical) returns all AWS S3 objects in lower sub
#' directories, default: `TRUE`
#' @param ... Additional arguments passed to `s3fs::s3_dir_tree()`
#' @family buckets
#' @return character vector of objects/files within the bucket,
#' printed as a tree
#' @examplesIf aws_has_creds()
#' bucket_name <- random_bucket()
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
  con_s3fs()$dir_tree(s3_path(bucket), recurse = recurse, ...)
}
