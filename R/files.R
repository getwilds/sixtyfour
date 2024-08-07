equal_lengths <- function(x, y) {
  # s3fs is not checking that length(remote_path) == length(path) as
  # it purports to be doing
  if (length(x) != length(y)) {
    xarg <- deparse(substitute(x))
    yarg <- deparse(substitute(y))
    stop(glue::glue("lengths of '{xarg}' and '{yarg}' must be equal"))
  }
}

#' Upload a file
#'
#' @export
#' @importFrom fs file_exists
#' @importFrom purrr map2_vec
#' @param path (character) a file path to read from. required
#' @param remote_path (character) a remote path where the file
#' should go. required
#' @param ... named parameters passed on to `s3fs::s3_file_copy()`
#' @return (character) a vector of remote s3 paths
#' @details to upload a folder of files see [aws_bucket_upload()]
#' @family files
#' @examplesIf interactive()
#' bucket <- random_string("bucket")
#' aws_bucket_create(bucket)
#' demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
#' aws_file_upload(
#'   demo_rds_file,
#'   s3_path(bucket, basename(demo_rds_file))
#' )
#'
#' ## many files at once
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' aws_file_upload(
#'   c(demo_rds_file, links_file),
#'   s3_path("s64-test-2", c(basename(demo_rds_file), basename(links_file)))
#' )
#'
#' # set expiration, expire 1 minute from now
#' aws_file_upload(demo_rds_file, s3_path("s64-test-2", "ddd.rds"),
#'   Expires = Sys.time() + 60
#' )
#'
#' # bucket doesn't exist
#' aws_file_upload(demo_rds_file, "s3://not-a-bucket/eee.rds")
#'
#' # path doesn't exist
#' aws_file_upload(
#'   "file_doesnt_exist.txt",
#'   s3_path("s64-test-2", "file_doesnt_exist.txt")
#' )
#'
#' # Path's without file extensions behave a little weird
#' ## With extension
#' ## Both of these lines do the same exact thing: make a file in the
#' ## same path in a bucket
#' aws_file_upload("LICENSE.md", s3_path(bucket, "LICENSE.md"))
#' aws_file_upload("LICENSE.md", s3_path(bucket))
#'
#' ## Without extension
#' ## However, it's different for a file without an extension
#' ## This makes a file in the bucket at path DESCRIPTION
#' aws_file_upload("DESCRIPTION", s3_path(bucket))
#'
#' ## Whereas this creates a directory called DESCRIPTION with
#' ## a file DESCRIPTION within it
#' aws_file_upload("DESCRIPTION", s3_path(bucket, "DESCRIPTION"))
aws_file_upload <- function(path, remote_path, ...) {
  stopifnot(fs::file_exists(path))
  bucket <- path_s3_parse(remote_path)[[1]]$bucket
  stop_if_not(
    aws_bucket_exists(bucket),
    "bucket {.strong {bucket}} doesn't exist"
  )
  map2_vec(path, remote_path, con_s3fs()$file_copy, ...)
}

#' Magically upload a file
#'
#' @export
#' @param path (character) one or more file paths to add to
#' the `bucket`. required. cannot include directories
#' @inheritParams aws_file_copy
#' @param ... named params passed on to
#' [put_object](https://www.paws-r-sdk.com/docs/s3_put_object/)
#' @section What is magical:
#' - Exits early if files do not exist
#' - Exits early if any `path` values are directories
#' - Creates the bucket if it does not exist
#' - Adds files to the bucket, figuring out the key to use from
#' the supplied path
#' - Function is vectoried for the `path` argument; you can
#' pass in many file paths
#' @family files
#' @family magicians
#' @return (character) a vector of remote s3 paths where your
#' files are located
#' @examplesIf interactive()
#' bucket <- random_string("bucket")
#' demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
#' six_file_upload(demo_rds_file, bucket)
#'
#' ## many files at once
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' six_file_upload(c(demo_rds_file, links_file), bucket)
#'
#' # set expiration, expire 1 minute from now
#' six_file_upload(demo_rds_file, bucket, Expires = Sys.time() + 60)
#'
#' # bucket doesn't exist, ask if you want to create it
#' six_file_upload(demo_rds_file, "not-a-buckets")
#'
#' # path doesn't exist
#' # six_file_upload("file_doesnt_exist.txt", random_string("bucket"))
#'
#' # directories not supported
#' mydir <- tempdir()
#' # six_file_upload(mydir, random_string("bucket"))
six_file_upload <- function(path, bucket, force = FALSE, ...) {
  stop_if_not(
    all(fs::file_exists(path)),
    "one or more of {.strong path} don't exist"
  )
  stop_if(
    any(fs::is_dir(path)),
    "one or more of {.strong path} is a directory; file paths only"
  )
  bucket_create_if_not(bucket, force)
  if (!aws_bucket_exists(bucket)) {
    cli_warning("bucket {.strong {bucket}} not created; exiting")
    return(invisible())
  }
  map(path, \(p) {
    con_s3()$put_object(Bucket = bucket, Key = basename(p), Body = p, ...)
  })
  s3_path(bucket, basename(path))
}

#' Download a file
#'
#' @export
#' @importFrom cli cli_abort
#' @param remote_path (character) one or more remote S3 paths. required
#' @param path (character) one or more file paths to write to. required
#' @param ... named parameters passed on to [s3fs::s3_file_download()]
#' @return (character) a vector of local file paths
#' @family files
#' @examples \dontrun{
#' tfile <- tempfile()
#' aws_file_download(remote_path = "s3://s64-test-2/DESCRIPTION", path = tfile)
#'
#' # many files
#' tfiles <- replicate(n = 3, tempfile())
#' aws_file_download(
#'   remote_path =
#'     s3_path("s64-test-2", c("a_file", "c_file", "d_file")), path = tfiles
#' )
#'
#' ## length of `remote_path` and `path` must be the same
#' tfiles <- replicate(n = 2, tempfile())
#' aws_file_download(
#'   remote_path =
#'     s3_path("s64-test-2", c("a_file", "c_file", "d_file")), path = tfiles
#' )
#'
#' # S3 file does not exist
#' temp_path <- tempfile()
#' aws_file_download(s3_path("s64-test-2", "TESTING123"), temp_path)
#' }
aws_file_download <- function(remote_path, path, ...) {
  equal_lengths(remote_path, path)
  res <- tryCatch(
    con_s3fs()$file_download(remote_path, path),
    error = function(e) e
  )
  if (rlang::is_error(res)) {
    if (grepl("SerializationError", res$message)) {
      cli::cli_abort(c("Remote file not found", "S3 error: {res$message}"))
    } else {
      cli::cli_abort(res$message)
    }
  }
  res
}

#' Delete a file
#'
#' @export
#' @param remote_path (character) one or more remote S3 paths. required
#' @param ... named parameters passed on to
#' [delete_object](https://www.paws-r-sdk.com/docs/s3_delete_object/)
#' @family files
#' @return `NULL` invisibly
#' @examples \dontrun{
#' # create a file
#' tfile <- tempfile()
#' cat("Hello World!", file = tfile)
#' aws_file_upload(remote_path = "s3://s64-test-2", path = tfile)
#'
#' # delete the file
#' aws_file_delete(s3_path("s64-test-2", basename(tfile)))
#'
#' # file does not exist - no error is raised
#' aws_file_delete(s3_path("s64-test-2", "TESTING123"))
#' }
aws_file_delete <- function(remote_path, ...) {
  map(remote_path, aws_file_delete_one, ...)
  remote_path
}

aws_file_delete_one <- function(one_path, ...) {
  path_parsed <- path_s3_parse(one_path)
  trailing_slash <- grepl("/$", one_path)
  key <- if (nzchar(path_parsed[[1]]$path)) {
    file.path(path_parsed[[1]]$path, path_parsed[[1]]$file)
  } else {
    path_parsed[[1]]$file
  }
  con_s3()$delete_object(
    path_parsed[[1]]$bucket,
    glue("{key}{ifelse(trailing_slash, '/', '')}")
  )
  invisible()
}

#' File attributes
#'
#' @export
#' @inheritParams aws_file_download
#' @return a tibble with many columns, with number of rows matching length
#' of `remote_path`
#' @note uses [s3fs::s3_file_info()] internally
#' @family files
#' @examples \dontrun{
#' # files one by one
#' aws_file_attr(s3_path("s64-test-2", "DESCRIPTION"))
#' aws_file_attr(s3_path("s64-test-2", "ddd"))
#' aws_file_attr(s3_path("s64-test-2", "doesntexist"))
#' # or all together
#' aws_file_attr(s3_path("s64-test-2", c("DESCRIPTION", "ddd")))
#' }
aws_file_attr <- function(remote_path) {
  con_s3fs()$file_info(remote_path) %>% as_tibble()
}

#' Check if a file exists
#'
#' @export
#' @inheritParams aws_file_attr
#' @return vector of booleans (`TRUE` or `FALSE`), length matches
#' `length(remote_path)`
#' @family files
#' @examples \dontrun{
#' aws_file_exists(s3_path("s64-test-2", "DESCRIPTION"))
#' aws_file_exists(s3_path("s64-test-2", "doesntexist"))
#' aws_file_exists(s3_path("s64-test-2", c("DESCRIPTION", "doesntexist")))
#' }
aws_file_exists <- function(remote_path) {
  con_s3fs()$file_exists(remote_path)
}

#' Rename a remote file
#'
#' @export
#' @inheritParams aws_file_attr
#' @param new_remote_path (character) one or more remote S3 paths. required.
#' length must match `remote_path`
#' @param ... named parameters passed on to [s3fs::s3_file_move()]
#' @return vector of paths, length matches `length(remote_path)`
#' @family files
#' @examples \dontrun{
#' aws_file_rename(
#'   s3_path("s64-test-2", "DESCRIPTION"),
#'   s3_path("s64-test-2", "DESC")
#' )
#'
#' tfiles <- replicate(n = 3, tempfile())
#' for (i in tfiles) cat("Hello\nWorld\n", file = i)
#' paths <- s3_path("s64-test-2", c("aaa", "bbb", "ccc"), ext = "txt")
#' aws_file_upload(tfiles, paths)
#' new_paths <- s3_path("s64-test-2", c("new_aaa", "new_bbb", "new_ccc"),
#'   ext = "txt"
#' )
#' aws_file_rename(paths, new_paths)
#' }
aws_file_rename <- function(remote_path, new_remote_path, ...) {
  equal_lengths(remote_path, new_remote_path)
  con_s3fs()$file_move(remote_path, new_remote_path, ...)
}

#' Copy files between buckets
#'
#' @export
#' @inheritParams aws_file_attr
#' @param bucket (character) bucket to copy files to. required.
#' if the bucket does not exist we prompt you asking if you'd like
#' the bucket to be created
#' @param force (logical) force bucket creation without going through
#' the prompt. default: `FALSE`. Should only be set to `TRUE` when
#' required for non-interactive use.
#' @param ... named parameters passed on to `s3fs::s3_file_copy()`
#' @return vector of paths, length matches `length(remote_path)`
#' @family files
#' @examples \dontrun{
#' # create files in an existing bucket
#' tfiles <- replicate(n = 3, tempfile())
#' for (i in tfiles) cat("Hello\nWorld\n", file = i)
#' paths <- s3_path("s64-test-2", c("aaa", "bbb", "ccc"), ext = "txt")
#' aws_file_upload(tfiles, paths)
#'
#' # create a new bucket
#' new_bucket <- aws_bucket_create(bucket = "s64-test-3")
#'
#' # add existing files to the new bucket
#' aws_file_copy(paths, path_as_s3(new_bucket))
#' # create bucket that doesn't exist yet
#' aws_file_copy(paths, "s64-test-4")
#' }
aws_file_copy <- function(remote_path, bucket, force = FALSE, ...) {
  stop_if(rlang::is_missing(remote_path), "{.strong remote_path} is required")
  stop_if(rlang::is_missing(bucket), "{.strong bucket} is required")
  bucket_create_if_not(bucket, force)
  parsed <- path_s3_parse(remote_path)
  parsed <- purrr::map(parsed, function(x) {
    x$bucket <- bucket
    x
  })
  new_paths <- path_s3_build(parsed)
  equal_lengths(remote_path, new_paths)
  con_s3fs()$file_copy(remote_path, new_paths, ...)
}
