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
#' @importFrom s3fs s3_file_copy
#' @inheritParams aws_file_copy
#' @param path (character) a file path to read from. required
#' @param remote_path (character) a remote path where the file
#' should go. required
#' @param ... named parameters passed on to [s3fs::s3_file_copy()]
#' @return (character) a vector of remote s3 paths
#' @details
#' - For upload: if it does exist it will be created
#' - For download: if it does not exist, function will return an error
#' @examples \dontrun{
#' demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
#' aws_file_upload(
#'   demo_rds_file,
#'   s3_path("s64-test-2", basename(demo_rds_file))
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
#' }
#'
#' @examplesIf interactive()
#' # path doesn't exist
#' aws_file_upload(
#'   "file_doesnt_exist.txt",
#'   s3_path("s64-test-2", "file_doesnt_exist.txt")
#' )
aws_file_upload <- function(path, remote_path, force = FALSE, ...) {
  stopifnot(fs::file_exists(path))
  bucket <- path_s3_parse(remote_path)[[1]]$bucket
  bucket_create_if_not(bucket, force)
  purrr::map2_vec(path, remote_path, s3fs::s3_file_copy, ...)
}

#' Download a file
#'
#' @export
#' @param remote_path (character) one or more remote S3 paths. required
#' @param path (character) one or more file paths to write to. required
#' @param ... named parameters passed on to [s3fs::s3_file_download()]
#' @return (character) a vector of local file paths
#' @note USES A FORK OF s3fs FOR A MINOR FIX THAT MAKES LENGTH>1 INPUTS WORK
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
  s3fs::s3_file_download(remote_path, path, ...)
}

#' Delete a file
#'
#' @export
#' @importFrom s3fs s3_file_delete
#' @param remote_path (character) one or more remote S3 paths. required
#' @param ... named parameters passed on to [s3fs::s3_file_delete()]
#' @return (character) a vector of remote file paths
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
  s3fs::s3_file_delete(remote_path, ...)
}

#' File attributes
#'
#' @export
#' @inheritParams aws_file_download
#' @return a tibble with many columns, with number of rows matching length
#' of `remote_path`
#' @note uses [s3fs::s3_file_info()] internally
#' @examples \dontrun{
#' # files one by one
#' aws_file_attr(s3_path("s64-test-2", "DESCRIPTION"))
#' aws_file_attr(s3_path("s64-test-2", "ddd"))
#' aws_file_attr(s3_path("s64-test-2", "doesntexist"))
#' # or all together
#' aws_file_attr(s3_path("s64-test-2", c("DESCRIPTION", "ddd")))
#' }
aws_file_attr <- function(remote_path) {
  # TODO: error behavior isn't ideal b/c the error message doesn't indicate
  # which file does not exist
  s3fs::s3_file_info(remote_path) %>% as_tibble()
}

#' Check if a file exists
#'
#' @export
#' @inheritParams aws_file_attr
#' @return vector of booleans (`TRUE` or `FALSE`), length matches
#' `length(remote_path)`
#' @examples \dontrun{
#' aws_file_exists(s3_path("s64-test-2", "DESCRIPTION"))
#' aws_file_exists(s3_path("s64-test-2", "doesntexist"))
#' aws_file_exists(s3_path("s64-test-2", c("DESCRIPTION", "doesntexist")))
#' }
aws_file_exists <- function(remote_path) {
  s3fs::s3_file_exists(remote_path)
}

#' Rename a remote file
#'
#' @export
#' @inheritParams aws_file_attr
#' @param new_remote_path (character) one or more remote S3 paths. required.
#' length must match `remote_path`
#' @param ... named parameters passed on to [s3fs::s3_file_move()]
#' @return vector of paths, length matches `length(remote_path)`
#' @examples \dontrun{
#' aws_file_rename(s3_path("s64-test-2", "DESCRIPTION"),
#'   s3_path("s64-test-2", "DESC"))
#'
#' tfiles <- replicate(n = 3, tempfile())
#' for (i in tfiles) cat("Hello\nWorld\n", file = i)
#' paths <- s3_path("s64-test-2", c("aaa", "bbb", "ccc"), ext = "txt")
#' aws_file_upload(tfiles, paths)
#' new_paths <- s3_path("s64-test-2", c("new_aaa", "new_bbb", "new_ccc"),
#'   ext = "txt")
#' aws_file_rename(paths, new_paths)
#' }
aws_file_rename <- function(remote_path, new_remote_path, ...) {
  equal_lengths(remote_path, new_remote_path)
  s3fs::s3_file_move(remote_path, new_remote_path, ...)
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
#' @param ... named parameters passed on to [s3fs::s3_file_copy()]
#' @return vector of paths, length matches `length(remote_path)`
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
  bucket_create_if_not(bucket, force)
  parsed <- path_s3_parse(remote_path)
  parsed <- purrr::map(parsed, function(x) {
    x$bucket <- bucket
    x
  })
  new_paths <- path_s3_build(parsed)
  equal_lengths(remote_path, new_paths)
  s3fs::s3_file_copy(remote_path, new_paths, ...)
}
