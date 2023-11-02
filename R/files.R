#' Upload a file
#'
#' @export
#' @importFrom fs file_exists
#' @importFrom s3fs s3_file_copy
#' @importFrom cli cli_inform
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
#' aws_file_upload(demo_rds_file, s3_path("s64-test-2", basename(demo_rds_file)))
#'
#' ## many files at once
#' links_rds_file <- file.path(system.file(), "Meta/links.rds")
#' aws_file_upload(
#'   c(demo_rds_file, links_rds_file),
#'   s3_path("s64-test-2", c(basename(demo_rds_file), basename(links_rds_file)))
#' )
#'
#' # set expiration, expire 1 minute from now
#' aws_file_upload(demo_rds_file, s3_path("s64-test-2", "ddd.rds"), Expires = Sys.time() + 60)
#'
#' # bucket doesn't exist
#' aws_file_upload(demo_rds_file, "s3://not-a-bucket/eee.rds")
#' }
#'
#' @examplesIf interactive()
#' # path doesn't exist
#' aws_file_upload("file_doesnt_exist.txt", s3_path("s64-test-2", "file_doesnt_exist.txt"))
aws_file_upload <- function(path, remote_path, ...) {
  stopifnot(fs::file_exists(path))
  bucket <- path_s3_parser(remote_path)[[1]]$bucket
  if (!aws_bucket_exists(bucket)) {
    if (yesno("{.strong {bucket}} does not exist. Create it?")) {
      cli::cli_inform("Exiting without uploading {.strong {basename(path)}}")
      return(invisible())
    }
    aws_bucket_create(bucket)
  }
  s3fs::s3_file_copy(path, remote_path, ...)
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
#' aws_file_download(remote_path = s3_path("s64-test-2", c("a_file", "c_file", "d_file")), path = tfiles)
#'
#' ## length of `remote_path` and `path` must be the same
#' tfiles <- replicate(n = 2, tempfile())
#' aws_file_download(remote_path = s3_path("s64-test-2", c("a_file", "c_file", "d_file")), path = tfiles)
#'
#' # S3 file does not exist
#' temp_path <- tempfile()
#' aws_file_download(s3_path("s64-test-2", "TESTING123"), temp_path)
#' }
aws_file_download <- function(remote_path, path, ...) {
  # FIXME: s3fs is not checking that length(remote_path) == length(path)
  stopifnot(length(remote_path) == length(path))
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
