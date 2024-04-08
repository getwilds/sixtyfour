skip_if_not(minio_available(), "Minio Not Available")

Sys.setenv(AWS_PROFILE = "minio")
buckets_empty()

demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
links_file <- file.path(system.file(), "Meta/links.rds")

test_that("aws_file_upload - error behavior", {
  expect_error(aws_file_upload())
  expect_error(aws_file_upload(""))

  # file doesn't exist
  expect_error(
    aws_file_upload(
      "file_doesnt_exist.txt",
      s3_path("s64-test-2", "file_doesnt_exist.txt")
    ),
    "is not TRUE"
  )
})

test_that("aws_file_upload - 1 file", {
  bucket <- random_string("bucket")
  aws_bucket_create(bucket)
  res <- aws_file_upload(
    demo_rds_file,
    s3_path(bucket, basename(demo_rds_file))
  )

  expect_type(res, "character")
  expect_length(res, 1)
})

test_that("aws_file_upload - many files", {
  aws_bucket_create("upload")

  the_files <- replicate(50, tempfile(fileext = ".txt"))
  for (f in the_files) cat(letters, file = f)

  res <- aws_file_upload(
    the_files, s3_path("upload", basename(the_files))
  )

  expect_length(res, length(the_files))
  for (f in res) expect_type(f, "character")
  for (f in res) expect_match(f, "upload")
  for (f in res) expect_match(f, ".txt")

  bucket_delete("upload", force = TRUE)
})

test_that("aws_file_download - error behavior", {
  expect_error(aws_file_download())
  expect_error(aws_file_download(""))

  aws_bucket_create("download")

  # remote file doesn't exist
  expect_error(
    aws_file_download(
      s3_path("download", "file_does_not_exist.txt"),
      tempfile("file_does_not_exist", fileext = ".txt")
    ),
    "Remote file not found"
  )

  aws_file_upload(
    demo_rds_file,
    s3_path("download", basename(demo_rds_file))
  )
  expect_error(
    aws_file_download(
      s3_path("download", basename(demo_rds_file)),
      5L
    ),
    "invalid 'file' argument"
  )

  bucket_delete("download", force = TRUE)
})

test_that("aws_file_download - many files", {
  aws_bucket_create("download")

  the_files <- replicate(10, tempfile(fileext = ".txt"))
  for (f in the_files) cat(letters, "\n", file = f)

  res <- aws_file_upload(
    the_files, s3_path("download", basename(the_files))
  )

  downloaded_files <- replicate(10, tempfile(fileext = ".txt"))
  out <- aws_file_download(
    s3_path("download", basename(the_files)),
    downloaded_files
  )

  expect_length(out, length(the_files))
  for (f in out) expect_type(f, "character")
  for (f in out) expect_match(f, ".txt")
  for (f in out) {
    expect_equal(
      strsplit(readLines(f, warn = FALSE), " ")[[1]],
      letters
    )
  }

  bucket_delete("download", force = TRUE)
})

test_that("aws_file_delete - error behavior", {
  expect_error(aws_file_delete())

  # bucket DOES NOT exist, error raised that no bucket exists
  expect_error(
    aws_file_delete(s3_path("a-bucket", "TESTING123")),
    "bucket does not exist"
  )

  # bucket DOES exist, no error is raised
  aws_bucket_create("a-bucket")
  expect_no_error(aws_file_delete(s3_path("a-bucket", "TESTING123")))

  bucket_delete("a-bucket", force = TRUE)
})

test_that("aws_file_delete", {
  aws_bucket_create("b-bucket")

  tfile <- tempfile(fileext = ".txt")
  cat("Hello World!", file = tfile)
  remote_path <- s3_path("b-bucket", basename(tfile))
  aws_file_upload(tfile, remote_path)

  expect_true(aws_file_exists(remote_path))
  res <- aws_file_delete(remote_path)
  expect_type(res, "character")
  expect_false(aws_file_exists(remote_path))

  bucket_delete("b-bucket", force = TRUE)
})


test_that("aws_file_attr - error behavior", {
  expect_error(aws_file_attr())

  # bucket DOES NOT exist
  expect_error(
    aws_file_attr(s3_path("a-bucket", "TESTING123")),
    "SerializationError"
  )

  # bucket DOES exist, same error is raised for the file
  aws_bucket_create("attr-bucket")
  expect_error(
    aws_file_attr(s3_path("attr-bucket", "TESTING123")),
    "SerializationError"
  )

  bucket_delete("attr-bucket", force = TRUE)
})

test_that("aws_file_attr", {
  aws_bucket_create("attr-bucket")

  tfile <- tempfile(fileext = ".txt")
  cat("Hello World!", file = tfile)
  remote_path <- s3_path("attr-bucket", basename(tfile))
  aws_file_upload(tfile, remote_path)

  expect_true(aws_file_exists(remote_path))
  res <- aws_file_attr(remote_path)
  expect_s3_class(res, "tbl")
  expect_equal(NROW(res), 1)
  expect_equal(res$uri, remote_path)

  bucket_delete("attr-bucket", force = TRUE)
})


test_that("aws_file_exists - error behavior", {
  expect_error(aws_file_exists())

  bucket <- random_string("bucket")

  # bucket DOES NOT exist, just FALSE
  expect_false(aws_file_exists(s3_path(bucket, "TESTING123")))

  # bucket DOES exist, also FALSE
  aws_bucket_create(bucket)
  expect_false(aws_file_exists(s3_path(bucket, "TESTING123")))

  bucket_delete(bucket, force = TRUE)
})

test_that("aws_file_exists", {
  bucket <- random_string("bucket")
  aws_bucket_create(bucket)

  files <- replicate(5, tempfile(fileext = ".txt"))
  for (i in files) {
    cat("Hello World!\n\n", file = i)
    remote_path <- s3_path(bucket, basename(i))
    aws_file_upload(i, remote_path)
  }

  for (z in files) {
    expect_true(
      aws_file_exists(s3_path(bucket, basename(z)))
    )
  }
  expect_false(
    aws_file_exists(s3_path(bucket, "non-existent.txt"))
  )

  bucket_delete(bucket, force = TRUE)
})

test_that("aws_file_rename", {
  expect_error(aws_file_rename())
  expect_error(aws_file_rename(""))

  bucket <- random_string("bucket")
  aws_bucket_create(bucket)

  aws_file_upload(links_file, s3_path(bucket, basename(links_file)))

  expect_true(aws_file_exists(s3_path(bucket, "links.rds")))
  expect_false(aws_file_exists(s3_path(bucket, "mylinks.rds")))

  res <- aws_file_rename(
    s3_path(bucket, "links.rds"),
    s3_path(bucket, "mylinks.rds")
  )

  expect_false(aws_file_exists(s3_path(bucket, "links.rds")))
  expect_true(aws_file_exists(s3_path(bucket, "mylinks.rds")))
})


test_that("aws_file_copy", {
  expect_error(aws_file_copy())
  expect_error(aws_file_copy(""))

  bucket <- random_string("bucket")
  aws_bucket_create(bucket)

  aws_file_upload(links_file, s3_path(bucket, basename(links_file)))

  bucket_2 <- random_string("bucket")
  aws_bucket_create(bucket_2)

  expect_false(aws_file_exists(s3_path(bucket_2, "links.rds")))

  res <- aws_file_copy(
    remote_path = s3_path(bucket, "links.rds"),
    bucket = bucket_2
  )

  expect_true(aws_file_exists(s3_path(bucket_2, "links.rds")))
  expect_type(res, "character")
  expect_equal(res, s3_path(bucket_2, "links.rds"))
})



# cleanup
buckets_empty()
Sys.unsetenv("AWS_PROFILE")
