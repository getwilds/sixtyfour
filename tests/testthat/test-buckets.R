skip_if_not(minio_available(), "Minio Not Available")

Sys.setenv(AWS_PROFILE = "minio")
buckets_empty()

demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
links_file <- file.path(system.file(), "Meta/links.rds")

test_that("aws_bucket_create", {
  expect_error(aws_bucket_create())
  expect_error(aws_bucket_create(5))
  expect_error(aws_bucket_create(letters))

  bucket <- random_bucket()
  expect_false(aws_bucket_exists(bucket))
  aws_bucket_create(bucket)
  expect_true(aws_bucket_exists(bucket))
})

test_that("aws_bucket_exists", {
  expect_error(aws_bucket_exists())
  expect_error(aws_bucket_exists(5))
  expect_error(aws_bucket_exists(letters))

  bucket <- random_bucket()

  # bucket DOES NOT exist, gives FALSE
  expect_false(aws_bucket_exists(bucket))

  # bucket DOES exist, also FALSE
  aws_bucket_create(bucket)
  expect_true(aws_bucket_exists(bucket))

  bucket_delete(bucket, force = TRUE)
})

test_that("aws_bucket_delete", {
  expect_error(aws_bucket_delete())
  expect_error(aws_bucket_delete(5))
  expect_error(aws_bucket_delete(letters))

  bucket <- random_bucket()
  aws_bucket_create(bucket)
  expect_true(aws_bucket_exists(bucket))
  res <- aws_bucket_delete(bucket, force = TRUE)
  expect_null(res)
  expect_false(aws_bucket_exists(bucket))
})

test_that("aws_bucket_download", {
  expect_error(aws_bucket_download())
  expect_error(aws_bucket_download(""))

  bucket <- random_bucket()
  aws_bucket_create(bucket)

  aws_file_upload(demo_rds_file, s3_path(bucket, basename(demo_rds_file)))
  aws_file_upload(links_file, s3_path(bucket, basename(links_file)))
  temp_dir <- file.path(tempdir(), "tmp-bucket-369")

  expect_length(list.files(temp_dir), 0)

  aws_bucket_download(bucket = bucket, dest_path = temp_dir)

  expect_length(list.files(temp_dir), 2)

  bucket_delete(bucket, force = TRUE)
})

test_that("aws_bucket_upload", {
  expect_error(aws_bucket_upload())
  expect_error(aws_bucket_upload(""))

  res <- aws_bucket_upload(
    file.path(system.file(), "Meta"),
    "metabucket",
    force = TRUE
  )

  expect_equal(NROW(aws_bucket_list_objects("metabucket")), 6)
  expect_type(res, "character")

  bucket_delete("metabucket", force = TRUE)
})

test_that("aws_bucket_list_objects", {
  expect_error(aws_bucket_list_objects())
  expect_error(aws_bucket_list_objects(5))

  bucket <- random_bucket()
  aws_bucket_create(bucket)
  ffs <- list.files(file.path(system.file(), "Meta"), full.names = TRUE)
  for (f in ffs) aws_file_upload(f, s3_path(bucket, basename(f)))

  res <- aws_bucket_list_objects(bucket)

  expect_s3_class(res, "tbl")
  expect_equal(NROW(res), 6)
  expect_s3_class(res$size, "fs_bytes")

  bucket_delete(bucket, force = TRUE)
})

test_that("aws_buckets", {
  for (i in replicate(10, random_bucket())) aws_bucket_create(i)

  res <- aws_buckets()

  expect_s3_class(res, "tbl")
  expect_gt(NROW(res), 5)

  buckets_empty()
})

test_that("aws_bucket_tree", {
  expect_error(aws_bucket_tree())
  expect_error(aws_bucket_tree("", 5))

  bucket <- random_bucket()
  aws_bucket_create(bucket)
  ffs <- list.files(file.path(system.file(), "Meta"), full.names = TRUE)
  for (f in ffs) aws_file_upload(f, s3_path(bucket, basename(f)))

  expect_output(
    res <- aws_bucket_tree(bucket),
    "s3://"
  )

  expect_type(res, "character")
  expect_length(res, 6)

  bucket_delete(bucket, force = TRUE)
})

test_that("six_bucket_upload, single file", {
  bucket <- random_bucket()
  demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
  res <- six_bucket_upload(path = demo_rds_file, remote = bucket, force = TRUE)
  objs <- aws_bucket_list_objects(bucket)

  expect_type(res, "character")
  expect_length(res, 1)
  expect_match(res, bucket) # bucket is in each s3 path
  expect_equal(NROW(objs), 1)
  expect_equal(objs$key, basename(demo_rds_file))
})

test_that("six_bucket_upload, mixed inputs (file and dir)", {
  bucket <- random_bucket()
  library(fs)
  demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
  tdir <- path(path_temp(), "mytmp")
  dir_create(tdir)
  purrr::map(letters, \(l) file_create(path(tdir, l)))
  res <- suppressMessages(
    six_bucket_upload(
      path = c(demo_rds_file, tdir),
      remote = bucket,
      force = TRUE
    )
  )
  objs <- aws_bucket_list_objects(bucket)

  expect_type(res, "character")
  expect_length(res, 27)
  expect_match(res, bucket) # bucket is in each s3 path
  expect_equal(NROW(objs), 27)
})

test_that("six_bucket_upload, remote includes key prefix change", {
  bucket <- random_bucket()
  library(fs)
  tdir <- path(path_temp(), "atmp")
  demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
  dir_create(tdir)
  purrr::map(letters, \(l) file_create(path(tdir, l)))
  res <- suppressMessages(
    six_bucket_upload(
      path = c(demo_rds_file, tdir),
      remote = path(bucket, "some", "dir"),
      force = TRUE
    )
  )
  objs <- aws_bucket_list_objects(bucket)

  expect_type(res, "character")
  expect_length(res, 27)
  expect_match(res, bucket) # bucket is in each s3 path
  expect_match(res, "some/dir") # key prefix is in each s3 path
  expect_equal(NROW(objs), 27)
})

test_that("six_bucket_upload error behavior", {
  expect_error(
    six_bucket_upload(path = letters, remote = c("z", "z")),
    "must be length 1"
  )
  expect_error(
    six_bucket_upload(path = 5, remote = ""),
    "must be character"
  )
  expect_error(
    six_bucket_upload(path = "", remote = 5),
    "must be character"
  )
  expect_error(
    six_bucket_upload(path = "", remote = ""),
    "don't exist"
  )
  expect_error(
    six_bucket_upload(path = "", remote = ""),
    "don't exist"
  )
})

# cleanup
buckets_empty()
Sys.unsetenv("AWS_PROFILE")
