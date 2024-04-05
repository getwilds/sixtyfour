skip_if_not(minio_available(), "Minio Not Available")

Sys.setenv(AWS_PROFILE = "minio")
buckets_empty()

demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
links_file <- file.path(system.file(), "Meta/links.rds")

test_that("aws_bucket_create", {
  expect_error(aws_bucket_create())
  expect_error(aws_bucket_create(5))
  expect_error(aws_bucket_create(letters))

  bucket <- random_string("bucket")
  expect_false(aws_bucket_exists(bucket))
  aws_bucket_create(bucket)
  expect_true(aws_bucket_exists(bucket))
})

test_that("aws_bucket_exists", {
  expect_error(aws_bucket_exists())
  expect_error(aws_bucket_exists(5))
  expect_error(aws_bucket_exists(letters))

  bucket <- random_string("bucket")

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

  bucket <- random_string("bucket")
  aws_bucket_create(bucket)
  expect_true(aws_bucket_exists(bucket))
  res <- aws_bucket_delete(bucket, force = TRUE)
  expect_null(res)
  expect_false(aws_bucket_exists(bucket))
})

test_that("aws_bucket_download", {
  expect_error(aws_bucket_download())
  expect_error(aws_bucket_download(""))

  bucket <- random_string("bucket")
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

  bucket <- random_string("bucket")
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
  for (i in replicate(10, random_string("bucket"))) aws_bucket_create(i)

  res <- aws_buckets()

  expect_s3_class(res, "tbl")
  expect_gt(NROW(res), 5)

  buckets_empty()
})

test_that("aws_bucket_tree", {
  expect_error(aws_bucket_tree())
  expect_error(aws_bucket_tree("", 5))

  bucket <- random_string("bucket")
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


# cleanup
buckets_empty()
Sys.unsetenv("AWS_PROFILE")
