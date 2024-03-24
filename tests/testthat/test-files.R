# skip_on_os(c("windows", "mac"))
skip_if_not(minio_available(), "Minio Not Available")

invisible(env64$s3 <- set_s3_interface("minio"))
buckets_empty()

demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
links_file <- file.path(system.file(), "Meta/links.rds")

test_that("aws_file_upload error behavior", {
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
  res <- aws_file_upload(
    demo_rds_file,
    s3_path("s64-test-2", basename(demo_rds_file)),
    force = TRUE
  )

  expect_type(res, "character")
  expect_length(res, 1)
})

# test_that("aws_file_upload - many files", {
#   aws_file_upload(demo_rds_file, "s3://not-a-bucket/eee.rds"),
#   expect_type(res, "character")
#   expect_length(res, 1)
# })

# cleanup
buckets_empty()
invisible(env64$s3 <- set_s3_interface("aws"))
