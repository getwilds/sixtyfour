test_that("path_s3_parse works", {
  a <- path_s3_parse("s3://s64-test-2/DESCRIPTION")
  expect_type(a, "list")
  expect_named(a, NULL)
  expect_named(a[[1]], c("bucket", "path", "file"))
  expect_equal(a[[1]]$bucket, "s64-test-2")
  expect_equal(a[[1]]$path, "")
  expect_equal(a[[1]]$file, "DESCRIPTION")

  paths <- c(
    "s3://s64-test-2/DESCRIPTION",
    "s3://s64-test-2/stuff.txt",
    "s3://s64-test-2/some/other/path/things.csv"
  )
  b <- path_s3_parse(paths)
  expect_type(b, "list")
  expect_named(b, NULL)
  for (i in b) expect_named(i, c("bucket", "path", "file"))
  for (i in b) expect_equal(i$bucket, "s64-test-2")
  for (i in b) expect_type(i$path, "character")
  for (i in b) expect_type(i$file, "character")
})
