test_that("aws_file_exists works", {
  vcr::use_cassette("aws_file_exists", {
    res_true <-
      aws_file_exists(remote_path = s3_path("s64-test-2", "DESCRIPTION"))
    res_false <-
      aws_file_exists(remote_path = s3_path("s64-test-2", "doesntexist"))
  })

  expect_true(res_true)
  expect_false(res_false)
})
