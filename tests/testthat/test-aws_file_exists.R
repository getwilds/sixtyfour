test_that("aws_file_exists works", {
  vcr::use_cassette("aws_file_exists", {
    res_true <- aws_file_exists(bucket = "s64-test-2", key = "DESCRIPTION")
    res_false <- aws_file_exists(bucket = "s64-test-2", key = "doesntexist")
  })

  expect_true(res_true)
  expect_false(res_false)
})
