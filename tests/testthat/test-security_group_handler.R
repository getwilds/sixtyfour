test_that("security_group_handler", {
  # missing `id` param
  expect_error(security_group_handler())
  # if `id` given and not `NULL`, returns itself
  an_id <- 123
  expect_equal(security_group_handler(an_id), an_id)
  # if `id` given and IS `NULL`, errors b/c `engine` missing
  expect_error(security_group_handler(NULL))
  # if `id` given, engine value not supported
  expect_error(security_group_handler(NULL, engine = "asdff"))
})
