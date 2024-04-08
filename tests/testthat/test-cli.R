test_that("cli package helpers", {
  expect_error(cli_info())
  expect_message(cli_info("Hello World"))

  expect_error(cli_warning())
  expect_message(cli_warning("Hello World"))

  expect_error(cli_success())
  expect_message(cli_success("Hello World"))
})
