test_that("wait", {
  expect_type(wait_until(), "closure")

  my_fun <- function(...) {
    "available"
  }

  wait_for_test <- wait_until(
    my_fun,
    "test initializing"
  )

  expect_message(
    (out <- wait_for_test()),
    "Waiting for"
  )
  expect_null(out)
})
