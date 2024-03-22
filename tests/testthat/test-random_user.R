test_that("random_user", {
  # character
  expect_type(random_user(), "character")
  # non-empty
  expect_true(nzchar(random_user()))
  # all no longer than 16 length
  for (i in replicate(100, random_user())) {
    expect_true(nchar(i) <= 16)
  }
})
