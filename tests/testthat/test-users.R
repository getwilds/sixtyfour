test_that("aws_users", {
  withr::local_envvar(c("TESTING64" = TRUE))

  vcr::use_cassette("aws_users", {
    res <- aws_users()
  })

  expect_s3_class(res, "tbl")
  expect_gte(NROW(res), 4)
  expect_type(res$UserName, "character")
  expect_true(inherits(res$CreateDate, "POSIXct"))
})

test_that("aws_user", {
  withr::local_envvar(c("TESTING64" = TRUE))

  vcr::use_cassette("aws_user", {
    res <- aws_user()
  })

  expect_type(res, "list")
  expect_s3_class(res$user, "tbl")
  expect_type(res$policies, "character")
  expect_s3_class(res$attached_policies, "tbl")
  expect_type(res$groups, "list")
  expect_equal(NROW(res$user), 1)
})
