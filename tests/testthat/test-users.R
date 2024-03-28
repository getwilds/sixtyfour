skip_if_not(localstack_available(), "LocalStack Not Available")

# FIXME: something wrong with aws_users when using localstack
# perhaps having to do with using purrr?
# test_that("aws_users", {
#   withr::local_options(c("paws.log_level" = 3L))
#   withr::with_envvar(
#     c("AWS_PROFILE" = "localstack"),
#     res <- aws_users()
#   )

#   expect_s3_class(res, "tbl")
#   expect_gte(NROW(res), 1)
#   expect_equal(res$UserName, "scott")
#   expect_type(res$UserName, "character")
#   expect_true(inherits(res$CreateDate, "POSIXct"))
# })

test_that("aws_user", {
  # withr::local_options(c("paws.log_level" = 3L))

  # FIXME: behavior for aws_user is different for localstack
  # when hitting actual AWS the current user is determined from the AWS
  # Access Key, but there doesn't appear to be a way to do that
  # with localstack, but perhaps there is?
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- aws_user("scott")
  )

  expect_type(res, "list")
  expect_s3_class(res$user, "tbl")
  expect_type(res$policies, "character")
  expect_s3_class(res$attached_policies, "tbl")
  expect_type(res$groups, "list")
  expect_equal(NROW(res$user), 1)
})

test_that("aws_user_exists", {
  # withr::local_options(c("paws.log_level" = 3L))
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- aws_user_exists("scott")
  )

  expect_type(res, "logical")
  expect_true(res)
})

test_that("aws_user_create", {
  # withr::local_options(c("paws.log_level" = 3L))
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"), {
      user <- random_user()
      res <- aws_user_create(user)
    }
  )

  expect_s3_class(res, "tbl")
  expect_equal(res$UserName, user)

  # cleanup
  aws_user_delete(user)
})
