skip_if_not(localstack_available(), "LocalStack Not Available")

## FIXME: something wrong with aws_users when using localstack
## perhaps having to do with using purrr?
## perhaps b/c with_envvar only goes down one level of the stack?
# test_that("aws_users", {
#   withr::local_options(c("paws.log_level" = 3L))
#   withr::with_envvar(
#     c("AWS_PROFILE" = "localstack"),
#     res <- aws_users()
#   )
#   expect_s3_class(res, "tbl")
#   expect_gte(NROW(res), 1)
#   expect_equal(res$UserName, user)
#   expect_type(res$UserName, "character")
#   expect_true(inherits(res$CreateDate, "POSIXct"))
# })

# create user first
the_user <- random_user()
withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  if (!aws_user_exists(the_user)) {
    aws_user_create(the_user)
  }
)

test_that("aws_user", {
  # FIXME: behavior for aws_user is different for localstack
  # when hitting actual AWS the current user is determined from the AWS
  # Access Key, but there doesn't appear to be a way to do that
  # with localstack, but perhaps there is?
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- aws_user(the_user)
  )

  expect_type(res, "list")
  expect_s3_class(res$user, "tbl")
  expect_type(res$policies, "character")
  expect_s3_class(res$attached_policies, "tbl")
  expect_type(res$groups, "list")
  expect_equal(NROW(res$user), 1)
})

test_that("aws_user_exists", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- aws_user_exists(the_user)
  )

  expect_type(res, "logical")
  expect_true(res)
})

test_that("aws_user_create", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"), {
      a_user <- random_user()
      res <- aws_user_create(a_user)
    }
  )

  expect_s3_class(res, "tbl")
  expect_equal(res$UserName, a_user)

  # cleanup
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    aws_user_delete(a_user)
  )
})

test_that("aws_user_delete", {
  # create first
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"), {
      delete_user <- random_user()
      res <- aws_user_create(delete_user)
    }
  )

  # user exists
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    delete_user_before <- aws_user(delete_user)
  )

  expect_type(delete_user_before, "list")
  expect_equal(delete_user_before$user$UserName, delete_user)

  # then delete
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"), {
      res_del <- aws_user_delete(delete_user)
    }
  )

  expect_type(res_del, "list")
  expect_length(res_del, 0)

  # now user does not exist
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    expect_error(aws_user(delete_user), "cannot be found")
  )
})

test_that("aws_user_access_key", {
  # create user
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"), {
      key_user <- random_user()
      aws_user_create(key_user)
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    withr::with_options(
      list(cli.default_handler = function(...) { }),
      keys <- aws_user_access_key(key_user)
    )
  )

  expect_null(keys)

  # cleanup
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    aws_user_delete(key_user)
  )
})


# cleanup
withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  aws_user_delete(the_user)
)
