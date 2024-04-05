skip_if_not(localstack_available(), "LocalStack Not Available")

Sys.setenv(AWS_PROFILE = "localstack")
buckets_empty()

test_that("aws_s3_policy_doc_create", {
  doc <- aws_s3_policy_doc_create(
    bucket = "s64-test-22",
    action = s3_actions_read()
  )

  expect_s3_class(doc, "json")
  expect_type(unclass(doc), "character")
  expect_identical(
    jsonlite::fromJSON(doc)$Statement$Action[[1]],
    s3_actions_read()
  )
  expect_equal(
    jsonlite::fromJSON(doc)$Version,
    "2012-10-17" # hard-coded version doc number #nolint
  )
})


test_that("six_bucket_add_user - failure behavior", {
  expect_error(
    six_bucket_add_user(
      bucket = "mybucket",
      username = "sam",
      permissions = "notavalidpermission"
    ),
    "permissions must be one of"
  )

  expect_error(
    six_bucket_add_user(
      bucket = "mybucket",
      username = "sam",
      permissions = c("read", "write")
    ),
    "permissions must be length 1"
  )
})

test_that("six_bucket_add_user", {
  user_name <- random_string("user")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    the_user <- aws_user_create(user_name)
  )

  bucket_name <- random_string("bucket")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    aws_bucket_create(bucket_name)
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      withr::with_options(
        list(cli.default_handler = function(...) { }),
        {
          user_added <- six_bucket_add_user(
            bucket = bucket_name,
            username = user_name,
            permissions = "read"
          )
        }
      )
    }
  )

  expect_null(user_added)

  # cleanup
  policy_name <- bucket_to_policy_name(bucket_name, "read")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      if (aws_user_exists(user_name)) {
        aws_user(user_name) %>%
          aws_policy_detach(policy_name)
        aws_user_delete(user_name)
      }
      if (aws_policy_exists(policy_name)) {
        aws_policy_delete(policy_name)
      }
      if (aws_bucket_exists(bucket_name)) {
        aws_bucket_delete(bucket_name, force = TRUE)
      }
    }
  )
})


test_that("six_bucket_permissions", {
  expect_error(six_bucket_permissions("asdf"), "does not exist")

  user1 <- random_string("user")
  user2 <- random_string("user")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      aws_user_create(user1)
      aws_user_create(user2)
    }
  )

  bucket_name <- random_string("bucket")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    aws_bucket_create(bucket_name)
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      withr::with_options(
        list(cli.default_handler = function(...) { }),
        {
          six_bucket_add_user(
            bucket = bucket_name,
            username = user1,
            permissions = "read"
          )
          six_bucket_add_user(
            bucket = bucket_name,
            username = user2,
            permissions = "read"
          )
        }
      )
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- six_bucket_permissions(bucket_name)
  )

  expect_s3_class(res, "tbl")

  # cleanup
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      if (aws_user_exists(user1)) {
        withr::with_options(
          list(cli.default_handler = function(...) { }),
          six_user_delete(user1)
        )
      }
      if (aws_user_exists(user2)) {
        withr::with_options(
          list(cli.default_handler = function(...) { }),
          six_user_delete(user2)
        )
      }
      if (aws_bucket_exists(bucket_name)) {
        aws_bucket_delete(bucket_name, force = TRUE)
      }
    }
  )
})

test_that("six_bucket_remove_user", {
  user_name <- random_string("user")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      aws_user_create(user_name)
    }
  )

  bucket_name <- random_string("bucket")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      aws_bucket_create(bucket_name)
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      withr::with_options(
        list(cli.default_handler = function(...) { }),
        {
          six_bucket_add_user(
            bucket = bucket_name,
            username = user_name,
            permissions = "read"
          )
        }
      )
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      withr::with_options(
        list(cli.default_handler = function(...) { }),
        {
          user_removed <- six_bucket_remove_user(
            bucket = bucket_name,
            username = user_name
          )
        }
      )
    }
  )

  expect_null(user_removed)

  # cleanup
  policy_name <- bucket_to_policy_name(bucket_name, "read")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      if (aws_user_exists(user_name)) {
        withr::with_options(
          list(cli.default_handler = function(...) { }),
          six_user_delete(user_name)
        )
      }
      if (aws_policy_exists(policy_name)) {
        aws_policy_delete(policy_name)
      }
      if (aws_bucket_exists(bucket_name)) {
        aws_bucket_delete(bucket_name, force = TRUE)
      }
    }
  )
})

# cleanup
buckets_empty()
Sys.unsetenv("AWS_PROFILE")
