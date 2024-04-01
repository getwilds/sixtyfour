skip_on_ci()

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
  # setup (freeze user and bucket names to match test fixtures)
  ## create user
  # user_name <- random_string("user") #nolint
  user_name <- "userbuqoexlg"
  vcr::use_cassette("six_bucket_add_user_setup_user", {
    the_user <- aws_user_create(user_name)
  })
  ## create bucket
  # bucket_name <- random_string("bucket") #nolint
  bucket_name <- "buckethixsfyzj"
  vcr::use_cassette("six_bucket_add_user_setup_bucket", {
    aws_bucket_create(bucket_name)
  })

  # the tests
  vcr::use_cassette("six_bucket_add_user", {
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
  })

  expect_null(user_added)

  # cleanup
  policy_name <- bucket_to_policy_name(bucket_name, "read")
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
})


test_that("six_bucket_permissions", {
  expect_error(six_bucket_permissions("asdf"), "does not exist")

  # setup
  # user_name <- random_string("user") #nolint
  user1 <- "userioubcghd"
  user2 <- "userbqaczysg"
  vcr::use_cassette("six_bucket_permissions_setup_users", {
    aws_user_create(user1)
    aws_user_create(user2)
  })
  # bucket_name <- random_string("bucket") #nolint
  bucket_name <- "bucketihvmjysp"
  vcr::use_cassette("six_bucket_permissions_setup_bucket", {
    aws_bucket_create(bucket_name)
  })

  # the function
  vcr::use_cassette("six_bucket_permissions", {
    res <- six_bucket_permissions(bucket_name)
  })

  expect_s3_class(res, "tbl")

  # cleanup
  if (aws_user_exists(user1)) {
    aws_user_delete(user1)
  }
  if (aws_user_exists(user2)) {
    aws_user_delete(user2)
  }
  if (aws_bucket_exists(bucket_name)) {
    aws_bucket_delete(bucket_name, force = TRUE)
  }
})


test_that("six_bucket_remove_user", {
  # setup (freeze user and bucket names to match test fixtures)
  ## create user
  # user_name <- random_string("user") #nolint
  user_name <- "userauzdiwhk"
  vcr::use_cassette("six_bucket_remove_user_setup_user", {
    the_user <- aws_user_create(user_name)
  })
  ## create bucket
  # bucket_name <- random_string("bucket") #nolint
  bucket_name <- "bucketvanoxfrh"
  vcr::use_cassette("six_bucket_remove_user_setup_bucket", {
    aws_bucket_create(bucket_name)
  })

  # the tests
  vcr::use_cassette("six_bucket_remove_user_setup_add_user", {
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
  })

  vcr::use_cassette("six_bucket_remove_user", {
    withr::with_options(
      list(cli.default_handler = function(...) { }),
      {
        user_removed <- six_bucket_remove_user(
          bucket = bucket_name,
          username = user_name
        )
      }
    )
  })

  expect_null(user_removed)

  # cleanup
  policy_name <- bucket_to_policy_name(bucket_name, "read")
  if (aws_user_exists(user_name)) {
    aws_user_delete(user_name)
  }
  if (aws_policy_exists(policy_name)) {
    aws_policy_delete(policy_name)
  }
  if (aws_bucket_exists(bucket_name)) {
    aws_bucket_delete(bucket_name, force = TRUE)
  }
})
