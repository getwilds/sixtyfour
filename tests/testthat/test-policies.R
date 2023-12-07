test_that("aws_policy", {
  withr::local_envvar(c("TESTING64" = TRUE))

  vcr::use_cassette("aws_policy", {
    res_name <- aws_policy("ReadOnlyAccess")
    res_arn <- aws_policy("arn:aws:iam::aws:policy/ReadOnlyAccess")
  })

  expect_s3_class(res_arn, "tbl")
  expect_true("PolicyName" %in% names(res_arn))
  expect_true(res_arn$IsAttachable)
  expect_type(res_arn$Description, "character")

  expect_identical(res_name, res_arn)
})

test_that("aws_policy_exists", {
  withr::local_envvar(c("TESTING64" = TRUE))

  vcr::use_cassette("aws_policy_exists", {
    res_true <- aws_policy_exists("ReadOnlyAccess")
    res_false <- aws_policy_exists("Flurrrrb")
  })

  expect_true(res_true)
  expect_false(res_false)
})

test_that("as_policy_arn", {
  withr::local_envvar(c("TESTING64" = TRUE))

  x_name <- as_policy_arn("ReadOnlyAccess")
  x_arn <- as_policy_arn("arn:aws:iam::aws:policy/ReadOnlyAccess")

  expect_type(x_name, "character")
  expect_length(x_name, 1)
  expect_match(x_name, "arn:aws:iam")
  expect_identical(x_name, x_arn)

  expect_error(as_policy_arn("Blarp"), "known")
  expect_error(as_policy_arn(letters), "length")
  expect_error(as_policy_arn(5), "character")
})

test_that("aws_policy_attach", {
  withr::local_envvar(c("TESTING64" = TRUE))

  # FIXME: do setup/teardown for setting up this user if does not exist?
  vcr::use_cassette("aws_policy_attach", {
    user_before <- aws_user("testUser2")
    user_after <- aws_user("testUser2") %>%
      aws_policy_attach("AWSCloudHSMReadOnlyAccess")
  })

  expect_type(user_before, "list")
  expect_type(user_after, "list")

  expect_equal(NROW(user_before$attached_policies), 0)
  expect_equal(NROW(user_after$attached_policies), 1)
})

test_that("aws_policy_detach", {
  withr::local_envvar(c("TESTING64" = TRUE))

  vcr::use_cassette("aws_policy_detach", {
    user_before <- aws_user("testUser2")
    user_after <- aws_user("testUser2") %>%
      aws_policy_detach("AWSCloudHSMReadOnlyAccess")
  })

  expect_type(user_before, "list")
  expect_type(user_after, "list")

  expect_equal(NROW(user_before$attached_policies), 1)
  expect_equal(NROW(user_after$attached_policies), 0)
})
