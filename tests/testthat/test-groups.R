skip_if_not(localstack_available(), "LocalStack Not Available")

# create gorup first
the_group <- random_string("group")
withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  if (!aws_group_exists(the_group)) {
    aws_group_create(name = the_group)
  }
)

test_that("aws_group", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- aws_group(the_group)
  )

  expect_type(res, "list")
  expect_s3_class(res$group, "tbl")
  expect_type(res$policies, "character")
  expect_s3_class(res$attached_policies, "tbl")
  expect_equal(NROW(res$group), 1)
})

test_that("aws_group_exists", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      extant <- aws_group_exists(the_group)
      extinct <- aws_group_exists("notathing")
    }
  )

  expect_true(extant)
  expect_false(extinct)
})

test_that("aws_groups", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    listofgroups <- aws_groups()
  )

  expect_s3_class(listofgroups, "tbl")
  expect_equal(listofgroups$GroupName, the_group)
  expect_match(listofgroups$Arn, "arn:aws:iam::")
  expect_equal(NROW(listofgroups), 1)
})

test_that("aws_group_create_and_delete", {
  create_group_group <- random_string("group")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      if (aws_group_exists(create_group_group)) {
        aws_group_delete(create_group_group)
      }
      created_group <- aws_group_create(create_group_group)
    }
  )

  expect_s3_class(created_group, "tbl")
  expect_equal(created_group$GroupName, create_group_group)
  expect_match(created_group$Arn, "arn:aws:iam::")
  expect_equal(NROW(created_group), 1)

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      z <- aws_group_delete(create_group_group)
    }
  )

  expect_type(z, "list")
  expect_length(z, 0)
})

# cleanup
withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  aws_group_delete(the_group)
)
