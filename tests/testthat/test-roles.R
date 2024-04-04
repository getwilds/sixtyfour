skip_if_not(localstack_available(), "LocalStack Not Available")

# create user first
the_role <- random_string("role")
withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  if (!aws_role_exists(the_role)) {
    role_name <- "MyRole"
    trust_policy <- list(
      Version = "2012-10-17",
      Statement = list(
        list(
          Effect = "Allow",
          Principal = list(
            Service = "lambda.amazonaws.com"
          ),
          Action = "sts:AssumeRole"
        )
      )
    )
    doc <- jsonlite::toJSON(trust_policy, auto_unbox = TRUE)
    desc <- "My test role"
    aws_role_create(
      name = the_role,
      assume_role_policy_document = doc,
      description = desc
    )
  }
)

test_that("aws_role", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- aws_role(the_role)
  )

  expect_type(res, "list")
  expect_s3_class(res$role, "tbl")
  expect_type(res$policies, "character")
  expect_s3_class(res$attached_policies, "tbl")
  expect_equal(NROW(res$role), 1)
})

test_that("aws_role_exists", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    res <- aws_role_exists(the_role)
  )

  expect_true(res)
})

test_that("aws_roles", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    listofroles <- aws_roles()
  )

  expect_s3_class(listofroles, "tbl")
  expect_equal(listofroles$RoleName, the_role)
  expect_match(listofroles$Arn, "arn:aws:iam::")
  expect_equal(NROW(listofroles), 1)
})

test_that("aws_role_create", {
  create_role_role <- random_string("role")
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"), {
      if (aws_role_exists(create_role_role)) {
        aws_role_delete(create_role_role)
      }
      role_name <- "TestRole"
      trust_policy <- list(
        Version = "2012-10-17",
        Statement = list(
          list(
            Effect = "Allow",
            Principal = list(
              Service = "lambda.amazonaws.com"
            ),
            Action = "sts:AssumeRole"
          )
        )
      )
      doc <- jsonlite::toJSON(trust_policy, auto_unbox = TRUE)
      desc <- "A testing role"
      created_role <- aws_role_create(
        name = create_role_role,
        assume_role_policy_document = doc,
        description = desc
      )
    }
  )

  expect_s3_class(created_role, "tbl")
  expect_equal(created_role$RoleName, create_role_role)
  expect_match(created_role$Arn, "arn:aws:iam::")
  expect_equal(NROW(created_role), 1)

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"), {
      z <- aws_role_delete(create_role_role)
    }
  )

  expect_type(z, "list")
  expect_length(z, 0)
})

# cleanup
withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  aws_role_delete(the_role)
)
