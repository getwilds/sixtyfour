skip_on_ci()
skip_if_not(localstack_available(), "LocalStack Not Available")

# setup
withr::with_envvar(
  c("AWS_PROFILE" = "localstack"),
  {
    st8ment1 <- aws_policy_statement("iam:GetUser", "*")
    st8ment2 <- aws_policy_statement("s3:ListAllMyBuckets", "*")
    doc <- aws_policy_document_create(st8ment1, st8ment2)
    test_policy_name <- random_string("policy")
    if (aws_policy_exists(test_policy_name)) aws_policy_delete(test_policy_name)
    aws_policy_create(test_policy_name, document = doc)
  }
)

test_that("aws_policy_list_entities", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    polents_empty <- aws_policy_list_entities(test_policy_name)
  )

  expect_s3_class(polents_empty, "tbl")
  expect_equal(NROW(polents_empty), 0)

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      user <- random_user()
      aws_user_create(user)
      aws_user(user) %>% aws_policy_attach(test_policy_name)
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    polents_non_empty <- aws_policy_list_entities(test_policy_name)
  )

  expect_s3_class(polents_non_empty, "tbl")
  expect_gt(NROW(polents_non_empty), 0)
  expect_named(polents_non_empty, c("type", "name", "id"))

  # cleanup
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      withr::with_options(
        list(cli.default_handler = function(...) { }),
        six_user_delete(user)
      )
      aws_policy_delete(test_policy_name)
    }
  )
})

test_that("aws_policy", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      res_name <- aws_policy("ReadOnlyAccess")
      res_arn <- aws_policy("arn:aws:iam::aws:policy/ReadOnlyAccess")
    }
  )

  expect_s3_class(res_arn, "tbl")
  expect_true("PolicyName" %in% names(res_arn))
  expect_true(is.na(res_arn$IsAttachable))
  expect_true(is.na(res_arn$Description))

  expect_identical(res_name, res_arn)
})

test_that("aws_policy_exists", {
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      res_true <- aws_policy_exists("ReadOnlyAccess")
      res_false <- aws_policy_exists("Flurrrrb")
    }
  )

  expect_true(res_true)
  expect_false(res_false)
})

test_that("as_policy_arn", {
  x_name <- as_policy_arn("ReadOnlyAccess")
  x_arn <- as_policy_arn("arn:aws:iam::aws:policy/ReadOnlyAccess")

  expect_type(x_name, "character")
  expect_length(x_name, 1)
  expect_match(x_name, "arn:aws:iam")
  expect_identical(x_name, x_arn)

  expect_match(as_policy_arn("Blarp"), "Blarp")

  expect_error(as_policy_arn(letters), "length")
  expect_error(as_policy_arn(5), "character")
})

test_that("aws_policy_attach", {
  policy_name <- "AWSCloudHSMReadOnlyAccess"
  user1 <- "useratfwdqpi"

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      aws_user_create(user1)
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      user_before <- aws_user(user1)
      user_after <- aws_user(user1) %>%
        aws_policy_attach(policy_name)
    }
  )

  expect_type(user_before, "list")
  expect_type(user_after, "list")

  expect_equal(NROW(user_before$attached_policies), 0)
  expect_equal(NROW(user_after$attached_policies), 1)

  # cleanup
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      if (aws_user_exists(user1)) {
        aws_user(user1) %>%
          aws_policy_detach(policy_name)
        aws_user_delete(user1)
      }
    }
  )
})

test_that("aws_policy_detach", {
  policy_name <- "AmazonS3ReadOnlyAccess"
  user <- random_user()

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      aws_user_create(user)
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      aws_user(user) %>%
        aws_policy_attach(policy_name)
    }
  )

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      user_before <- aws_user(user)
      user_after <- aws_user(user) %>%
        aws_policy_detach(policy_name)
    }
  )

  expect_type(user_before, "list")
  expect_type(user_after, "list")

  expect_equal(NROW(user_before$attached_policies), 1)
  expect_equal(NROW(user_after$attached_policies), 0)

  # cleanup
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      if (aws_user_exists(user)) {
        aws_user_delete(user)
      }
    }
  )
})

test_that("aws_policy_document_create", {
  doc1 <- aws_policy_document_create(
    aws_policy_statement(
      action = "rds-db:connect",
      resource = resource_rds("*", "*")
    )
  )
  doc1lst <- jsonlite::fromJSON(doc1, FALSE)

  expect_type(doc1, "character")
  expect_s3_class(doc1, "json")
  expect_named(doc1lst, c("Version", "Statement"))
  expect_named(doc1lst$Statement[[1]], c("Effect", "Action", "Resource"))
  expect_equal(doc1lst$Statement[[1]]$Effect, "Allow")
  expect_equal(doc1lst$Statement[[1]]$Action, "rds-db:connect")
})

test_that("aws_policy_create", {
  my_doc <- aws_policy_document_create(
    aws_policy_statement(
      action = c("s3:ListAllMyBuckets", "s3-object-lambda:*"),
      resource = "*"
    )
  )

  policy_name <- "MyTestPolicy"

  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      polisee <- aws_policy_create(policy_name, document = my_doc)
    }
  )

  expect_type(polisee, "list")
  expect_named(polisee, "Policy")
  expect_equal(polisee$Policy$PolicyName, policy_name)

  # cleanup
  withr::with_envvar(
    c("AWS_PROFILE" = "localstack"),
    {
      if (aws_policy_exists(policy_name)) {
        aws_policy_delete(policy_name)
      }
    }
  )
})
