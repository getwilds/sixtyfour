skip_on_ci()

test_that("aws_policy_list_entities", {
  vcr::use_cassette("aws_policy_list_entities_empty", {
    polents_empty <- aws_policy_list_entities("S3ReadOnlyAccessMyBucket")
  })

  vcr::use_cassette("aws_policy_list_entities_non_empty", {
    polents_non_empty <- aws_policy_list_entities("S3ReadOnlyAccessS64Test22")
  })

  expect_s3_class(polents_empty, "tbl")
  expect_s3_class(polents_non_empty, "tbl")
  expect_equal(NROW(polents_empty), 0)
  expect_gt(NROW(polents_non_empty), 0)
})

test_that("aws_policy", {
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
  vcr::use_cassette("aws_policy_exists", {
    res_true <- aws_policy_exists("ReadOnlyAccess")
    res_false <- aws_policy_exists("Flurrrrb")
  })

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
  vcr::use_cassette("aws_policy_attach_setup_users", {
    aws_user_create(user1)
  })

  vcr::use_cassette("aws_policy_attach", {
    user_before <- aws_user(user1)
    user_after <- aws_user(user1) %>%
      aws_policy_attach(policy_name)
  })

  expect_type(user_before, "list")
  expect_type(user_after, "list")

  expect_equal(NROW(user_before$attached_policies), 0)
  expect_equal(NROW(user_after$attached_policies), 1)

  # cleanup
  if (aws_user_exists(user1)) {
    aws_user(user1) %>%
      aws_policy_detach(policy_name)
    aws_user_delete(user1)
  }
})

test_that("aws_policy_detach", {
  policy_name <- "AmazonS3ReadOnlyAccess"

  user <- "userbnwaqdif"
  vcr::use_cassette("aws_policy_detach_setup_user", {
    aws_user_create(user)
  })

  vcr::use_cassette("aws_policy_detach_setup_attach", {
    aws_user(user) %>%
      aws_policy_attach(policy_name)
  })

  vcr::use_cassette("aws_policy_detach", {
    user_before <- aws_user(user)
    user_after <- aws_user(user) %>%
      aws_policy_detach(policy_name)
  })

  expect_type(user_before, "list")
  expect_type(user_after, "list")

  expect_equal(NROW(user_before$attached_policies), 1)
  expect_equal(NROW(user_after$attached_policies), 0)

  # cleanup
  if (aws_user_exists(user)) {
    aws_user_delete(user)
  }
})

test_that("aws_policy_document_create", {
  doc1 <- aws_policy_document_create(
    region = "us-east-2",
    account_id = "1234567890",
    resource_id = "*",
    user = "*",
    action = "rds-db:connect"
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
    region = "us-east-2",
    account_id = "1234567890",
    resource_id = "db-ABCDEFGHIJKL01234",
    user = c("jane_doe", "mary_roe"),
    action = "s3:ListAllMyBuckets"
  )

  policy_name <- "MyTestPolicy"

  vcr::use_cassette("aws_policy_create", {
    polisee <- aws_policy_create(policy_name, document = my_doc)
  })

  expect_type(polisee, "list")
  expect_named(polisee, "Policy")
  expect_equal(polisee$Policy$PolicyName, policy_name)

  # cleanup
  if (aws_policy_exists(policy_name)) {
    aws_policy_delete(policy_name)
  }
})
