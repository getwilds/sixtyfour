test_that("aws_secrets_list", {
  vcr::use_cassette("aws_secrets_list", {
    purge_secrets()
    res <- aws_secrets_list()
  })

  expect_type(res, "list")
  expect_named(res, c("SecretList", "NextToken"))
  expect_equal(length(res$SecretList), 0)
})

# Sys.sleep(5) # sleep to allow purge_secrets to finish aws side of deletion

test_that("aws_secrets_create", {
  secret_name <- "Testing6789"

  vcr::use_cassette("aws_secrets_create", {
    x <- aws_secrets_create(
      name = secret_name,
      secret = '{"username":"bear","password":"apple"}',
      description = "A note about the secret"
    )
  })

  expect_type(x, "list")
  expect_named(x, c("ARN", "Name", "VersionId", "ReplicationStatus"))
  expect_equal(x$Name, secret_name)
  expect_match(x$ARN, "arn:aws")
  expect_type(x$VersionId, "character")

  # cleanup
  aws_secrets_delete(secret_name, ForceDeleteWithoutRecovery = TRUE)
})


test_that("aws_secrets_get", {
  vcr::use_cassette("aws_secrets_get_by_name", {
    secret_name_thename <- "TestingTheThing1"
    the_secret <- '{"username":"bear","password":"apple"}'
    aws_secrets_create(
      name = secret_name_thename,
      secret = the_secret,
      description = "A note about the secret"
    )

    res <- aws_secrets_get(secret_name_thename)
  })

  expect_type(res, "list")
  expect_named(res, c(
    "ARN", "Name", "VersionId", "SecretBinary",
    "SecretString", "VersionStages", "CreatedDate"
  ))
  expect_equal(res$Name, secret_name_thename)
  expect_match(res$ARN, "arn:aws")
  expect_equal(res$SecretString, the_secret)

  vcr::use_cassette("aws_secrets_get_by_arn", {
    secret_name_arn <- "TestingAnotherThing1"
    the_secret <- '{"username":"deer","password":"bananas"}'
    x <- aws_secrets_create(
      name = secret_name_arn,
      secret = the_secret,
      description = "A quick note about the secret"
    )

    res <- aws_secrets_get(x$ARN)
  })

  expect_type(res, "list")
  expect_named(res, c(
    "ARN", "Name", "VersionId", "SecretBinary",
    "SecretString", "VersionStages", "CreatedDate"
  ))
  expect_equal(res$Name, secret_name_arn)
  expect_match(res$ARN, "arn:aws")
  expect_equal(res$SecretString, the_secret)

  # cleanup
  aws_secrets_delete(secret_name_thename, ForceDeleteWithoutRecovery = TRUE)
  aws_secrets_delete(secret_name_arn, ForceDeleteWithoutRecovery = TRUE)
})
