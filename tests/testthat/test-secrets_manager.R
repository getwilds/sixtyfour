skip_if_not(localstack_available(), "LocalStack Not Available")

Sys.setenv(AWS_PROFILE = "localstack")
purge_secrets()

test_that("aws_secrets_list", {
  res <- aws_secrets_list()

  expect_type(res, "list")
  expect_named(res, c("SecretList", "NextToken"))
  expect_equal(length(res$SecretList), 0)
})

test_that("aws_secrets_create", {
  secret_name <- "Testing6789"

  x <- aws_secrets_create(
    name = secret_name,
    secret = '{"username":"bear","password":"apple"}',
    description = "A note about the secret"
  )

  expect_type(x, "list")
  expect_named(x, c("ARN", "Name", "VersionId", "ReplicationStatus"))
  expect_equal(x$Name, secret_name)
  expect_match(x$ARN, "arn:aws")
  expect_type(x$VersionId, "character")

  # cleanup
  purge_secrets()
})

test_that("aws_secrets_get", {
  secret_name_thename <- "TestingTheThing1"
  the_secret <- '{"username":"bear","password":"apple"}'
  aws_secrets_create(
    name = secret_name_thename,
    secret = the_secret,
    description = "A note about the secret"
  )

  res <- aws_secrets_get(secret_name_thename)

  expect_type(res, "list")
  expect_named(
    res,
    c(
      "ARN",
      "Name",
      "VersionId",
      "SecretBinary",
      "SecretString",
      "VersionStages",
      "CreatedDate"
    )
  )
  expect_equal(res$Name, secret_name_thename)
  expect_match(res$ARN, "arn:aws")
  expect_equal(res$SecretString, the_secret)

  secret_name_arn <- "TestingAnotherThing1"
  the_secret <- '{"username":"deer","password":"bananas"}'
  x <- aws_secrets_create(
    name = secret_name_arn,
    secret = the_secret,
    description = "A quick note about the secret"
  )

  res <- aws_secrets_get(x$ARN)

  expect_type(res, "list")
  expect_named(
    res,
    c(
      "ARN",
      "Name",
      "VersionId",
      "SecretBinary",
      "SecretString",
      "VersionStages",
      "CreatedDate"
    )
  )
  expect_equal(res$Name, secret_name_arn)
  expect_match(res$ARN, "arn:aws")
  expect_equal(res$SecretString, the_secret)

  # cleanup
  purge_secrets()
})

test_that("aws_secrets_all", {
  x <- aws_secrets_all()
  expect_type(x, "list")
  expect_equal(NROW(x), 0)

  aws_secrets_create(
    name = "TestingTheThing1",
    secret = '{"username":"bear","password":"apple"}',
    description = "A note about the secret"
  )
  x <- aws_secrets_all()
  expect_type(x, "list")
  expect_equal(NROW(x), 1)
})

test_that("construct_db_secret", {
  secret_redshift <- construct_db_secret(
    "redshift",
    dbname = "hello",
    port = 5439
  )
  secret_mariadb <- construct_db_secret(
    "mariadb",
    dbname = "world",
    port = 3306
  )
  secret_postgresql_raw <- construct_db_secret(
    "postgresql",
    dbname = "bears",
    port = 5432,
    as = "raw"
  )

  expect_type(secret_redshift, "character")
  expect_match(secret_redshift, "5439")

  expect_type(secret_mariadb, "character")
  expect_match(secret_mariadb, "3306")

  expect_type(secret_postgresql_raw, "raw")
  expect_match(rawToChar(secret_postgresql_raw), "5432")
})


# cleanup
purge_secrets()
Sys.unsetenv("AWS_PROFILE")
