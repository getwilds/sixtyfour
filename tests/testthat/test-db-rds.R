test_that("aws_db_rds_create", {
  skip_on_ci()
  vcr::use_cassette("aws_db_rds_create", {
    z <- aws_db_rds_create(
      id = "bananas", class = "db.t3.micro",
      security_group_ids = list("sg-0ade14818d03997a4"),
      BackupRetentionPeriod = 0,
      wait = FALSE,
      verbose = FALSE
    )
  })

  # Note: the paws RDS client is just a list of fxns, hard
  # to test it
  expect_type(z, "list")
  expect_type(z[[1]], "closure")
})

test_that("aws_db_rds_client", {
  x <- aws_db_rds_client()
  expect_type(x, "list")
  expect_type(x[[1]], "closure")
  lnames <- names(x)
  expect_equal(lnames[length(lnames)], ".internal")
})

test_that("instance_details", {
  # Recorded with no RDS instances running
  vcr::use_cassette("instance_details", {
    x <- instance_details()
  })

  expect_type(x, "list")
  expect_named(x, c("Marker", "DBInstances"))
})
