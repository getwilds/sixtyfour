test_that("aws_db_rds_create", {
  skip_on_ci()
  skip_if_not(running_local_only_tests())
  skip_if_not(aws_has_creds())
  vcr::use_cassette("aws_db_rds_create", {
    z <- aws_db_rds_create(
      id = "bananas2",
      class = "db.t3.micro",
      security_group_ids = list("sg-0ade14818d03997a4"),
      BackupRetentionPeriod = 0,
      wait = FALSE,
      verbose = FALSE
    )
  })

  # retuns NULL b/c invisible()
  expect_null(z)
})

test_that("aws_db_rds_client", {
  x <- con_rds()
  expect_s3_class(x, "sixtyfour_client")
  expect_type(x, "list")
  expect_type(x[[1]], "closure")
  lnames <- names(x)
  expect_equal(lnames[length(lnames)], ".internal")
})

test_that("instance_details", {
  # Recorded with no RDS instances running
  skip_if_not(aws_has_creds())
  vcr::use_cassette("instance_details", {
    x <- instance_details()
  })

  expect_type(x, "list")
  expect_named(x, c("Marker", "DBInstances"))
})
