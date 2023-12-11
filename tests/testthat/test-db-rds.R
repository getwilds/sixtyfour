test_that("aws_db_rds_create", {
  vcr::use_cassette("aws_db_rds_create", {
    z <- aws_db_rds_create(
      id = "aninstance", class = "db.t3.micro",
      user = "xxxx", pwd = "zzzzzz",
      security_group_ids = list("sg-xxxxxxxxx"),
      wait = FALSE, verbose = FALSE
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
