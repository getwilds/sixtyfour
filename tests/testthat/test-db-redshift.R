skip("skipping until secrets stuff done")

test_that("aws_db_redshift_create", {
  vcr::use_cassette("aws_db_redshift_create", {
    z <- aws_db_redshift_create(
      id = "abdfghtyu",
      user = "floppy", pwd = "xxxxxxxxxx",
      security_group_ids = list("sg-xxxxxxxxx"),
      wait = FALSE, verbose = FALSE
    )
  })

  # Note: the paws Redshift client is just a list of fxns, hard
  # to test it
  expect_type(z, "list")
  expect_type(z[[1]], "closure")
})

test_that("aws_db_redshift_client", {
  x <- aws_db_redshift_client()
  expect_type(x, "list")
  expect_type(x[[1]], "closure")
  lnames <- names(x)
  expect_equal(lnames[length(lnames)], ".internal")
})

test_that("cluster_details", {
  # Recorded with no Redshift instances running
  vcr::use_cassette("cluster_details", {
    x <- cluster_details()
  })

  expect_type(x, "list")
  expect_named(x, c("Marker", "Clusters"))
})
