test_that("aws_db_redshift_create", {
  skip_if_not(aws_has_creds())
  vcr::use_cassette("aws_db_redshift_create", {
    z <- aws_db_redshift_create(
      id = "abdfghtyu",
      user = "floppy", pwd = "xxxxxxxxxx",
      security_group_ids = list("sg-xxxxxxxxx"),
      wait = FALSE, verbose = FALSE
    )
  })

  # retuns NULL b/c invisible()
  expect_null(z)
})

test_that("aws_db_redshift_client", {
  x <- con_redshift()
  expect_s3_class(x, "sixtyfour_client")
  expect_type(x, "list")
  expect_type(x[[1]], "closure")
  lnames <- names(x)
  expect_equal(lnames[length(lnames)], ".internal")
})

test_that("cluster_details", {
  # Recorded with no Redshift instances running
  skip_if_not(aws_has_creds())
  vcr::use_cassette("cluster_details", {
    x <- cluster_details()
  })

  expect_type(x, "list")
  expect_named(x, c("Marker", "Clusters"))
})
