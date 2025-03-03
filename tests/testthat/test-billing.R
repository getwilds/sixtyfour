test_that("aws_billing", {
  withr::with_envvar(
    c(
      "AWS_REGION" = "us-east-1",
      "AWS_ACCESS_KEY_ID" = "aaaaa",
      "AWS_SECRET_ACCESS_KEY" = "bbbbbb"
    ),
    {
      withr::with_package("webmockr", {
        ## Two stubs below are needed b/c aws_billing makes two http requests
        ##  in one function call, for unblended and blended costs
        stub_request("post", "https://ce.us-east-1.amazonaws.com") |>
          wi_th(body = including(list(Metrics = list("UnblendedCost")))) |>
          to_return(
            body = jsonlite::minify(response_billing_unblended_1),
            status = 200L,
            headers = list("Content-type" = "application/x-amz-json-1.1")
          )

        stub_request("post", "https://ce.us-east-1.amazonaws.com") |>
          wi_th(body = including(list(Metrics = list("BlendedCost")))) |>
          to_return(
            body = jsonlite::minify(response_billing_blended_1),
            status = 200L,
            headers = list("Content-type" = "application/x-amz-json-1.1")
          )

        enable(quiet = TRUE)

        out <- aws_billing(date_start = "2024-10-03", date_end = "2024-10-05")

        expect_s3_class(out, "tbl")
        expect_equal(NCOL(out), 6)
        expect_equal(sort(unique(out$id)), c("blended", "unblended"))

        stub_registry_clear()
        disable(quiet = TRUE)
      })
    }
  )
})
