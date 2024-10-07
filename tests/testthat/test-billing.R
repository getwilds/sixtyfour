test_that("aws_billing", {
  stub_request("post", "https://ce.us-east-1.amazonaws.com") |>
    # wi_th(body = list(Metrics = c("UnblendedCost"))) |>
    wi_th(body = fromJSON('{"TimePeriod":{"Start":"2024-10-03","End":"2024-10-05"},"Granularity":"DAILY","Metrics":["UnblendedCost"],"GroupBy":[{"Type":"DIMENSION","Key":"SERVICE"},{"Type":"DIMENSION","Key":"LINKED_ACCOUNT"}]}', FALSE)) |>
    to_return(
      body = jsonlite::minify(response_billing_unblended_1),
      status = 200L,
      headers = list("Content-type" = "application/x-amz-json-1.1")
    )
  stub_request("post", "https://ce.us-east-1.amazonaws.com") |>
    # wi_th(body = list(Metrics = c("BlendedCost"))) |>
    wi_th(body = fromJSON('{"TimePeriod":{"Start":"2024-10-03","End":"2024-10-05"},"Granularity":"DAILY","Metrics":["BlendedCost"],"GroupBy":[{"Type":"DIMENSION","Key":"SERVICE"},{"Type":"DIMENSION","Key":"LINKED_ACCOUNT"}]}', FALSE)) |>
    to_return(
      body = jsonlite::minify(response_billing_blended_1),
      status = 200L,
      headers = list("Content-type" = "application/x-amz-json-1.1")
    )

  enable(quiet = TRUE)

  aws_billing(date_start = "2024-10-03", date_end = "2024-10-05")

  expect_type(start_res, "list")
  expect_type(start_res$job_id, "character")
  expect_type(start_res$info, "character")

  stub_registry_clear()
  disable(quiet = TRUE)
})
