library("vcr")
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_sensitive_data = list(
    "<<aws_region>>" = Sys.getenv("AWS_REGION")
  ),
  filter_request_headers = list(
    Authorization = "redacted",
    "X-Amz-Content-Sha256" = "redacted"
  ),
  filter_response_headers = list(
    "x-amz-id-2" = "redacted",
    "x-amz-request-id" = "redacted"
  )
))
vcr::check_cassette_names()
