#' s3fs connection
#' @export
#' @details we set `refresh=TRUE` on [s3fs::s3_file_system()] so that
#' you can change the s3 interface within an R session
#'
#' You can toggle the interface set for one of minio, localstack, aws.
#' See [connections] for more information.
#' @return An S3 list with class 'sixtyfour_client'
#' @seealso [paws_clients]
#' @examplesIf aws_has_creds()
#' con <- con_s3fs()
#' con
#' con_s3fs()$file_copy
con_s3fs <- function() {
  profile <- Sys.getenv("AWS_PROFILE")
  if (profile == "minio") {
    con <- s3fs::s3_file_system(
      aws_access_key_id = Sys.getenv("MINIO_USER", MINIO_USER_PWD),
      aws_secret_access_key = Sys.getenv("MINIO_PWD", MINIO_USER_PWD),
      endpoint = Sys.getenv("MINIO_ENDPOINT", MINIO_ENDPOINT),
      refresh = TRUE
    )
  } else if (profile == "localstack") {
    con <- s3fs::s3_file_system(
      aws_access_key_id =
        Sys.getenv("LOCALSTACK_KEY", LOCALSTACK_KEY),
      aws_secret_access_key =
        Sys.getenv("LOCALSTACK_SECRET", LOCALSTACK_SECRET),
      endpoint =
        Sys.getenv("LOCALSTACK_ENDPOINT", LOCALSTACK_ENDPOINT),
      refresh = TRUE
    )
  } else {
    con <- s3fs::s3_file_system(
      aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
      aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      region_name = Sys.getenv("AWS_REGION"),
      refresh = TRUE
    )
  }
  as_64_con(con, "s3", profile)
}

#' `paws` connection factory
#' @param service (character) any service supported by \pkg{paws}
#' @return a function for returning the client for the service in
#' the `service` parameter
#' @keywords internal
con_factory <- function(service) {
  function() {
    svc <- utils::getFromNamespace(service, ns = "paws")
    profile <- Sys.getenv("AWS_PROFILE", "aws")
    if (profile == "minio") {
      con <- svc(
        credentials = list(
          creds = list(
            access_key_id = Sys.getenv("MINIO_USER", MINIO_USER_PWD),
            secret_access_key = Sys.getenv("MINIO_PWD", MINIO_USER_PWD)
          )
        ),
        endpoint = Sys.getenv("MINIO_ENDPOINT", MINIO_ENDPOINT)
      )
    } else if (profile == "localstack") {
      con <- svc(
        credentials = list(
          creds = list(
            access_key_id =
              Sys.getenv("LOCALSTACK_KEY", LOCALSTACK_KEY),
            secret_access_key =
              Sys.getenv("LOCALSTACK_SECRET", LOCALSTACK_SECRET)
          )
        ),
        endpoint = Sys.getenv("LOCALSTACK_ENDPOINT", LOCALSTACK_ENDPOINT)
      )
    } else {
      con <- svc(region = NULL)
    }
    as_64_con(con, service, profile)
  }
}

as_64_con <- function(con, service, profile) {
  structure(con,
    class = "sixtyfour_client",
    service = service,
    profile = profile
  )
}

#' @export
print.sixtyfour_client <- function(x, ...) {
  cli::cli_text("{.cls {class(x)}}")
  cli::cli_text("{.strong package} paws")
  cli::cli_text("{.strong service:} {attr(x, 'service')}")
  cli::cli_text("{.strong profile:} {attr(x, 'profile')}")
  cli::cli_text("{.strong no. of methods:} {length(x)}")
  cli::cli_text("{.emph call a method via $method_name}")
  invisible(x)
}

#' Get a `paws` client for a service
#'
#' @export
#' @aliases paws_clients
#' @seealso [con_s3fs()]
#' @details Toggles the credentials used based on the environment
#' variable `AWS_PROFILE` for one of: minio, localstack, aws.
#'
#' If `AWS_PROFILE` is "minio" then we set the following in the
#' credentials for the connection:
#' - `access_key_id` uses env var `MINIO_USER`, with default "minioadmin"
#' - `secret_access_key` uses env var `MINIO_PWD`, with default "minioadmin"
#' - `endpoint` uses env var `MINIO_ENDPOINT`, with default
#' "http://127.0.0.1:9000"
#'
#' If `AWS_PROFILE` is "localstack" then we set the following in the
#' credentials for the connection:
#' - `access_key_id` uses env var `LOCALSTACK_KEY`, with a default
#' string which is essentially ignored. you do not need to set the
#' `LOCALSTACK_KEY` env var. However, if you want to set an account
#' ID for your Localstack you can set the env var and it will be used.
#' see <https://docs.localstack.cloud/references/credentials/>
#' - `secret_access_key` uses env var `LOCALSTACK_SECRET`, with a default
#' string which is ignored; and any value you set for `LOCALSTACK_SECRET`
#' will be ignored by Localstack as well. see
#' <https://docs.localstack.cloud/references/credentials/>
#' - `endpoint` uses env var `LOCALSTACK_ENDPOINT`. You can set this to
#' the URL for where your Localstack is running at. Default is
#' `http://localhost.localstack.cloud:4566`
#'
#' If `AWS_PROFILE` is not set, set to "aws", or anything else (other
#' than "localstack") then we don't set any credentials internally, but
#' `paws` will gather any credentials you've set via env vars, config
#' files, etc.-
#'
#' @return
#' - `con_s3`: a list with methods for interfacing with S3;
#' <https://www.paws-r-sdk.com/docs/s3/>
#' - `con_iam`: a list with methods for interfacing with IAM;
#' <https://www.paws-r-sdk.com/docs/iam/>
#' - `con_sm`: a list with methods for interfacing with Secrets Manager;
#' <https://www.paws-r-sdk.com/docs/secretsmanager/>
#' - `con_ec2`: a list with methods for interfacing with EC2;
#' <https://www.paws-r-sdk.com/docs/ec2/>
#' - `con_rds`: a list with methods for interfacing with RDS;
#' <https://www.paws-r-sdk.com/docs/rds/>
#' - `con_redshift`: a list with methods for interfacing with Redshift;
#' <https://www.paws-r-sdk.com/docs/redshift/>
#' - `con_ce`: a list with methods for interfacing with Cost Explorer;
#' <https://www.paws-r-sdk.com/docs/costexplorer/>
#'
#' @examplesIf aws_has_creds()
#' z <- con_iam()
#' z
#'
#' withr::with_envvar(
#'   c("AWS_PROFILE" = "localstack"),
#'   con_iam()
#' )
#' withr::with_envvar(
#'   c("AWS_PROFILE" = "minio"),
#'   con_s3()
#' )
con_iam <- con_factory("iam")

#' @export
#' @rdname con_iam
con_s3 <- con_factory("s3")

#' @export
#' @rdname con_iam
con_sm <- con_factory("secretsmanager")

#' @export
#' @rdname con_iam
con_ec2 <- con_factory("ec2")

#' @export
#' @rdname con_iam
con_rds <- con_factory("rds")

#' @export
#' @rdname con_iam
con_redshift <- con_factory("redshift")

#' @export
#' @rdname con_iam
con_ce <- con_factory("costexplorer")
