#' Get a database connection to Amazon RDS
#'
#' Supports: MariaDB, MySQL, and Postgres
#'
#' @export
#' @inheritParams aws_db_redshift_con
#' @param engine (character) The engine to use. optional if `user`, `pwd`, and
#' `id` are supplied - otherwise required
#' @details RDS supports many databases, but we only provide support for
#' MariaDB, MySQL, and Postgres
#'
#' If the `engine` you've chosen for your RDS instance is not supported
#' with this function, you can likely connect to it on your own
#' @family database
#' @examples \dontrun{
#' con_rds <- aws_db_rds_con("<define all params here>")
#' con_rds
#'
#' library(DBI)
#' library(RMariaDB)
#' dbListTables(con_rds)
#' dbWriteTable(con_rds, "mtcars", mtcars)
#' dbListTables(con_rds)
#' dbReadTable(con_rds, "mtcars")
#'
#' library(dplyr)
#' tbl(con_rds, "mtcars")
#' }
aws_db_rds_con <- function(
    user = NULL, pwd = NULL, id = NULL, host = NULL,
    port = NULL, dbname = NULL, engine = NULL, ...) {
  check_for_pkg("DBI")
  is_class(engine, "character")

  if (!is.null(id)) {
    con_info <- instance_con_info(id)
    host <- con_info$host
    port <- con_info$port
    dbname <- con_info$dbname
    engine <- con_info$engine
  }
  if (any(vapply(list(host, port, dbname, engine), is.null, logical(1)))) {
    stop("`host`, `port`, `dbname`, and `engine` can not be NULL",
      call. = FALSE
    )
  }

  creds <- ui_fetch_secret(user, pwd, engine, id)

  DBI::dbConnect(
    which_driver(engine),
    host = host,
    port = port,
    dbname = dbname,
    user = creds$user,
    password = creds$password,
    ...
  )
}

#' Create an RDS cluster
#'
#' @export
#' @importFrom paws rds
#' @param id (character) required. instance identifier. The identifier for
#' this DB instance. This parameter is stored as a lowercase string.
#' Constraints: must contain from 1 to 63 letters, numbers, or hyphens; first
#' character must be a letter; can't end with a hyphen or contain two
#' consecutive hyphens. required.
#' @param class (character) required. The compute and memory capacity of the
#' DB instance, for example `db.m5.large`.
#' @param user (character) User name associated with the admin user account for
#' the cluster that is being created. If `NULL`, we generate a random user
#' name, see [random_user()]
#' @param pwd (character) Password associated with the admin user account for
#' the cluster that is being created. If `NULL`, we generate a random password
#' with [aws_secrets_pwd()] (which uses the AWS Secrets Manager service)
#' @param dbname (character) The name of the first database to be created when
#' the cluster is created. default: "dev". additional databases can be created
#' within the cluster
#' @param engine (character) The engine to use. default: "mariadb". required.
#' one of: mariadb, mysql, or postgres
#' @param storage (character) The amount of storage in gibibytes (GiB) to
#' allocate for the DB instance. default: 20
#' @param storage_encrypted (logical) Whether the DB instance is encrypted.
#' default: `TRUE`
#' @param security_group_ids (character) VPC security group identifiers; one
#' or more. If none are supplied, you should go into your AWS Redshift
#' dashboard and add the appropriate VPC security group.
#' @param wait (logical) wait for cluster to initialize? default: `TRUE`. If
#' you don't wait (`FALSE`) then there's many operations you can not do
#' until the cluster is available. If `wait=FALSE` use
#' `aws_db_instance_status()` to check on the cluster status.
#' @param verbose (logical) verbose informational output? default: `TRUE`
#' @param aws_secrets (logical) should we manage your database credentials
#' in AWS Secrets Manager? default: `TRUE`
#' @param iam_database_auth (logical) Use IAM database authentication?
#' default: `FALSE`
#' @param ... named parameters passed on to
#' [create_db_instance](https://www.paws-r-sdk.com/docs/rds_create_db_instance/)
#' @details See above link to `create_db_instance` docs for details on
#' requirements for each parameter
#'
#' Note that even though you can use any option for `engine` in this function,
#' we may not provide the ability to connect to the chosen data source
#' in this package.
#' @section Waiting:
#' Note that with `wait = TRUE` this function waits for the instance to be
#' available for returning. That wait can be around 5 - 7 minutes. You can
#' instead set `wait = FALSE` and then check on the status of the instance
#' yourself in the AWS dashboard.
#' @family database
#' @return returns `NULL`, this function called for the side effect of
#' creating an RDS instance
aws_db_rds_create <-
  function(id, class, user = NULL, pwd = NULL, dbname = "dev",
           engine = "mariadb", storage = 20,
           storage_encrypted = TRUE, security_group_ids = NULL,
           wait = TRUE, verbose = TRUE, aws_secrets = TRUE,
           iam_database_auth = FALSE, ...) {
    aws_db_rds_client()
    if (is.null(user)) {
      user <- random_user()
      if (verbose) {
        cli::cli_alert_info("`user` is NULL; created user: {.strong {user}}")
      }
    }
    if (is.null(pwd)) {
      pwd <- aws_secrets_pwd()
      if (verbose) {
        cli::cli_alert_info("`pwd` is NULL; created password: *******")
      }
    }
    security_group_ids <- security_group_handler(security_group_ids, engine)
    env64$rds$create_db_instance(
      DBName = dbname, DBInstanceIdentifier = id,
      Engine = engine, DBInstanceClass = class,
      AllocatedStorage = storage,
      MasterUsername = user, MasterUserPassword = pwd,
      VpcSecurityGroupIds = security_group_ids,
      StorageEncrypted = storage_encrypted,
      EnableIAMDatabaseAuthentication = iam_database_auth,
      ...
    )
    if (wait) {
      wait_for_instance(id)
    }
    if (aws_secrets) {
      if (verbose) cli::cli_alert_info("Uploading user/pwd to secrets manager")
      x <- instance_con_info(id)
      aws_secrets_create(
        name = paste0(id, random_db_id_str()),
        secret = construct_db_secret(
          engine = x$engine,
          host = x$host,
          username = user,
          password = pwd,
          dbname = x$dbname,
          port = x$port
        )
      )
    }
    if (verbose) info(id, instance_con_info, "aws_db_rds_con")
    invisible()
  }

#' Get the `paws` RDS client
#' @export
#' @note returns existing client if found; a new client otherwise
#' @family database
#' @return a list with methods for interfacing with RDS;
#' see <https://www.paws-r-sdk.com/docs/rds/>
aws_db_rds_client <- function() {
  if (is.null(env64$rds)) env64$rds <- paws::rds()
  return(env64$rds)
}

#' Get information for all RDS instances
#' @export
#' @return a list of RDS instance details, see link below for format,
#' with slots:
#' - Marker (for pagination)
#' - DBInstances (each instance; empty list if no instances)
#' @references <https://www.paws-r-sdk.com/docs/describe_db_instances/>
#' @keywords internal
instance_details <- function() {
  aws_db_rds_client()
  env64$rds$describe_db_instances()
}

split_grep <- function(column, split, pattern) {
  grep(glue("^{pattern}$"), strsplit(column, split)[[1]], value = TRUE)
}

#' Get information for all RDS instances
#' @importFrom dplyr select
#' @export
#' @family database
#' @return a tibble of instance details;
#' see <https://www.paws-r-sdk.com/docs/describe_db_instances/>
#' an empty tibble if no instances found
#' @autoglobal
#' @examplesIf interactive()
#' aws_db_rds_list()
aws_db_rds_list <- function() {
  lst <- instance_details()
  dbs <- lst$DBInstances
  if (rlang::is_empty(dbs)) {
    return(tibble())
  }
  map(dbs, \(x) {
    as_tibble(x[c(
      "DBInstanceIdentifier",
      "DBInstanceClass",
      "Engine",
      "DBInstanceStatus",
      "DBName",
      "DbiResourceId",
      "DBInstanceArn"
    )])
  }) %>%
    list_rbind() %>%
    mutate(
      AccountId = split_grep(DBInstanceArn, ":", "^[0-9]+$"),
      Region = split_grep(DBInstanceArn, ":", "^[a-z]+-[a-z]+-[0-9]$")
    ) %>%
    select(-DBInstanceArn)
}

#' Get connection information for all instances
#' @importFrom purrr keep
#' @inheritParams aws_db_redshift_create
#' @return a list of cluster details
#' @keywords internal
instance_con_info <- function(id) {
  deets <- instance_details()$DBInstances
  z <- keep(deets, \(x) x$DBInstanceIdentifier == id)
  if (!length(z)) rlang::abort(glue("Instance identifier {id} not found"))
  z <- z[[1]]
  list(
    host = z$Endpoint$Address,
    port = z$Endpoint$Port,
    dbname = z$DBName,
    engine = z$Engine,
    class = z$DBInstanceClass,
    status = z$DBInstanceStatus
  )
}

#' Get instance status
#' @export
#' @inheritParams aws_db_rds_create
#' @family database
#' @return (character) the status of the instance, e.g., "creating",
#' "available", "not found"
#' @examples \dontrun{
#' aws_db_instance_status(id = "thedbinstance")
#' }
aws_db_instance_status <- function(id) {
  deets <- instance_details()$DBInstances
  instance <- Filter(function(x) x$DBInstanceIdentifier == id, deets)
  if (!length(instance)) {
    warning(glue::glue("instance id '{id}' not found"))
    return("not found")
  }
  instance[[1]]$DBInstanceStatus
}
