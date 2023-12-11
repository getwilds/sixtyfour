#' Get a database connection to Amazon RDS
#'
#' JUST MariaDB, MySQL, PostgreSQL FOR NOW!!!!
#'
#' @export
#' @inheritParams aws_db_redshift_con
#' @param engine (character) The engine to use. optional if `user`, `pwd`, and
#' `id` are supplied - otherwise required
#' @details RDS supports: Aurora (both PostgreSQL and MySQL compatible),
#' PostgreSQL, MariaDB, MySQL, Oracle, MS SQL Server
#'
#' If the `engine` you've chosen for your RDS instance is not supported
#' with this function, you can likely connect to it on your own
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
aws_db_rds_con <- function(user, pwd, id = NULL, host = NULL, port = NULL,
                           dbname = NULL, engine = NULL, ...) {
  check_for_pkg("DBI")

  stopifnot("user is required" = !missing(user))
  stopifnot("pwd is required" = !missing(pwd))

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

  DBI::dbConnect(
    which_driver(engine),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = pwd,
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
#' character must be a letter; cn't end with a hyphen or contain two
#' consecutive hyphens. required.
#' @param class (character) required. The compute and memory capacity of the
#' DB instance, for example `db.m5.large`.
#' @param user (character) User name associated with the admin user account for
#' the cluster that is being created.
#' @param pwd (character) Password associated with the admin user account for
#' the cluster that is being created.
#' @param dbname (character) The name of the first database to be created when
#' the cluster is created. default: "dev". additional databases can be created
#' within the cluster
#' @param engine (character) The engine to use. default: "mariadb". required.
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
#' @param ... named parameters passed on to
#' [create_db_instance](https://www.paws-r-sdk.com/docs/rds_create_db_instance/)
#' @note See above link to `create_cluster` docs for details on requirements
#' for each parameter
#' @return a list with methods for interfacing with RDS;
#' see <https://www.paws-r-sdk.com/docs/rds/>
aws_db_rds_create <-
  function(id, class, user, pwd, dbname = "dev",
           engine = "mariadb", storage = 20,
           storage_encrypted = TRUE, security_group_ids = NULL,
           wait = TRUE, verbose = TRUE, ...) {
    aws_db_rds_client()
    env64$rds$create_db_instance(
      DBName = dbname, DBInstanceIdentifier = id,
      Engine = engine, DBInstanceClass = class,
      AllocatedStorage = storage,
      MasterUsername = user, MasterUserPassword = pwd,
      VpcSecurityGroupIds = security_group_ids,
      StorageEncrypted = storage_encrypted,
      ...
    )
    if (wait) {
      wait_for_instance(id)
    }
    if (verbose) info(id, instance_con_info)
    return(env64$rds)
  }

#' Get the `paws` RDS client
#' @export
#' @note returns existing client if found; a new client otherwise
#' @return a list with methods for interfacing with RDS;
#' see <https://www.paws-r-sdk.com/docs/rds/>
aws_db_rds_client <- function() {
  if (is.null(env64$rds)) env64$rds <- paws::rds()
  return(env64$rds)
}

#' Get information for all RDS instances
#' @return a list of instance details
#' @keywords internal
instance_details <- function() {
  aws_db_rds_client()
  instances <- env64$rds$describe_db_instances()
  return(instances)
}

#' Get connection information for all instances
#' @inheritParams aws_db_redshift_create
#' @return a list of cluster details
#' @keywords internal
instance_con_info <- function(id) {
  deets <- instance_details()$DBInstances
  z <- Filter(function(x) x$DBInstanceIdentifier == id, deets)[[1]]
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
