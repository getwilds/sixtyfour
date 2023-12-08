#' Get a database connection to Amazon Redshift
#'
#' @export
#' @param id (character) Cluster identifier. If you supply `id`, we'll fetch
#' `host`, `port`, and `dbname`. If `id` is not supplied. you have to supply
#' `host`, `port`, and `dbname`. Refer to this parameter definition in
#' [aws_db_redshift_create()] for more details.
#' @param user,pwd,host,port,dbname,... named parameters passed on to
#' [DBI::dbConnect](https://dbi.r-dbi.org/reference/dbconnect). Note that
#' the `user` and `pwd` are for your AWS IAM account; and the same as
#' those you used to create the cluster with [aws_db_redshift_create()]
#' @details
#' The connection returned is created using
#' [RPostgres](https://rpostgres.r-dbi.org/)
#'
#' You can manage Redshift programatically via
#' [paws::redshift](https://www.paws-r-sdk.com/docs/redshift/)
#' @return an object of class `RedshiftConnection`
#' @examples \dontrun{
#' library(DBI)
#' library(RPostgres)
#'
#' con_rshift <- aws_db_redshift_con("<define all params here>")
#' con_rshift
#' library(RPostgres)
#' dbListTables(con_rshift)
#' dbWriteTable(con_rshift, "mtcars", mtcars)
#' dbListTables(con_rshift)
#'
#' library(dplyr)
#' tbl(con_rshift, "mtcars")
#' }
aws_db_redshift_con <- function(user, pwd, id = NULL, host = NULL, port = NULL,
                                dbname = NULL, ...) {
  check_for_pkg("DBI")
  check_for_pkg("RPostgres")

  stopifnot("user is required" = !missing(user))
  stopifnot("pwd is required" = !missing(pwd))

  if (!is.null(id)) {
    con_info <- cluster_con_info(id)
    host <- con_info$host
    port <- con_info$port
    dbname <- con_info$dbname
  }
  if (any(vapply(list(host, port, dbname), is.null, logical(1)))) {
    stop("`host`, `port`, and `dbname` can not be NULL", call. = FALSE)
  }

  DBI::dbConnect(
    RPostgres::Redshift(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = pwd,
    ...
  )
}

#' Create a Redshift cluster
#'
#' @export
#' @importFrom paws redshift
#' @param id (character) Cluster identifier. Use this identifier to refer to
#' the cluster for any subsequent cluster operations such as deleting or
#' modifying. The identifier also appears in the Amazon Redshift console.
#' Must be unique for all clusters within a Amazon Web Services account.
#' @param user (character) User name associated with the admin user account for
#' the cluster that is being created.
#' @param pwd (character) Password associated with the admin user account for
#' the cluster that is being created.
#' @param dbname (character) The name of the first database to be created when
#' the cluster is created. default: "dev". additional databases can be created
#' within the cluster
#' @param cluster_type (character) The type of the cluster: "single-node" or
#' "multi-node" (default).
#' @param node_type (character) The node type to be provisioned for the cluster.
#' defaul: "dc2.large"
#' @param number_nodes (integer/numeric) number of nodes; for multi-node
#' cluster type, this must be 2 or greater. default: 2
#' @param security_group_ids (character) VPC security group identifiers; one
#' or more. If none are supplied, you should go into your AWS Redshift
#' dashboard and add the appropriate VPC security group.
#' @param wait (logical) wait for cluster to initialize? default: `TRUE`. If
#' you don't wait (`FALSE`) then there's many operations you can not do
#' until the cluster is available. If `wait=FALSE` use
#' `aws_db_cluster_status()` to check on the cluster status.
#' @param ... named parameters passed on to
#' [create_cluster](https://www.paws-r-sdk.com/docs/redshift_create_cluster/)
#' @note See above link to `create_cluster` docs for details on requirements
#' for each parameter
#' @return a list with methods for interfacing with Redshift;
#' see <https://www.paws-r-sdk.com/docs/redshift/>
aws_db_redshift_create <-
  function(id, user, pwd, dbname = "dev", cluster_type = "multi-node",
           node_type = "dc2.large", number_nodes = 2,
           security_group_ids = NULL, wait = TRUE, ...) {
    aws_db_redshift_client()
    env64$redshift$create_cluster(
      DBName = dbname, ClusterIdentifier = id,
      ClusterType = cluster_type, NodeType = node_type,
      MasterUsername = user, MasterUserPassword = pwd,
      NumberOfNodes = number_nodes,
      VpcSecurityGroupIds = security_group_ids,
      ...
    )
    if (wait) {
      wait_for_cluster(id)
    }
    return(env64$redshift)
  }

#' Get the `paws` Redshift client
#' @export
#' @return a list with methods for interfacing with Redshift;
#' see <https://www.paws-r-sdk.com/docs/redshift/>
aws_db_redshift_client <- function() {
  if (is.null(env64$redshift)) env64$redshift <- paws::redshift()
  return(env64$redshift)
}

#' Get information for all clusters
#' @return a list of cluster details
#' @keywords internal
cluster_details <- function() {
  clusters <- env64$redshift$describe_clusters()
  return(clusters)
}

#' Get connection information for all clusters
#' @inheritParams aws_db_redshift_create
#' @return a list of cluster details
#' @keywords internal
cluster_con_info <- function(id) {
  deets <- cluster_details()$Clusters
  z <- Filter(function(x) x$ClusterIdentifier == id, deets)[[1]]
  list(host = z$Endpoint$Address, port = z$Endpoint$Port, dbname = z$DBName)
}

#' Get cluster status
#' @export
#' @inheritParams aws_db_redshift_create
#' @return (character) the status of the cluster, e.g., "creating",
#' "available", "not found"
#' @examples \dontrun{
#' aws_db_cluster_status(id = "scotts-test-cluster-456")
#' }
aws_db_cluster_status <- function(id) {
  deets <- cluster_details()$Clusters
  cluster <- Filter(function(x) x$ClusterIdentifier == id, deets)
  if (!length(cluster)) {
    warning(glue::glue("cluster id '{id}' not found"))
    return("not found")
  }
  cluster[[1]]$ClusterStatus
}
