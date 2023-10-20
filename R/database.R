#' Get a database connection to Amazon Redshift
#'
#' @export
#' @param host,port,dbname,user,pwd,... named parameters passed on to
#' [DBI::dbConnect](https://dbi.r-dbi.org/reference/dbconnect)
#' @details
#' The connection returned is created using
#' [RPostgres](https://rpostgres.r-dbi.org/)
#'
#' You can manage Redshift programatically via
#' [paws::redshift](https://www.paws-r-sdk.com/docs/redshift/)
#' @examples \dontrun{
#' library(DBI)
#' library(RPostgres)
#'
#' con <- aws_db_redshift_con()
#' con
#'
#' con <- aws_db_rds_con()
#' con
#' }
aws_db_redshift_con <- function(host, port, dbname, user, pwd, ...) {
	check_for_pkg("DBI")
	check_for_pkg("RPostgres")
	# DBI::dbConnect(RPostgres::Redshift())
	DBI::dbConnect(
		RPostgres::Postgres(),
		host = host,
		port = port,
		dbname = dbname,
    user = user,
    password = pwd,
    ...
	)
}

#' Get a database connection to Amazon RDS
#'
#' @export
#' @param ... named parameters passed on to
#' [DBI::dbConnect](https://dbi.r-dbi.org/reference/dbconnect)
#' @notes RDS supports: Aurora (both PostgreSQL and MySQL compatible),
#' PostgreSQL, MariaDB, MySQL, Orace, MS SQL Server
aws_db_rds_con <- function(...) {
	check_for_pkg("DBI")
	check_for_pkg("RPostgres")
	DBI::dbConnect(RPostgres::Postgres(), ...)
}

#' Create a Redshift cluster
#'
#' !NOT READY YET!
#'
#' @export
#' @importFrom paws redshift
#' @param id (character) Cluster identifier. Use this identifier to refer to the
#' cluster for any subsequent cluster operations such as deleting or modifying.
#' The identifier also appears in the Amazon Redshift console. Must be unique
#' for all clusters within an Amazon Web Services account.
#' @param user (character) User name associated with the admin user account for
#' the cluster that is being created.
#' @param pwd (character) Password associated with the admin user account for
#' the cluster that is being created.
#' @param db_name (character) The name of the first database to be created when
#' the cluster is created. default: "dev"
#' @param cluster_type (character) The type of the cluster: "single-node" or
#' "multi-node" (default).
#' @param node_type (character) The node type to be provisioned for the cluster.
#' defaul: "dc2.large"
#' @param number_nodes (integer/numeric) number of nodes; for multi-node
#' cluster type, this must be 2 or greater. default: 2
#' @param ... named parameters passed on to
#' [create_cluster](https://www.paws-r-sdk.com/docs/redshift_create_cluster/)
#' @note See above link to `create_cluster` docs for details on requirements
#' for each parameter
#' @return a redshift class from `paws` with methods on the object;
#' see <https://www.paws-r-sdk.com/docs/redshift/>
aws_db_redshift_create <- function(id, user, pwd, db_name = "dev",
	cluster_type = "multi-node", node_type = "dc2.large", number_nodes = 2,
	...) {

	redshift <- paws::redshift()
	redshift$create_cluster(DBName = db_name, ClusterIdentifier = id,
		ClusterType = cluster_type, NodeType = node_type,
		MasterUsername = user, MasterUserPassword = pwd,
		NumberOfNodes = number_nodes, ...)
	return(redshift)
}
