% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database-rds.R
\name{aws_db_rds_con}
\alias{aws_db_rds_con}
\title{Get a database connection to Amazon RDS}
\usage{
aws_db_rds_con(
  user = NULL,
  pwd = NULL,
  id = NULL,
  host = NULL,
  port = NULL,
  dbname = NULL,
  engine = NULL,
  ...
)
}
\arguments{
\item{user, pwd, host, port, dbname, ...}{named parameters passed on to
\href{https://dbi.r-dbi.org/reference/dbConnect.html}{DBI::dbConnect}. Note that
the \code{user} and \code{pwd} are for your AWS IAM account; and the same as
those you used to create the cluster with \code{\link[=aws_db_redshift_create]{aws_db_redshift_create()}}}

\item{id}{(character) Cluster identifier. If you supply \code{id}, we'll fetch
\code{host}, \code{port}, and \code{dbname}. If \code{id} is not supplied. you have to supply
\code{host}, \code{port}, and \code{dbname}. Refer to this parameter definition in
\code{\link[=aws_db_redshift_create]{aws_db_redshift_create()}} for more details.}

\item{engine}{(character) The engine to use. optional if \code{user}, \code{pwd}, and
\code{id} are supplied - otherwise required}
}
\description{
Supports: MariaDB, MySQL, and Postgres
}
\details{
RDS supports many databases, but we only provide support for
MariaDB, MySQL, and Postgres

If the \code{engine} you've chosen for your RDS instance is not supported
with this function, you can likely connect to it on your own
}
\examples{
\dontrun{
con_rds <- aws_db_rds_con("<define all params here>")
con_rds

library(DBI)
library(RMariaDB)
dbListTables(con_rds)
dbWriteTable(con_rds, "mtcars", mtcars)
dbListTables(con_rds)
dbReadTable(con_rds, "mtcars")

library(dplyr)
tbl(con_rds, "mtcars")
}
}
\seealso{
Other database: 
\code{\link{aws_db_cluster_status}()},
\code{\link{aws_db_instance_status}()},
\code{\link{aws_db_rds_create}()},
\code{\link{aws_db_rds_list}()},
\code{\link{aws_db_redshift_con}()},
\code{\link{aws_db_redshift_create}()}
}
\concept{database}
