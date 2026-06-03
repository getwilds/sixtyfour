# Get a database connection to Amazon RDS

Supports: MariaDB, MySQL, and Postgres

## Usage

``` r
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
```

## Arguments

- user, pwd, host, port, dbname, ...:

  named parameters passed on to
  [DBI::dbConnect](https://dbi.r-dbi.org/reference/dbConnect.html). Note
  that the `user` and `pwd` are for your AWS IAM account; and the same
  as those you used to create the cluster with
  [`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)

- id:

  (character) Cluster identifier. If you supply `id`, we'll fetch
  `host`, `port`, and `dbname`. If `id` is not supplied. you have to
  supply `host`, `port`, and `dbname`. Refer to this parameter
  definition in
  [`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)
  for more details.

- engine:

  (character) The engine to use. optional if `user`, `pwd`, and `id` are
  supplied - otherwise required

## Value

an S4 object that inherits from `DBIConnection`

## Details

RDS supports many databases, but we only provide support for MariaDB,
MySQL, and Postgres

If the `engine` you've chosen for your RDS instance is not supported
with this function, you can likely connect to it on your own

## See also

Other database:
[`aws_db_cluster_status()`](https://getwilds.org/sixtyfour/reference/aws_db_cluster_status.md),
[`aws_db_instance_status()`](https://getwilds.org/sixtyfour/reference/aws_db_instance_status.md),
[`aws_db_rds_create()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_create.md),
[`aws_db_rds_list()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_list.md),
[`aws_db_redshift_con()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_con.md),
[`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
