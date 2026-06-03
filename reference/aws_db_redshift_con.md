# Get a database connection to Amazon Redshift

Get a database connection to Amazon Redshift

## Usage

``` r
aws_db_redshift_con(
  user,
  pwd,
  id = NULL,
  host = NULL,
  port = NULL,
  dbname = NULL,
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

## Value

an object of class `RedshiftConnection`

## Details

The connection returned is created using
[RPostgres](https://rpostgres.r-dbi.org/)

You can manage Redshift programatically via
[paws::redshift](https://www.paws-r-sdk.com/docs/redshift/)

## See also

Other database:
[`aws_db_cluster_status()`](https://getwilds.org/sixtyfour/reference/aws_db_cluster_status.md),
[`aws_db_instance_status()`](https://getwilds.org/sixtyfour/reference/aws_db_instance_status.md),
[`aws_db_rds_con()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_con.md),
[`aws_db_rds_create()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_create.md),
[`aws_db_rds_list()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_list.md),
[`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(DBI)
library(RPostgres)

con_rshift <- aws_db_redshift_con("<define all params here>")
con_rshift
library(RPostgres)
dbListTables(con_rshift)
dbWriteTable(con_rshift, "mtcars", mtcars)
dbListTables(con_rshift)

library(dplyr)
tbl(con_rshift, "mtcars")
} # }
```
