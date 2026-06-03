# Get cluster status

Get cluster status

## Usage

``` r
aws_db_cluster_status(id)
```

## Arguments

- id:

  (character) Cluster identifier. Use this identifier to refer to the
  cluster for any subsequent cluster operations such as deleting or
  modifying. The identifier also appears in the Amazon Redshift console.
  Must be unique for all clusters within a Amazon Web Services account.

## Value

(character) the status of the cluster, e.g., "creating", "available",
"not found"

## See also

Other database:
[`aws_db_instance_status()`](https://getwilds.org/sixtyfour/reference/aws_db_instance_status.md),
[`aws_db_rds_con()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_con.md),
[`aws_db_rds_create()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_create.md),
[`aws_db_rds_list()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_list.md),
[`aws_db_redshift_con()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_con.md),
[`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aws_db_cluster_status(id = "scotts-test-cluster-456")
} # }
```
