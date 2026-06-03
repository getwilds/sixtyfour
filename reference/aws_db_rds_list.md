# Get information for all RDS instances

Get information for all RDS instances

## Usage

``` r
aws_db_rds_list()
```

## Value

a tibble of instance details; see
<https://www.paws-r-sdk.com/docs/rds_describe_db_instances/> an empty
tibble if no instances found

## See also

Other database:
[`aws_db_cluster_status()`](https://getwilds.org/sixtyfour/reference/aws_db_cluster_status.md),
[`aws_db_instance_status()`](https://getwilds.org/sixtyfour/reference/aws_db_instance_status.md),
[`aws_db_rds_con()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_con.md),
[`aws_db_rds_create()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_create.md),
[`aws_db_redshift_con()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_con.md),
[`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)

## Examples

``` r
if (FALSE) { # interactive()
aws_db_rds_list()
}
```
