# Get instance status

Get instance status

## Usage

``` r
aws_db_instance_status(id)
```

## Arguments

- id:

  (character) required. instance identifier. The identifier for this DB
  instance. This parameter is stored as a lowercase string. Constraints:
  must contain from 1 to 63 letters, numbers, or hyphens; first
  character must be a letter; can't end with a hyphen or contain two
  consecutive hyphens. required.

## Value

(character) the status of the instance, e.g., "creating", "available",
"not found"

## See also

Other database:
[`aws_db_cluster_status()`](https://getwilds.org/sixtyfour/reference/aws_db_cluster_status.md),
[`aws_db_rds_con()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_con.md),
[`aws_db_rds_create()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_create.md),
[`aws_db_rds_list()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_list.md),
[`aws_db_redshift_con()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_con.md),
[`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aws_db_instance_status(id = "thedbinstance")
} # }
```
