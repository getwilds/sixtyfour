# Create a Redshift cluster

Create a Redshift cluster

## Usage

``` r
aws_db_redshift_create(
  id,
  user,
  pwd,
  dbname = "dev",
  cluster_type = "multi-node",
  node_type = "dc2.large",
  number_nodes = 2,
  security_group_ids = NULL,
  wait = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- id:

  (character) Cluster identifier. Use this identifier to refer to the
  cluster for any subsequent cluster operations such as deleting or
  modifying. The identifier also appears in the Amazon Redshift console.
  Must be unique for all clusters within a Amazon Web Services account.

- user:

  (character) User name associated with the admin user account for the
  cluster that is being created. This is the username for your IAM
  account

- pwd:

  (character) Password associated with the admin user account for the
  cluster that is being created. This is the password for your IAM
  account

- dbname:

  (character) The name of the first database to be created when the
  cluster is created. default: "dev". additional databases can be
  created within the cluster

- cluster_type:

  (character) The type of the cluster: "single-node" or "multi-node"
  (default).

- node_type:

  (character) The node type to be provisioned for the cluster. defaul:
  "dc2.large"

- number_nodes:

  (integer/numeric) number of nodes; for multi-node cluster type, this
  must be 2 or greater. default: 2

- security_group_ids:

  (character) VPC security group identifiers; one or more. If none are
  supplied, you should go into your AWS Redshift dashboard and add the
  appropriate VPC security group.

- wait:

  (logical) wait for cluster to initialize? default: `TRUE`. If you
  don't wait (`FALSE`) then there's many operations you can not do until
  the cluster is available. If `wait=FALSE` use
  [`aws_db_cluster_status()`](https://getwilds.org/sixtyfour/reference/aws_db_cluster_status.md)
  to check on the cluster status.

- verbose:

  (logical) verbose informational output? default: `TRUE`

- ...:

  named parameters passed on to
  [create_cluster](https://www.paws-r-sdk.com/docs/redshift_create_cluster/)

## Value

returns `NULL`, this function called for the side effect of creating an
Redshift instance

## Note

See above link to `create_cluster` docs for details on requirements for
each parameter

## Waiting

Note that with `wait = TRUE` this function waits for the instance to be
available for returning. That wait can be around 5 - 7 minutes. You can
instead set `wait = FALSE` and then check on the status of the instance
yourself in the AWS dashboard.

## See also

Other database:
[`aws_db_cluster_status()`](https://getwilds.org/sixtyfour/reference/aws_db_cluster_status.md),
[`aws_db_instance_status()`](https://getwilds.org/sixtyfour/reference/aws_db_instance_status.md),
[`aws_db_rds_con()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_con.md),
[`aws_db_rds_create()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_create.md),
[`aws_db_rds_list()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_list.md),
[`aws_db_redshift_con()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_con.md)
