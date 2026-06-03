# Create an RDS cluster

Create an RDS cluster

## Usage

``` r
aws_db_rds_create(
  id,
  class,
  user = NULL,
  pwd = NULL,
  dbname = "dev",
  engine = "mariadb",
  storage = 20,
  storage_encrypted = TRUE,
  security_group_ids = NULL,
  wait = TRUE,
  verbose = TRUE,
  aws_secrets = TRUE,
  iam_database_auth = FALSE,
  ...
)
```

## Arguments

- id:

  (character) required. instance identifier. The identifier for this DB
  instance. This parameter is stored as a lowercase string. Constraints:
  must contain from 1 to 63 letters, numbers, or hyphens; first
  character must be a letter; can't end with a hyphen or contain two
  consecutive hyphens. required.

- class:

  (character) required. The compute and memory capacity of the DB
  instance, for example `db.m5.large`.

- user:

  (character) User name associated with the admin user account for the
  cluster that is being created. If `NULL`, we generate a random user
  name, see
  [`random_user()`](https://getwilds.org/sixtyfour/reference/random_string.md)

- pwd:

  (character) Password associated with the admin user account for the
  cluster that is being created. If `NULL`, we generate a random
  password with
  [`aws_secrets_pwd()`](https://getwilds.org/sixtyfour/reference/aws_secrets_pwd.md)
  (which uses the AWS Secrets Manager service)

- dbname:

  (character) The name of the first database to be created when the
  cluster is created. default: "dev". additional databases can be
  created within the cluster

- engine:

  (character) The engine to use. default: "mariadb". required. one of:
  mariadb, mysql, or postgres

- storage:

  (character) The amount of storage in gibibytes (GiB) to allocate for
  the DB instance. default: 20

- storage_encrypted:

  (logical) Whether the DB instance is encrypted. default: `TRUE`

- security_group_ids:

  (character) VPC security group identifiers; one or more. If none are
  supplied, you should go into your AWS Redshift dashboard and add the
  appropriate VPC security group.

- wait:

  (logical) wait for cluster to initialize? default: `TRUE`. If you
  don't wait (`FALSE`) then there's many operations you can not do until
  the cluster is available. If `wait=FALSE` use
  [`aws_db_instance_status()`](https://getwilds.org/sixtyfour/reference/aws_db_instance_status.md)
  to check on the cluster status.

- verbose:

  (logical) verbose informational output? default: `TRUE`

- aws_secrets:

  (logical) should we manage your database credentials in AWS Secrets
  Manager? default: `TRUE`

- iam_database_auth:

  (logical) Use IAM database authentication? default: `FALSE`

- ...:

  named parameters passed on to
  [create_db_instance](https://www.paws-r-sdk.com/docs/rds_create_db_instance/)

## Value

returns `NULL`, this function called for the side effect of creating an
RDS instance

## Details

See above link to `create_db_instance` docs for details on requirements
for each parameter

Note that even though you can use any option for `engine` in this
function, we may not provide the ability to connect to the chosen data
source in this package.

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
[`aws_db_rds_list()`](https://getwilds.org/sixtyfour/reference/aws_db_rds_list.md),
[`aws_db_redshift_con()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_con.md),
[`aws_db_redshift_create()`](https://getwilds.org/sixtyfour/reference/aws_db_redshift_create.md)
