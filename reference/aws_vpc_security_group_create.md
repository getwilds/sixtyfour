# Create a security group

Create a security group

## Usage

``` r
aws_vpc_security_group_create(
  name,
  engine = "mariadb",
  description = NULL,
  vpc_id = NULL,
  tags = NULL,
  ...
)

aws_vpc_security_group_delete(id = NULL, name = NULL, ...)
```

## Arguments

- name:

  (character) The name of the new secret. required for `*_create` and
  optional for `*_delete`

- engine:

  (character) The engine to use. default: "mariadb". required. one of:
  mariadb, mysql, or postgres

- description:

  (character) The description of the secret. optional

- vpc_id:

  (character) a VPC id. optional. if not supplied your default VPC is
  used. To get your VPCs, see
  [`aws_vpcs()`](https://getwilds.org/sixtyfour/reference/aws_vpcs.md)

- tags:

  (character) The tags to assign to the security group. optional

- ...:

  named parameters passed on to
  [create_security_group](https://www.paws-r-sdk.com/docs/ec2_create_security_group/)

- id:

  (character) The id of the security group. optional. provide `id` or
  `name`

## Value

(list) with fields:

- GroupId (character)

- Tags (list)

## See also

Other security groups:
[`aws_vpc_sec_group_rules_mod()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sec_group_rules_mod.md),
[`aws_vpc_security_group()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group.md),
[`aws_vpc_security_group_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_ingress.md),
[`aws_vpc_security_groups()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_groups.md),
[`aws_vpc_sg_with_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sg_with_ingress.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# create security group
grp_name1 <- random_string("vpcsecgroup")
x <- aws_vpc_security_group_create(
  name = grp_name1,
  description = "Testing security group creation"
)

grp_name2 <- random_string("vpcsecgroup")
aws_vpc_security_group_create(name = grp_name2)

grp_name3 <- random_string("vpcsecgroup")
aws_vpc_security_group_create(
  name = grp_name3,
  tags = list(
    list(
      ResourceType = "security-group",
      Tags = list(
        list(
          Key = "sky",
          Value = "blue"
        )
      )
    )
  )
)

# add ingress
aws_vpc_security_group_ingress(
  id = x$GroupId,
  ip_permissions = ip_permissions_generator("mariadb")
)

# cleanup
aws_vpc_security_group_delete(name = grp_name1)
aws_vpc_security_group_delete(name = grp_name2)
aws_vpc_security_group_delete(name = grp_name3)
} # }
```
