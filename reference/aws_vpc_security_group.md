# Get a security group by ID

Get a security group by ID

## Usage

``` r
aws_vpc_security_group(id, ...)
```

## Arguments

- id:

  (character) The id of the security group. required

- ...:

  named parameters passed on to
  [describe_security_groups](https://www.paws-r-sdk.com/docs/ec2_describe_security_groups/)

## Value

(list) with fields:

- SecurityGroups (list) each security group

  - Description

  - GroupName

  - IpPermissions

  - OwnerId

  - GroupId

  - IpPermissionsEgress

  - Tags

  - VpcId

- NextToken (character) token for paginating

## See also

Other security groups:
[`aws_vpc_sec_group_rules_mod()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sec_group_rules_mod.md),
[`aws_vpc_security_group_create()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_create.md),
[`aws_vpc_security_group_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_ingress.md),
[`aws_vpc_security_groups()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_groups.md),
[`aws_vpc_sg_with_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sg_with_ingress.md)
