# Authorize Security Group Ingress

Authorize Security Group Ingress

## Usage

``` r
aws_vpc_security_group_ingress(id, ip_permissions = NULL, ...)
```

## Arguments

- id:

  (character) security group id. required

- ip_permissions:

  (list) list of persmissions. see link to `paws` docs below or use
  [`ip_permissions_generator()`](https://getwilds.org/sixtyfour/reference/ip_permissions_generator.md)
  to generate the list for this parameter

- ...:

  named parameters passed on to
  [authorize_security_group_ingress](https://www.paws-r-sdk.com/docs/ec2_authorize_security_group_ingress/)

## Value

list with slots:

- Return (boolean)

- SecurityGroupRules (list)

  - SecurityGroupRuleId

  - GroupId

  - GroupOwnerId

  - IsEgress

  - IpProtocol

  - FromPort

  - ToPort

  - CidrIpv4

  - CidrIpv6

  - PrefixListId

  - ReferencedGroupInfo

  - Description

  - Tags

## See also

Other security groups:
[`aws_vpc_sec_group_rules_mod()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sec_group_rules_mod.md),
[`aws_vpc_security_group()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group.md),
[`aws_vpc_security_group_create()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_create.md),
[`aws_vpc_security_groups()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_groups.md),
[`aws_vpc_sg_with_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sg_with_ingress.md)
