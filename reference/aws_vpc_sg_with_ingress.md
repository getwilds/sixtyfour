# Get a security group with one ingress rule based on the engine

Get a security group with one ingress rule based on the engine

## Usage

``` r
aws_vpc_sg_with_ingress(engine)
```

## Arguments

- engine:

  (character) The engine to use. default: "mariadb". required. one of:
  mariadb, mysql, postgres, or redshift

## Value

(character) security group ID

## Details

Adds an ingress rule specific to the `engine` supplied (port changes
based on the engine), and your IP address. To create your own security
group and ingress rules see
[`aws_vpc_security_group_create()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_create.md)
and
[`aws_vpc_security_group_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_ingress.md)

## See also

Other security groups:
[`aws_vpc_sec_group_rules_mod()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sec_group_rules_mod.md),
[`aws_vpc_security_group()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group.md),
[`aws_vpc_security_group_create()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_create.md),
[`aws_vpc_security_group_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_ingress.md),
[`aws_vpc_security_groups()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_groups.md)
