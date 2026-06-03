# List VPC security groups

List VPC security groups

## Usage

``` r
aws_vpc_security_groups(...)
```

## Arguments

- ...:

  named parameters passed on to
  [describe_security_groups](https://www.paws-r-sdk.com/docs/ec2_describe_security_groups/)

## Value

(list) list with security groups, see
[`aws_vpc_security_group()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group.md)
for details

## See also

Other security groups:
[`aws_vpc_sec_group_rules_mod()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sec_group_rules_mod.md),
[`aws_vpc_security_group()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group.md),
[`aws_vpc_security_group_create()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_create.md),
[`aws_vpc_security_group_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_ingress.md),
[`aws_vpc_sg_with_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sg_with_ingress.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
aws_vpc_security_groups()
aws_vpc_security_groups(MaxResults = 6)
}
```
