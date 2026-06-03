# Modify security group rules

Modify security group rules

## Usage

``` r
aws_vpc_sec_group_rules_mod(id, rules, ...)
```

## Arguments

- id:

  (character) security group id. required

- rules:

  list of rules to add/modify on the security group `id`. required

- ...:

  named parameters passed on to
  [modify_security_group_rules](https://www.paws-r-sdk.com/docs/ec2_modify_security_group_rules/)

## Value

list. if successful then `list(Return=TRUE)`

## See also

Other security groups:
[`aws_vpc_security_group()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group.md),
[`aws_vpc_security_group_create()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_create.md),
[`aws_vpc_security_group_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_ingress.md),
[`aws_vpc_security_groups()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_groups.md),
[`aws_vpc_sg_with_ingress()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sg_with_ingress.md)

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
# create a security group
a_grp_name <- random_string("vpcsecgroup")
x <- aws_vpc_security_group_create(name = a_grp_name)
x

# add an inbound rule
my_rule <- aws_vpc_security_group_ingress(
  id = x$GroupId,
  ip_permissions = ip_permissions_generator("mariadb")
)
my_rule

# modify the rule
rule_id <- my_rule$SecurityGroupRules[[1]]$SecurityGroupRuleId
fields_to_keep <- c(
  "IpProtocol", "FromPort", "ToPort", "CidrIpv4",
  "CidrIpv6", "PrefixListId", "Description"
)
rule_old <- my_rule$SecurityGroupRules[[1]]
rule_new <- rule_old[fields_to_keep]
rule_new$Description <- "Modified description"

aws_vpc_sec_group_rules_mod(
  id = x$GroupId,
  rules = list(
    SecurityGroupRuleId = rule_id,
    SecurityGroupRule = rule_new
  )
)

# cleanup
aws_vpc_security_group_delete(name = a_grp_name)
}
```
