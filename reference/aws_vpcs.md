# List VPCs

List VPCs

## Usage

``` r
aws_vpcs(...)
```

## Arguments

- ...:

  parameters passed on to
  [describe_vpcs](https://www.paws-r-sdk.com/docs/ec2_describe_vpcs/)

## Value

(list) list with VPCs, see
[`aws_vpc()`](https://getwilds.org/sixtyfour/reference/aws_vpc.md) for
details

## Examples

``` r
if (FALSE) { # interactive() && aws_has_creds()
aws_vpcs()
aws_vpcs(MaxResults = 6)
}
```
