# Get a VPC by id

Get a VPC by id

## Usage

``` r
aws_vpc(id, ...)
```

## Arguments

- id:

  (character) The id of the VPC. required

- ...:

  parameters passed on to
  [describe_vpcs](https://www.paws-r-sdk.com/docs/ec2_describe_vpcs/)

## Value

(list) with fields:

- Vpcs (list) each VPC group

- NextToken (character) token for paginating

Each element of Vpcs is a list with slots:

- CidrBlock

- DhcpOptionsId

- State

- VpcId

- OwnerId

- InstanceTenancy

- Ipv6CidrBlockAssociationSet

- CidrBlockAssociationSet

- IsDefault

- Tags
