#' List VPCs
#' @export
#' @param ... parameters passed on to [describe_vpcs](
#' https://www.paws-r-sdk.com/docs/ec2_describe_vpcs/)
#' @return (list) list with VPCs, see [aws_vpc()] for details
#' @examplesIf interactive() && aws_has_creds()
#' aws_vpcs()
#' aws_vpcs(MaxResults = 6)
aws_vpcs <- function(...) {
  con_ec2()$describe_vpcs(...)
}

#' Get a VPC by id
#' @export
#' @param id (character) The id of the VPC. required
#' @inheritParams aws_vpcs
#' @return (list) with fields:
#' - Vpcs (list) each VPC group
#' - NextToken (character) token for paginating
#'
#' Each element of Vpcs is a list with slots:
#' - CidrBlock
#' - DhcpOptionsId
#' - State
#' - VpcId
#' - OwnerId
#' - InstanceTenancy
#' - Ipv6CidrBlockAssociationSet
#' - CidrBlockAssociationSet
#' - IsDefault
#' - Tags
aws_vpc <- function(id, ...) {
  aws_vpcs(VpcIds = id, ...)
}
