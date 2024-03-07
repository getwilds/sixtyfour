#' Get the `paws` EC2 client - primarily for usage of VPC security groups
#' @export
#' @note returns existing client if found; a new client otherwise
#' @family security groups
#' @return a list with methods for interfacing with EC2;
#' see <https://www.paws-r-sdk.com/docs/ec2/>
aws_ec2_client <- function() {
  if (is.null(env64$ec2)) env64$ec2 <- paws::ec2()
  return(env64$ec2)
}
