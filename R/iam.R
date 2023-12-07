#' Get the `paws` Identity and Access Management (IAM) client
#'
#' @export
#' @return a list with methods for interfacing with IAM;
#' see <https://www.paws-r-sdk.com/docs/iam/>
#' @examples aws_iam_client()
aws_iam_client <- function() {
  return(env64$iam)
}
