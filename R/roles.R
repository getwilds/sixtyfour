#' Role list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_roles/)
#' @keywords internal
#' @note leaves out: PermissionsBoundary, Tags, RoleLastUsed
role_list_tidy <- function(x) {
  vars <- c(
    "RoleName", "RoleId", "Path", "Arn", "CreateDate",
    "AssumeRolePolicyDocument", "Description", "MaxSessionDuration"
  )
  tidy_generator(vars)(x)
}

#' List roles
#'
#' @export
#' @param ... parameters passed on to the `paws`
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_roles/) method
#' @return A tibble with information about roles
#' @examples \dontrun{
#' aws_roles()
#' }
aws_roles <- function(...) {
  paginate_aws(env64$iam$list_roles, "Roles") %>% role_list_tidy()
}

#' Get a role
#'
#' @export
#' @param name (character) the role name
#' @return a tibble with role details
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_role/>
#' @autoglobal
#' @examples \dontrun{
#' aws_role(name = "OrganizationAccountSecurityRole")
#' }
aws_role <- function(name) {
  env64$iam$get_role(name)$Role %>%
    list(.) %>%
    role_list_tidy()
}
