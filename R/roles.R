#' Role list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_roles/)
#' @keywords internal
#' @note leaves out: PermissionsBoundary, Tags, RoleLastUsed,
#' MaxSessionDuration
role_list_tidy <- function(x) {
  vars <- c(
    "RoleName", "RoleId", "Path", "Arn", "CreateDate",
    "Description", "AssumeRolePolicyDocument"
  )
  tidy_generator(vars)(x)
}

#' List roles
#'
#' @export
#' @param ... parameters passed on to the `paws`
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_roles/) method
#' @family roles
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
#' @return a named list with slots for:
#' - role (tibble)
#' - policies (character)
#' - attached_policies (tibble)
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_role/>;
#' also includes policies and attached policies by calling `list_role_policies`
#' and `list_attached_role_policies`
#' @autoglobal
#' @family roles
#' @examplesIf interactive()
#' res <- aws_role(name = "OrganizationAccountSecurityRole")
#' res
#' res$role
#' res$policies
#' res$attached_policies
#'
#' aws_role("AWSServiceRoleForCloudTrail")
#' aws_role("AWSServiceRoleForRedshift")
aws_role <- function(name) {
  df <- env64$iam$get_role(name)$Role %>%
    list(.) %>%
    role_list_tidy()
  list(
    role = df,
    policies = policies("role", name),
    attached_policies = policies_attached("role", name)
  )
}

#' Create a role
#'
#' @export
#' @param name (character) A role name. required
#' @param path (character) The path for the role name. optional.
#' If it is not included, it defaults to a slash (/).
#' @param assume_role_policy_document (character) The trust relationship policy
#' document that grants an entity permission to assume the role. json as string.
#' required
#' @param description (character) a description fo the role. optional
#' @param max_session_duration (character) The maximum session duration
#' (in seconds) that you want to set for the specified role. optional
#' @param permission_boundary (character) The ARN of the managed policy
#' that is used to set the permissions boundary for the role. optional
#' @param tags (list) A list of tags that you want to attach to the new user.
#' optional
#' @return A tibble with information about the role created
#' @details See <https://www.paws-r-sdk.com/docs/iam_create_role/>
#' docs for details on the parameters
#' @family roles
#' @examples \dontrun{
#' role_name <- "MyRole"
#' trust_policy <- list(
#'   Version = "2012-10-17",
#'   Statement = list(
#'     list(
#'       Effect = "Allow",
#'       Principal = list(
#'         Service = "lambda.amazonaws.com"
#'       ),
#'       Action = "sts:AssumeRole"
#'     )
#'   )
#' )
#' doc <- jsonlite::toJSON(trust_policy, auto_unbox = TRUE)
#' desc <- "My test role"
#' z <- aws_role_create(role_name,
#'   assume_role_policy_document = doc,
#'   description = desc
#' )
#' z
#' # attach a policy
#' z %>% aws_policy_attach("AWSLambdaBasicExecutionRole")
#' }
aws_role_create <- function(
    name, assume_role_policy_document, path = NULL,
    description = NULL, max_session_duration = NULL, permission_boundary = NULL,
    tags = NULL) {
  env64$iam$create_role(
    Path = path,
    RoleName = name,
    AssumeRolePolicyDocument = assume_role_policy_document,
    Description = description,
    MaxSessionDuration = max_session_duration,
    PermissionsBoundary = permission_boundary,
    Tags = tags
  ) %>%
    role_list_tidy()
}

#' Delete a role
#'
#' @export
#' @inheritParams aws_role_create
#' @return an empty list
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_role/>
#' docs for more details
#' @family roles
#' @examples \dontrun{
#' aws_role_delete(name = "MyRole")
#' }
aws_role_delete <- function(name) {
  env64$iam$delete_role(name)
}
