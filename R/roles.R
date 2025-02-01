#' Role list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_roles/)
#' @keywords internal
#' @note leaves out: PermissionsBoundary, Tags, RoleLastUsed,
#' MaxSessionDuration
role_list_tidy <- function(x) {
  if (rlang::is_empty(x)) {
    return(tibble())
  }
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
#' @examplesIf interactive() && aws_has_creds()
#' aws_roles()
aws_roles <- function(...) {
  paginate_aws_marker("list_roles", "Roles") %>% role_list_tidy()
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
#' @examplesIf aws_has_creds()
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
#' desc <- "Another test role"
#' z <- aws_role_create("ALittleRole",
#'   assume_role_policy_document = doc,
#'   description = desc
#' )
#' aws_policy_attach(z, "ReadOnlyAccess")
#' res <- aws_role(name = "ALittleRole")
#' res
#' res$role
#' res$policies
#' res$attached_policies
#'
#' # cleanup
#' aws_role("ALittleRole") %>%
#'  aws_policy_detach("ReadOnlyAccess")
#' aws_role_delete("ALittleRole")
aws_role <- function(name) {
  df <- con_iam()$get_role(name)$Role %>%
    list(.) %>%
    role_list_tidy()
  list(
    role = df,
    policies = policies("role", name),
    attached_policies = policies_attached("role", name)
  )
}

check_aws_role <- purrr::safely(aws_role, otherwise = FALSE)

#' Check if a role exists
#'
#' @export
#' @inheritParams aws_role
#' @return a single boolean
#' @family roles
#' @examplesIf aws_has_creds()
#' aws_role_exists("AWSServiceRoleForRedshift")
#' aws_role_exists("NotARole")
aws_role_exists <- function(name) {
  is.null(check_aws_role(name)$error)
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
#' @examplesIf aws_has_creds()
#' role_name <- "AMinorRole"
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
#' # attach a policy
#' invisible(z %>% aws_policy_attach("AWSLambdaBasicExecutionRole"))
#'
#' # cleanup
#' invisible(z %>% aws_policy_detach("AWSLambdaBasicExecutionRole"))
#' aws_role_delete(role_name)
aws_role_create <- function(
    name, assume_role_policy_document, path = NULL,
    description = NULL, max_session_duration = NULL, permission_boundary = NULL,
    tags = NULL) {
  con_iam()$create_role(
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
#' @return `NULL` invisibly
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_role/>
#' docs for more details
#' @family roles
#' @examplesIf aws_has_creds()
#' if (aws_role_exists(name = "MyRole")) {
#'   aws_role_delete(name = "MyRole")
#' }
aws_role_delete <- function(name) {
  con_iam()$delete_role(name)
  invisible()
}
