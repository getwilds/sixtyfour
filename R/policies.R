#' Policy list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_policies/)
#' @keywords internal
#' @note leaves out: xxx
policy_list_tidy <- function(x) {
  vars <- c(
    "PolicyName", "PolicyId", "Path", "Arn", "CreateDate",
    "UpdateDate", "AttachmentCount", "PermissionsBoundaryUsageCount",
    "IsAttachable", "Description", "Tags"
  )
  tidy_generator(vars)(x)
}

#' List policies
#'
#' @export
#' @param ... parameters passed on to the `paws`
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_policies/) method
#' @return A tibble with information about policies
#' @examples \dontrun{
#' aws_policies()
#' }
aws_policies <- function(...) {
  paginate_aws(env64$iam$list_policies, "Policies") %>%
    policy_list_tidy()
}

#' Get a policy
#'
#' @export
#' @param name (character) the policy name
#' @return a tibble with policy details
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_policy/>
#' @autoglobal
#' @examples \dontrun{
#' aws_policy(name = "ReadOnlyAccess")
#' }
aws_policy <- function(name) {
  env64$iam$get_policy(as_policy_arn(name))$Policy %>%
    list(.) %>%
    policy_list_tidy()
}

#' Get a policy
#'
#' @export
#' @param name (character) the policy name
#' @return a tibble with policy details
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_policy/>
#' @examples \dontrun{
#' aws_policy(name = "ReadOnlyAccess")
#' }
aws_policy_exists <- function(name) {
  !is.null(purrr::safely(aws_policy)(name)$result)
}

as_policy_arn <- function(name) {
  glue::glue("arn:aws:iam::aws:policy/{name}")
}

#' Attach a policy to a user, group, or role
#'
#' @export
#' @param .x result of a call to create or get method for user,
#' group, or role
#' @param policy (character) a policy name
#' @return A tibble with information about policies
#' @examples \dontrun{
#' aws_user() %>%
#'   aws_policy_attach("AmazonRDSDataFullAccess")
#' aws_user()$attached_policies
#'
#' # aws_role("OrganizationAccountSecurityRole") %>%
#' #  aws_policy_attach("ReadOnlyAccess")
#' }
aws_policy_attach <- function(.x, policy) {
  method <- glue::glue("attach_{entity_type(.x)}_policy")
  env64$iam[[method]](entity_value(.x), as_policy_arn(policy))
}

#' Detach a policy from a user, group, or role
#'
#' @export
#' @param .x result of a call to create or get method for user,
#' group, or role
#' @param policy (character) a policy name
#' @return A tibble with information about policies
#' @examples \dontrun{
#' aws_user() %>%
#'   aws_policy_detach("AmazonRDSDataFullAccess")
#' aws_user()$attached_policies
#'
#' # aws_role("OrganizationAccountSecurityRole") %>%
#' #  aws_policy_detach("ReadOnlyAccess")
#' }
aws_policy_detach <- function(.x, policy) {
  method <- glue::glue("detach_{entity_type(.x)}_policy")
  env64$iam[[method]](entity_value(.x), as_policy_arn(policy))
}

# FIXME: this is probably fragile
# get the entity type, one  of: role, group, user
entity_type <- function(x) {
  if (is.data.frame(x)) {
    # aws_role
    piece <- names(x)[1]
  } else {
    # aws_user, aws_group
    piece <- names(x[[1]])[1]
  }
  # extract which type
  sub("name", "", tolower(piece))
}

# FIXME: this is probably fragile
# get the name of the e.g. RoleName, e.,g, OrganizationAccountSecurityRole
entity_value <- function(x) {
  if (is.data.frame(x)) {
    # aws_role
    x[[names(x)[1]]]
  } else {
    # aws_user, aws_group
    x[[1]][[names(x[[1]])[1]]]
  }
}
