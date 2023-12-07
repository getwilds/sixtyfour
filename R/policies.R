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

all_policies <- memoise::memoise(function(...) {
  if (Sys.getenv("TESTING64", FALSE)) {
    return(policies_sample)
  }
  paginate_aws(env64$iam$list_policies, "Policies", ...) %>% policy_list_tidy()
})

#' List policies
#'
#' @export
#' @param refresh (logical) refresh results? default: `FALSE`. to invalidate
#' cache and refresh policy data, set `refresh=TRUE`
#' @param ... parameters passed on to
#' [list_policies](https://www.paws-r-sdk.com/docs/iam_list_policies/)
#' @details uses `memoise` internally to cache results to speed up all
#' subsequent calls to the function
#' @return A tibble with information about policies
#' @examples \dontrun{
#' aws_policies()
#' aws_policies()
#' aws_policies(refresh = TRUE)
#' }
aws_policies <- function(refresh = FALSE, ...) {
  if (refresh) memoise::forget(all_policies)
  all_policies(...)
}

#' Get a policy
#'
#' @export
#' @param name (character) a policy name or ARN
#' @return a tibble with policy details
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_policy/>
#' @autoglobal
#' @examples \dontrun{
#' aws_policy("ReadOnlyAccess")
#' aws_policy("arn:aws:iam::aws:policy/ReadOnlyAccess")
#' }
aws_policy <- function(name) {
  env64$iam$get_policy(as_policy_arn(name))$Policy %>%
    list(.) %>%
    policy_list_tidy()
}

#' Get a policy
#'
#' @export
#' @param name (character) a policy name
#' @return a tibble with policy details
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_policy/>
#' @examples \dontrun{
#' aws_policy_exists("ReadOnlyAccess")
#' }
aws_policy_exists <- function(name) {
  !is.null(purrr::safely(aws_policy)(name)$result)
}

#' Convert a policy name to a policy ARN
#'
#' @export
#' @importFrom dplyr filter pull
#' @param name (character) a policy name or arn
#' @note uses exact matching; fails with error if there's no match;
#' beware as there is no validation is done of a user input policy arn
#' @return a policy ARN
#' @autoglobal
#' @examples \dontrun{
#' as_policy_arn("ReadOnlyAccess")
#' as_policy_arn("arn:aws:iam::aws:policy/ReadOnlyAccess")
#' as_policy_arn("AmazonRDSDataFullAccess")
#' # as_policy_arn("Blarp")
#' # as_policy_arn(letters)
#' # as_policy_arn(5)
#' }
as_policy_arn <- function(name) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  if (grepl("^arn:", name)) {
    return(name)
  }
  pols <- aws_policies()
  if (!name %in% pols$PolicyName) {
    stop(glue::glue(
      "'{name}' not associated with a known policy\n",
      "this function only checks against the policies listed with the\n",
      "https://www.paws-r-sdk.com/docs/iam_list_policies/ method"
    ))
  }
  pols %>%
    filter(PolicyName == name) %>%
    pull(Arn)
}

call_x_method <- function(x) {
  fun <- switch(entity_type(x),
    user = aws_user,
    role = aws_role,
    group = aws_group
  )
  fun(entity_value(x))
}

#' Attach a policy to a user, group, or role
#'
#' @export
#' @param .x result of a call to create or get method for user,
#' group, or role
#' @param policy (character) a policy name or ARN
#' @return A tibble with information about policies
#' @examples \dontrun{
#' aws_policy("AmazonRDSDataFullAccess")
#' aws_user() %>% aws_policy_attach("AmazonRDSDataFullAccess")
#' aws_user()$attached_policies
#'
#' # aws_role("OrganizationAccountSecurityRole") %>%
#' #  aws_policy_attach("ReadOnlyAccess")
#' }
aws_policy_attach <- function(.x, policy) {
  method <- glue::glue("attach_{entity_type(.x)}_policy")
  env64$iam[[method]](entity_value(.x), as_policy_arn(policy))
  call_x_method(.x)
}

#' Detach a policy from a user, group, or role
#'
#' @export
#' @param .x result of a call to create or get method for user,
#' group, or role
#' @param policy (character) a policy name or ARN
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
  call_x_method(.x)
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

#' @param which (character) one of role, user, or group
#' @param name (character) the name of a role, user or group
#' @return a tibble
#' @noRd
#' @keywords internal
policies <- function(which, name) {
  method <- glue::glue("list_{which}_policies")
  env64$iam[[method]](name)$PolicyNames
}
#' @importFrom dplyr bind_rows
#' @param which (character) one of role, user, or group
#' @param name (character) the name of a role, user or group
#' @return a tibble
#' @noRd
#' @keywords internal
policies_attached <- function(which, name) {
  method <- glue::glue("list_attached_{which}_policies")
  res <- env64$iam[[method]](name)
  res$AttachedPolicies %>% bind_rows()
}
