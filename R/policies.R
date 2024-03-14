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
#' @family policies
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
#' @family policies
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
#' @family policies
#' @examples \dontrun{
#' aws_policy_exists("ReadOnlyAccess")
#' }
aws_policy_exists <- function(name) {
  !is.null(purrr::safely(aws_policy)(name)$result)
}

#' Create a policy
#'
#' @export
#' @param name (character) a policy name. required
#' @param document (character) the policy document you want to use
#' as the content for the new policy. required.
#' @param path (character) the path for the policy. if not given
#' default is "/". optional
#' @param description (character) a friendly description of the policy.
#' optional. cannot be changed after assigning it
#' @param tags (character) a vector of tags that you want to attach to
#' the new IAM policy. Each tag consists of a key name and an associated
#' value. optional
#' @return a tibble with policy details
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_create_policy/>
#' @family policies
#' @examples \dontrun{
#' aws_db_rds_list()
#' aws_policy_document_create()
#' aws_policy_create("RdsAllow", document = doc)
#' }
aws_policy_create <- function(
    name, document, path = NULL,
    description = NULL, tags = NULL) {
  env64$iam$create_policy(
    PolicyName = name,
    PolicyDocument = document,
    Path = path,
    Description = description,
    Tags = tags
  )
}

#' Create a policy document
#'
#' @export
#' @param region (character) the AWS Region for the DB instance. length==1
#' @param account_id (character) the AWS account number for the DB instance.
#' length==1. The user must be in the same account as the account for the
#' DB instance
#' @param resource_id (character) the identifier for the DB instance. length==1
#' @param user (character) a user name that has an IAM account. length>=1
#' @param effect (character) valid values: "Allow" (default), "Deny". length==1
#' @param ... named args passed to [jsonlite::toJSON()]
#' @references #no lint start
#' https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html
#' #no lint end
#' @return a json class string. use [as.character()] to coerce to a regular
#' string
#' @note a few document items are hard-coded:
#' - `Version` is set to 2012-10-17"
#' - `Action` is set to "rds-db:connect"
#' @examplesIf interactive()
#' ### DB account = user in a database that has access to it
#' # all DB instances & DB accounts for a AWS account and AWS Region
#' aws_policy_document_create("us-east-2", "1234567890", "*", "*")
#' # all DB instances for a AWS account and AWS Region, single DB account
#' aws_policy_document_create("us-east-2", "1234567890", "*", "jane_doe")
#' # single DB instasnce, single DB account
#' aws_policy_document_create(
#'   "us-east-2",
#'   "1234567890", "db-ABCDEFGHIJKL01234", "jane_doe"
#' )
#' # single DB instance, many users
#' aws_policy_document_create(
#'   "us-east-2", "1234567890",
#'   "db-ABCDEFGHIJKL01234", c("jane_doe", "mary_roe")
#' )
aws_policy_document_create <- function(
    region, account_id, resource_id, user,
    effect = "Allow", ...) {
  resource <- glue(
    "arn:aws:rds-db:{region}:{account_id}:dbuser:{resource_id}/{user}"
  )
  doc <- list(
    Version = "2012-10-17",
    Statement = list(
      list(
        Effect = effect,
        Action = "rds-db:connect",
        Resource = resource
      )
    )
  )
  jsonlite::toJSON(doc, auto_unbox = TRUE, ...)
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
#' @family policies
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
#' @family policies
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
#' @family policies
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
