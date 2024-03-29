#' Policy list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_policies](https://www.paws-r-sdk.com/docs/iam_list_policies/)
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
  paginate_aws_marker(env64$iam$list_policies, "Policies", ...) %>%
    policy_list_tidy()
})

#' List policies
#'
#' @export
#' @param refresh (logical) refresh results? default: `FALSE`. to invalidate
#' cache and refresh policy data, set `refresh=TRUE`
#' @param ... named arguments passed on to
#' [list_policies](https://www.paws-r-sdk.com/docs/iam_list_policies/)
#' @details uses `memoise` internally to cache results to speed up all
#' subsequent calls to the function
#' @family policies
#' @return A tibble with information about policies. Each row is a policy.
#' Columns:
#' - PolicyName
#' - PolicyId
#' - Path
#' - Arn
#' - CreateDate
#' - UpdateDate
#' - AttachmentCount
#' - PermissionsBoundaryUsageCount
#' - IsAttachable
#' - Description
#' - Tags
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
#' @inheritParams as_policy_arn
#' @return a tibble with policy details
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_policy/>
#' @autoglobal
#' @family policies
#' @examples \dontrun{
#' aws_policy("ReadOnlyAccess")
#' aws_policy("arn:aws:iam::aws:policy/ReadOnlyAccess")
#' }
aws_policy <- function(name, local = FALSE) {
  env64$iam$get_policy(as_policy_arn(name, local))$Policy %>%
    list(.) %>%
    policy_list_tidy()
}

aws_policy_safe <- purrr::safely(aws_policy)

#' Check if a policy exists
#'
#' Checks for both customer managed and AWS managed policies
#'
#' @export
#' @inheritParams as_policy_arn
#' @return single logical, `TRUE` or `FALSE`
#' @family policies
#' @examplesIf interactive()
#' aws_policy_exists("ReadOnlyAccess")
#' aws_policy_exists("arn:aws:iam::aws:policy/ReadOnlyAccess")
aws_policy_exists <- function(name) {
  !is.null(aws_policy_safe(name)$result) ||
    !is.null(aws_policy_safe(name, local = TRUE)$result)
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
#' @examplesIf interactive()
#' doc <- aws_policy_document_create(
#'   region = "us-east-2",
#'   account_id = "1234567890",
#'   resource_id = "*",
#'   user = "jane_doe",
#'   action = "rds-db:connect"
#' )
#' aws_policy_create("RdsAllow123", document = doc)
#' # cleanup
#' aws_policy_delete("RdsAllow123")
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

#' Delete a user managed policy
#'
#' @export
#' @param name (character) a policy name. required. within the function
#' we lookup the policy arn which is what's passed to the AWS API
#' @return invisibly returns nothing
#' @section AWS managed policies:
#' You can not delete AWS managed policies.
#' @section Deleting process (adapted from `paws` docs):
#' Before you can delete a managed policy, you must first detach
#' the policy from all users, groups, and roles that it is attached to.
#' In addition, you must delete all the policy's versions. The following
#' steps describe the process for deleting a managed policy:
#'
#' - Detach the policy from all users, groups, and roles that the policy is
#' attached to using [aws_policy_attach()]. To list all the users, groups,
#' and roles that a policy is attached to use [aws_policy_list_entities()]
#' - Delete all versions of the policy using [aws_policy_delete_version()].
#' To list the policy's versions, use [aws_policy_list_versions()]. You cannot
#' use [aws_policy_delete_version()] to delete the version that is marked as
#' the default version. You delete the policy's default version in the next
#' step of the process.
#' - Delete the policy using this function (this automatically deletes the
#' policy's default version)
#' @references
#' [delete_policy](https://www.paws-r-sdk.com/docs/iam_delete_policy/)
#' @family policies
#' @examplesIf interactive()
#' doc <- aws_policy_document_create(
#'   region = "us-east-2",
#'   account_id = "1234567890",
#'   resource_id = "*",
#'   user = "jane_doe",
#'   action = "rds-db:connect"
#' )
#' aws_policy_create("RdsAllow456", document = doc)
#' aws_policy_delete("RdsAllow456")
aws_policy_delete <- function(name) {
  env64$iam$delete_policy(PolicyArn = figure_out_policy_arn(name))
}

#' Figure out policy Arn from a name
#' @importFrom purrr compact
#' @keywords internal
#' @examplesIf interactive()
#' # user managed, exists
#' figure_out_policy_arn("MyTestPolicy")
#' # user managed, doesn't exist
#' figure_out_policy_arn("DoesNotExist")
#' # aws managed
#' figure_out_policy_arn("AmazonS3ReadOnlyAccess")
figure_out_policy_arn <- function(name) {
  compact(c(
    aws_policy_safe(name, local = TRUE)$result$Arn,
    aws_policy_safe(name, local = FALSE)$result$Arn
  ))
}

#' Delete a policy version
#'
#' @export
#' @inheritParams aws_policy_delete
#' @param version_id (character) The policy version to delete. required.
#' Allows (via regex) a string of characters that consists of the lowercase
#' letter 'v' followed by one or two digits, and optionally followed by a
#' period '.' and a string of letters and digits.
#' @return invisibly returns nothing
#' @references
#' <https://www.paws-r-sdk.com/docs/iam_delete_policy_version/>
#' @family policies
#' @examplesIf interactive()
#' doc <- aws_policy_document_create(
#'   region = "us-east-2",
#'   account_id = "1234567890",
#'   resource_id = "*",
#'   user = "jane_doe",
#'   action = "rds-db:connect"
#' )
#' aws_policy_create("RdsAllow456", document = doc)
#' aws_policy_delete_version("RdsAllow456", "v1")
aws_policy_delete_version <- function(name, version_id) {
  env64$iam$delete_policy_version(
    PolicyArn = figure_out_policy_arn(name),
    VersionId = version_id
  )
}

#' List policy entities
#'
#' @export
#' @autoglobal
#' @inheritParams aws_policy_delete
#' @param ... additional named arguments passed on to internal `paws` method
#' (see link below to its docs)
#' @return tibble with columns:
#' - type: one of Users, Roles, Groups
#' - name: the user, role or group name
#' - id: the id for the user, role or group name
#'
#' Zero row tibble if there are no entities
#' @references
#' <https://www.paws-r-sdk.com/docs/iam_list_entities_for_policy/>
#' @family policies
#' @examplesIf interactive()
#' pols <- aws_policies(Scope = "Local")
#' for (policy in pols$PolicyName) {
#'   cat("\n\n", policy, "\n")
#'   print(aws_policy_list_entities(policy))
#' }
#' aws_policy_list_entities("S3ReadOnlyAccessS64Test22")
aws_policy_list_entities <- function(name, ...) {
  result <- env64$iam$list_entities_for_policy(
    PolicyArn = figure_out_policy_arn(name),
    ...
  )
  df <- result[grepl("Policy.+", names(result))] %>%
    map(\(x) {
      list_rbind(map(x, as_tibble)) %>%
        rename_with(~ sub("^User|^Role|^Group", "", .))
    }) %>%
    bind_rows(.id = "type") %>%
    as_tibble() %>%
    mutate(type = sub("Policy", "", type)) %>%
    rename_with(~ tolower(.))
  if (NROW(df) == 0) tibble() else df
}

#' List policy versions
#'
#' @export
#' @inheritParams aws_policy_delete
#' @inheritParams aws_policy_list_entities
#' @return tibble with columns:
#' - Document
#' - VersionId
#' - IsDefaultVersion
#' - CreateDate
#' @references
#' <https://www.paws-r-sdk.com/docs/iam_list_policy_versions/>
#' @family policies
#' @examplesIf interactive()
#' pols <- aws_policies()
#' aws_policy_list_versions(pols$PolicyName[1])
aws_policy_list_versions <- function(name, ...) {
  vars <- c(
    "Document", "VersionId", "IsDefaultVersion", "CreateDate"
  )
  env64$iam$list_policy_versions(
    PolicyArn = figure_out_policy_arn(name), ...
  )$Versions %>%
    tidy_generator(vars)(.)
}

#' Create a policy statement
#'
#' @export
#' @param action (character) an action. required. see Actions below.
#' @param resource (character) the object or objects the statement covers;
#' see link below for more information
#' @param effect (character) valid values: "Allow" (default), "Deny". length==1
#' @param ... Additional named arguments. See link in Details for options,
#' and examples below
#' @details
#' <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html> #nolint
#' @return a named list
#' @examples
#' aws_policy_statement("iam:GetUser", "*")
#' aws_policy_statement("iam:GetUser", "*", Sid = "MyStatementId")
#' aws_policy_statement("iam:GetUser", "*",
#'   Condition = list(
#'     StringEqualsIgnoreCase = list("aws:username" = "johndoe")
#'   )
#' )
#' aws_policy_statement("iam:GetUser", "*",
#'   Principal = list(Service = "s3.amazonaws.com")
#' )
aws_policy_statement <- function(action, resource, effect = "Allow", ...) {
  list(
    Effect = effect,
    Action = action,
    Resource = resource,
    ...
  )
}

#' Create a resource string for a policy statement for RDS
#'
#' @export
#' @param user (character) a user name that has an IAM account. length>=1.
#' required
#' @param resource_id (character) the identifier for the DB instance.
#' length==1. required
#' @param region (character) the AWS Region for the DB instance. length==1
#' @param account (character) the AWS account number for the DB instance.
#' length==1. The user must be in the same account as the account for the
#' DB instance. by default calls [account_id()]
#' @return a resource ARN (character)
resource_rds <- function(
    user,
    resource_id,
    region = Sys.getenv("AWS_REGION"),
    account = account_id()) {
  glue(
    "arn:aws:rds-db:{region}:{account}:dbuser:{resource_id}/{user}"
  )
}

#' Create a policy document
#'
#' @export
#' @param ...,.list policy statements as created by [aws_policy_statement()]
#' or created manually. Pass in 1 or more statements via `...` like
#' `statement1, statement2` or pass in as a list like
#' `.list = list(statement1, statement2)`. Each element must be a named list.
#' @references
#' <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements.html> # nolint
#' @return a json class string. use [as.character()] to coerce to a regular
#' string
#' @note a document item is hard-coded:
#' - `Version` is set to 2012-10-17"
#' @section Actions:
#' Actions documentation appears to be all over the web. Here's a start:
#' - S3: <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazons3.html> # nolint
#' - EC2: <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Operations.html> # nolint
#' - IAM: <https://docs.aws.amazon.com/IAM/latest/APIReference/API_Operations.html> # nolint
#' @examples
#' st8ment1 <- aws_policy_statement("iam:GetUser", "*")
#' st8ment2 <- aws_policy_statement("s3:ListAllMyBuckets", "*")
#' st8ment3 <- aws_policy_statement("s3-object-lambda:List*", "*")
#' aws_policy_document_create(st8ment1, st8ment2)
#' aws_policy_document_create(.list = list(st8ment1, st8ment2))
#' aws_policy_document_create(st8ment3, .list = list(st8ment1, st8ment2))
#'
#' # Policy document to give a user access to RDS
#' resource <- "arn:aws:rds-db:us-east-2:1234567890:dbuser:db-ABCDE1212/jane"
#' st8ment_rds <- aws_policy_statement(
#'   action = "rds-db:connect",
#'   resource = resource
#' )
#' aws_policy_document_create(st8ment_rds)
#'
#' ### DB account = user in a database that has access to it
#' # all DB instances & DB accounts for a AWS account and AWS Region
#' aws_policy_document_create(
#'   aws_policy_statement(
#'     action = "rds-db:connect",
#'     resource = resource_rds("*", "*")
#'   )
#' )
#' # all DB instances for a AWS account and AWS Region, single DB account
#' aws_policy_document_create(
#'   aws_policy_statement(
#'     action = "rds-db:connect",
#'     resource = resource_rds("jane_doe", "*")
#'   )
#' )
#' # single DB instasnce, single DB account
#' aws_policy_document_create(
#'   aws_policy_statement(
#'     action = "rds-db:connect",
#'     resource = resource_rds("jane_doe", "db-ABCDEFGHIJKL01234")
#'   )
#' )
#' # single DB instance, many users
#' aws_policy_document_create(
#'   aws_policy_statement(
#'     action = "rds-db:connect",
#'     resource = resource_rds(c("jane_doe", "mary_roe"), "db-ABCDEFGHIJKL01234")
#'   )
#' )
aws_policy_document_create <- function(..., .list = NULL) {
  stop_if_not(
    rlang::is_list(.list) || is.null(.list),
    "`.list` must be a list"
  )
  statements <- c(list(...), .list)
  stop_if_not(
    all(map_lgl(statements, rlang::is_list)),
    "all elements passed to `...` and `.list` must be lists"
  )
  stop_if_not(
    all(map_lgl(statements, rlang::is_named)),
    "all elements passed to `...` and `.list` must be named lists"
  )
  doc <- list(
    Version = "2012-10-17",
    Statement = statements
  )
  jsonlite::toJSON(doc, auto_unbox = TRUE)
}

#' Convert a policy name to a policy ARN
#'
#' This function simply constructs a string, then checks that the
#' ARN is valid using
#' [get_policy](https://www.paws-r-sdk.com/docs/iam_get_policy/)
#'
#' @export
#' @importFrom dplyr filter pull
#' @param name (character) a policy name or arn
#' @param local (logical) if `TRUE` use your AWS account for your own
#' managed policies. If `FALSE`, AWS managed policies
#' @details If an arn is supplied we check that a policy exists
#' with that arn, and return itself.
#'
#' If a policy name is supplied we construct an arn with `local` param
#' as an input, and then check that a policy exists with that arn
#'
#' In both cases we stop with an informative message if there's an error
#' @return a policy ARN (character)
#' @autoglobal
#' @family policies
#' @examples \dontrun{
#' as_policy_arn("ReadOnlyAccess")
#' as_policy_arn("arn:aws:iam::aws:policy/ReadOnlyAccess")
#' as_policy_arn("AmazonRDSDataFullAccess")
#' # as_policy_arn("Blarp")
#' # as_policy_arn(letters)
#' # as_policy_arn(5)
#' as_policy_arn("MyTestPolicy", local = TRUE)
#' # returns an arn - and if given an arn returns self
#' as_policy_arn("MyTestPolicy", local = TRUE) %>%
#'   as_policy_arn("MyTestPolicy")
#' }
as_policy_arn <- function(name, local = FALSE) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  if (grepl("^arn:", name)) {
    return(name)
  }
  account <- if (local) account_id() else "aws"
  glue("arn:aws:iam::{account}:policy/{name}")
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
  env64$iam[[method]](entity_value(.x), figure_out_policy_arn(policy))
  call_x_method(.x)
}

#' Detach a policy from a user, group, or role
#'
#' @export
#' @inheritParams aws_policy_attach
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
  env64$iam[[method]](entity_value(.x), figure_out_policy_arn(policy))
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

has_policy <- function(.x, policy) {
  stop_if_not(rlang::is_list(.x), "`.x` must be a list")
  if (rlang::is_empty(.x$attached_policies)) {
    return(FALSE)
  }
  if (!inherits(.x$attached_policies, "tbl")) {
    return(FALSE)
  }
  policy %in% .x$attached_policies$PolicyName
}
