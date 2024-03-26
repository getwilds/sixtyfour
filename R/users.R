#' User list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_users/)
#' @autoglobal
#' @keywords internal
user_list_tidy <- function(x) {
  if (length(x) == 0) return(tibble())
  vars <- c(
    "UserName", "UserId", "Path", "Arn", "CreateDate",
    "PasswordLastUsed"
  )
  tidy_generator(vars)(x) %>%
    mutate(PasswordLastUsed = as_datetime(PasswordLastUsed))
}

#' List Users
#'
#' @export
#' @param ... parameters passed on to the `paws`
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_users/) method
#' @family users
#' @returns A tibble with information about user accounts, with columns:
#' - UserName
#' - UserId
#' - Path
#' - Arn
#' - CreateDate
#' - PasswordLastUsed
#' @examples \dontrun{
#' aws_users()
#' }
aws_users <- function(...) {
  paginate_aws_marker(env64$iam$list_users, "Users") %>%
    user_list_tidy()
}

#' Get a user
#'
#' Gets user information, including policies, groups, and attached policies
#'
#' @export
#' @inheritParams aws_user_create
#' @return a named list with slots for:
#' - user (tibble)
#' - policies (list)
#' - attached_policies (list)
#' - groups (list)
#' @details See the following docs links for details
#' - <https://www.paws-r-sdk.com/docs/iam_get_user/>
#' - <https://www.paws-r-sdk.com/docs/iam_list_user_policies/>
#' - <https://www.paws-r-sdk.com/docs/iam_list_groups_for_user/>
#' - <https://www.paws-r-sdk.com/docs/iam_list_attached_user_policies/>
#' @note if username not supplied, gets logged in user
#' @family users
#' @examples \dontrun{
#' # if username not supplied, gets logged in user
#' aws_user()
#'
#' # supply a username to get that user's information
#' aws_user_create("testBlueBird")
#' aws_user(username = "testBlueBird")
#' aws_user_delete("testBlueBird") # cleanup user
#' }
aws_user <- function(username = NULL) {
  x <- env64$iam$get_user(username)$User %>%
    list(.) %>%
    user_list_tidy()
  if (is.null(username)) username <- x$UserName
  list(
    user = x,
    policies = policies("user", username),
    attached_policies = policies_attached("user", username),
    groups = env64$iam$list_groups_for_user(username)
  )
}

check_aws_user <- purrr::safely(aws_user, otherwise = FALSE)

#' Check if a user exists
#'
#' @export
#' @param username (character) the user name
#' @return a single boolean
#' @details uses `aws_group` internally. see docs
#' <https://www.paws-r-sdk.com/docs/iam_get_group/>
#' @family users
#' @examples \dontrun{
#' aws_user_exists(aws_user_current())
#' aws_user_exists("blueberry")
#' }
aws_user_exists <- function(username) {
  is.null(check_aws_user(username)$error)
}

#' Get the current logged-in username as a string
#' @export
#' @family users
#' @return username as character
aws_user_current <- function() {
  x <- aws_user()
  x$user$UserName
}

#' Create a user
#'
#' @export
#' @param username (character) A user name. required
#' @param path (character) The path for the user name. optional.
#' If it is not included, it defaults to a slash (/).
#' @param permission_boundary (character) The ARN of the managed policy
#' that is used to set the permissions boundary for the user. optional
#' @param tags (list) A list of tags that you want to attach to the new user.
#' optional
#' @return A tibble with information about the user created
#' @details See <https://www.paws-r-sdk.com/docs/iam_create_user/>
#' docs for details on the parameters
#' @family users
#' @examples \dontrun{
#' aws_user_create("testBlueBird")
#' }
aws_user_create <- function(
    username, path = NULL, permission_boundary = NULL,
    tags = NULL) {
  env64$iam$create_user(
    Path = path,
    UserName = username,
    PermissionsBoundary = permission_boundary,
    Tags = tags
  ) %>%
    user_list_tidy()
}

#' Create a user
#'
#' @export
#' @inheritParams aws_user_create
#' @details See [aws_user_create()] for more details.
#' This function creates a user, adds policies so the
#' user can access their own account, and grants them an access
#' key. Add more policies using `aws_polic*` functions
#' @section What is magical:
#' - Adds a `GetUser` policy to your account if doesn't exist yet
#' - Attaches `GetUser` policy to the user created
#' - Grants an access key
#' @family users
#' @family magicians
#' @return NULL invisibly. A draft email is copied to your clipboard
#' @examplesIf interactive()
#' name <- random_user()
#' six_user_create(name)
six_user_create <- function(
    username, path = NULL, permission_boundary = NULL,
    tags = NULL) {
  aws_user_create(
    path = path,
    username = username,
    permission_boundary = permission_boundary,
    tags = tags
  )
  policy_name <- "UserInfo"
  if (!aws_policy_exists(policy_name)) {
    policy_doc <- aws_policy_document_create(
      aws_policy_statement(c("iam:GetUser", "iam:ListUserPolicies"), "*")
    )
    aws_policy_create(policy_name, policy_doc)
    cli_alert_info("Added policy {.strong {policy_name}} to your account")
  }
  user_obj <- aws_user(username)
  if (!has_policy(user_obj, policy_name)) {
    aws_policy_attach(user_obj, policy_name)
    cli_alert_info(
      "Added policy {.strong {policy_name}} to {.strong {username}}"
    )
  }
  aws_user_creds(username, copy_to_cp = TRUE)
}

#' Delete a user
#'
#' @export
#' @inheritParams aws_user_create
#' @return NULL invisibly
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_user/>
#' docs for more details
#' @family users
#' @examples \dontrun{
#' aws_user_delete(username = "testBlueBird")
#' }
aws_user_delete <- function(username) {
  env64$iam$delete_user(username)
  invisible()
}

#' Delete a user
#'
#' @export
#' @inheritParams aws_user_create
#' @return an empty list
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_user/>
#' docs for more details
#' @section What is magical:
#' - Detaches any attached policies
#' - Deletes any access keys
#' - Then deletes the user
#' @family users
#' @family magicians
#' @examplesIf interactive()
#' name <- random_user()
#' six_user_create(name)
#' six_user_delete(name)
six_user_delete <- function(username) {
  user_obj <- aws_user(username)

  attpols <- user_obj$attached_policies
  if (!rlang::is_empty(attpols)) {
    policies <- attpols$PolicyName
    map(policies, \(policy) aws_policy_detach(user_obj, policy))
    cli_alert_info("Polic{?y/ies} {.strong {policies}} detached")
  }

  keys <- aws_user_access_key(username)
  if (!is.null(keys)) {
    map(keys$AccessKeyId, aws_user_access_key_delete, username = username)
  }

  aws_user_delete(username)
  cli_alert_info("{.strong {username}} deleted")
}

#' Get AWS Access Key for a user
#'
#' IMPORTANT: the secret access key is only accessible during key
#' and user creation
#'
#' @export
#' @inheritParams aws_user_create
#' @param ... further named args passed on to
#' [list_access_keys](https://www.paws-r-sdk.com/docs/iam_list_access_keys/)
#' @return a tibble with key details
#' @details See <https://www.paws-r-sdk.com/docs/iam_list_access_keys/>
#' docs for more details
#' @family users
aws_user_access_key <- function(username = NULL, ...) {
  out <- env64$iam$list_access_keys(username, ...)
  if (length(out$AccessKeyMetadata) == 0) {
    cli::cli_alert_warning("No access keys found for {.strong {username}}")
    return(invisible())
  }
  bind_rows(out$AccessKeyMetadata)
}

#' Delete current user's AWS Access Key
#'
#' @export
#' @param access_key_id (character) The access key ID for the access key ID
#' and secret access key you want to delete. required.
#' @param username (character) A user name. optional. however, if you do
#' not supply a username, `paws` will likely use the current user, and so
#' may not be the user the access key id is associated - and then you'll get
#' an error like `NoSuchEntity (HTTP 404). The Access Key with id
#' AKIA22PL7JXX6X6O62OT cannot be found`
#' @return NULL, invisibly
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_access_key/>
#' docs for more details
#' @family users
#' @examplesIf interactive()
#' aws_user_access_key_delete(access_key_id = "adfasdfadfadfasdf")
#' aws_user_access_key_delete(access_key_id = "adfasdf", username = "jane")
aws_user_access_key_delete <- function(access_key_id, username = NULL) {
  env64$iam$delete_access_key(UserName = username, AccessKeyId = access_key_id)
  cli::cli_alert_success("Access Key ID {.strong {access_key_id}} deleted")
  invisible()
}

#' Add a user to a group
#'
#' @export
#' @inheritParams aws_user_create
#' @param groupname (character) a group name. required
#' @inherit aws_user return
#' @details See <https://www.paws-r-sdk.com/docs/iam_add_user_to_group/>
#' docs for more details
#' @family users
#' @examples \dontrun{
#' if (!aws_group_exists("testgroup3")) {
#'   aws_group_create("testgroup3")
#' }
#' if (!aws_user_exists("testBlueBird3")) {
#'   aws_user_create("testBlueBird3")
#' }
#' aws_user_add_to_group(username = "testBlueBird3", groupname = "testgroup3")
#' }
aws_user_add_to_group <- function(username, groupname) {
  env64$iam$add_user_to_group(groupname, username)
  aws_user(username)
}
