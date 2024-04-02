#' User list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_users/)
#' @autoglobal
#' @keywords internal
user_list_tidy <- function(x) {
  vars <- c(
    "UserName", "UserId", "Path", "Arn", "CreateDate",
    "PasswordLastUsed", "Tags"
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
#' @returns A tibble with information about user accounts
#' @examples \dontrun{
#' aws_users()
#' }
aws_users <- function(...) {
  # aws_iam_client()
  # users <- paginate_aws_marker(env64$iam$list_users, "Users") %>%
  users <- paginate_aws_marker(get_iam()$list_users, "Users", ...) %>%
    user_list_tidy()
  # purrr::map(users$UserName, env64$iam$get_user) %>%
  purrr::map(users$UserName, get_iam()$get_user) %>%
    purrr::map(purrr::pluck, "User") %>%
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
  # aws_iam_client()
  x <- get_iam()$get_user(username)$User %>%
    list(.) %>%
    user_list_tidy()
  if (is.null(username)) username <- x$UserName
  list(
    user = x,
    policies = policies("user", username),
    attached_policies = policies_attached("user", username),
    groups = get_iam()$list_groups_for_user(username)
  )
}

check_aws_user <- purrr::safely(aws_user, otherwise = FALSE)

#' Check if a user exists
#'
#' @export
#' @param username (character) the user name
#' @return a single boolean
#' @details uses [aws_user()] internally. see docs
#' <https://www.paws-r-sdk.com/docs/iam_get_user/>
#' @family users
#' @examples \dontrun{
#' aws_user_exists(aws_user_current())
#' aws_user_exists("blueberry")
#' }
aws_user_exists <- function(username) {
  # aws_iam_client()
  is.null(check_aws_user(username)$error)
}

#' Get the current logged-in username as a string
#' @export
#' @family users
#' @return username as character
aws_user_current <- function() {
  # aws_iam_client()
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
  # aws_iam_client()
  get_iam()$create_user(
    Path = path,
    UserName = username,
    PermissionsBoundary = permission_boundary,
    Tags = tags
  ) %>%
    user_list_tidy()
}

#' Delete a user
#'
#' @export
#' @inheritParams aws_user_create
#' @return an empty list
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_user/>
#' docs for more details
#' @family users
#' @examples \dontrun{
#' aws_user_delete(username = "testBlueBird")
#' }
aws_user_delete <- function(username) {
  # aws_iam_client()
  get_iam()$delete_user(username)
}

#' Get the current user's AWS Access Key
#'
#' IMPORTANT: the secret access key is only accessible during key
#' and user creation
#'
#' @export
#' @return a tibble with key details
#' @details See <https://www.paws-r-sdk.com/docs/iam_list_access_keys/>
#' docs for more details
#' @family users
#' @examples \dontrun{
#' # aws_user_access_key()
#' }
aws_user_access_key <- function(username = NULL, ...) {
  out <- get_iam()$list_access_keys(username, ...)
  if (length(out$AccessKeyMetadata) == 0) {
    cli::cli_alert_warning("No access keys found for {.strong {username}}")
    return(invisible())
  }
  bind_rows(out$AccessKeyMetadata)
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
  # aws_iam_client()
  get_iam()$add_user_to_group(groupname, username)
  aws_user(username)
}
