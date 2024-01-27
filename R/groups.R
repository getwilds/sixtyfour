#' Group list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_gtroups/)
#' @keywords internal
group_list_tidy <- function(x) {
  vars <- c("GroupName", "GroupId", "Path", "Arn", "CreateDate")
  tidy_generator(vars)(x)
}

#' List all groups or groups for a single user
#'
#' @export
#' @param username (character) a username. optional
#' @param ... parameters passed on to `paws` `list_groups_for_user`
#' if username is non-NULL, otherwise passed on to `list_users`
#' @return A tibble with information about groups
#' @family groups
#' @examples \dontrun{
#' aws_groups()
#' aws_groups(username = aws_user_current())
#' }
aws_groups <- function(username = NULL, ...) {
  if (is.null(username)) {
    paginate_aws(env64$iam$list_groups, "Groups", ...) %>% group_list_tidy()
  } else {
    paginate_aws(env64$iam$list_groups_for_user, "Groups",
                 UserName = username, ...) %>%
      group_list_tidy()
  }
}

#' Get a group
#'
#' @export
#' @param name (character) the group name
#' @return a named list with slots for:
#' - group: information about the group (tibble)
#' - users: users in the group (tibble)
#' - policies (character)
#' - attached_policies (tibble)
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_group/>
#' @autoglobal
#' @family groups
#' @examples \dontrun{
#' aws_group(name="users")
#' }
aws_group <- function(name) {
  x <- env64$iam$get_group(name)
  list(
    group = x$Group %>% list(.) %>% group_list_tidy(),
    users = x$Users %>% user_list_tidy(),
    policies = policies("group", name),
    attached_policies = policies_attached("group", name)
  )
}

#' Check if a group exists
#'
#' @export
#' @param name (character) the group name
#' @return a single boolean
#' @details uses `aws_group` internally. see docs
#' <https://www.paws-r-sdk.com/docs/iam_get_group/>
#' @family groups
#' @examples \dontrun{
#' aws_group_exists(name="users")
#' aws_group_exists(name="apples")
#' }
aws_group_exists <- function(name) {
  check_aws_group <- purrr::safely(aws_group, otherwise = FALSE)
  is.null(check_aws_group(name)$error)
}

#' Create a group
#'
#' @export
#' @param name (character) A group name. required
#' @param path (character) The path for the group name. optional.
#' If it is not included, it defaults to a slash (/).
#' @return A tibble with information about the group created
#' @details See <https://www.paws-r-sdk.com/docs/iam_create_group/>
#' docs for details on the parameters
#' @family groups
#' @examples \dontrun{
#' aws_group_create("testgroup")
#' }
aws_group_create <- function(name, path = NULL) {
  env64$iam$create_group(Path = path, GroupName = name) %>%
    group_list_tidy()
}

#' Delete a group
#'
#' @export
#' @inheritParams aws_group_create
#' @return an empty list
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_group/>
#' docs for more details
#' @family groups
#' @examples \dontrun{
#' aws_group_delete(name = "testgroup")
#' }
aws_group_delete <- function(name) {
  env64$iam$delete_group(name)
}
