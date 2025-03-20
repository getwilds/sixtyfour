#' Group list cleanup
#'
#' @noRd
#' @param x (list) a nested list, from a call to
#' [list_users](https://www.paws-r-sdk.com/docs/iam_list_gtroups/)
#' @keywords internal
group_list_tidy <- function(x) {
  if (rlang::is_empty(x)) {
    return(tibble())
  }
  vars <- c("GroupName", "GroupId", "Path", "Arn", "CreateDate")
  tidy_generator(vars)(x) %>%
    mutate(Arn = ifelse(env64$redacted, env64$redact_str, Arn))
}

#' List all groups or groups for a single user
#'
#' @export
#' @param username (character) a username. optional
#' @param ... parameters passed on to `paws` `list_groups_for_user`
#' if username is non-NULL, otherwise passed on to `list_users`
#' @return A tibble with information about groups
#' @family groups
#' @examplesIf aws_has_creds()
#' aws_groups()
#' aws_groups(username = aws_user_current())
aws_groups <- function(username = NULL, ...) {
  if (is.null(username)) {
    paginate_aws_marker("list_groups", "Groups", ...) %>%
      group_list_tidy()
  } else {
    paginate_aws_marker(
      "list_groups_for_user",
      "Groups",
      UserName = username,
      ...
    ) %>%
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
#' @examplesIf aws_has_creds()
#' # create a group
#' aws_group_create("testing")
#' # get the group
#' aws_group(name = "testing")
#' # cleanup
#' aws_group_delete(name = "testing")
aws_group <- function(name) {
  x <- con_iam()$get_group(name)
  list(
    group = x$Group %>% list(.) %>% group_list_tidy(),
    users = x$Users %>% user_list_tidy(),
    policies = policies("group", name),
    attached_policies = policies_attached("group", name)
  )
}

#' Check if a group exists
#'
#' @importFrom purrr safely
#' @export
#' @param name (character) the group name
#' @return a single boolean
#' @details uses `aws_group` internally. see docs
#' <https://www.paws-r-sdk.com/docs/iam_get_group/>
#' @family groups
#' @examplesIf aws_has_creds()
#' aws_group_create("apples")
#' aws_group_exists("apples")
#' aws_group_exists("doesnotexist")
#' # cleanup
#' aws_group_delete("apples")
aws_group_exists <- function(name) {
  check_aws_group <- safely(aws_group, otherwise = FALSE)
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
#' @examplesIf aws_has_creds()
#' aws_group_create("testingagroup")
#' aws_group("testingagroup")
#' # cleanup
#' aws_group_delete("testingagroup")
aws_group_create <- function(name, path = NULL) {
  con_iam()$create_group(Path = path, GroupName = name) %>%
    group_list_tidy()
}

#' Delete a group
#'
#' @export
#' @inheritParams aws_group_create
#' @return `NULL` invisibly
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_group/>
#' docs for more details
#' @family groups
#' @examplesIf aws_has_creds()
#' aws_group_create("somegroup")
#' aws_group_delete("somegroup")
aws_group_delete <- function(name) {
  con_iam()$delete_group(name)
  invisible()
}

#' Delete a group, magically
#'
#' @export
#' @inheritParams aws_group_create
#' @return `NULL` invisibly
#' @details See <https://www.paws-r-sdk.com/docs/iam_delete_group/>
#' docs for more details
#' @family groups
#' @examplesIf aws_has_creds()
#' group <- random_string("group")
#' aws_group_create(group)
#' six_group_delete(group)
six_group_delete <- function(name) {
  group <- aws_group(name)

  # remove policies
  attpols <- group$attached_policies
  if (!rlang::is_empty(attpols)) {
    policies <- attpols$PolicyName
    map(policies, \(policy) aws_policy_detach(group, policy))
    cli_info("Polic{?y/ies} {.strong {policies}} detached")
  }

  # remove users
  if (!rlang::is_empty(group$users)) {
    users <- group$users
    map(users$UserName, \(g) aws_user_remove_from_group(g, name))
    cli_info("User{?s} {.strong {users$UserName}} detached")
  }

  aws_group_delete(name)
  cli_info("group {.strong {name}} deleted")
}
