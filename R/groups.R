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
#' @return A list with two elements:
#' - group: tibble with information about the group
#' - users: tibble with users in the group
#' @details see docs <https://www.paws-r-sdk.com/docs/iam_get_group/>
#' @autoglobal
#' @examples \dontrun{
#' aws_group(name="users")
#' }
aws_group <- function(name) {
  x <- env64$iam$get_group(name)
  list(
    group = x$Group %>% list(.) %>% group_list_tidy(),
    users = x$Users %>% user_list_tidy()
  )
}
