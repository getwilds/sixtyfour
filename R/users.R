## TODO: not looked over this file yet

#' User list cleanup - internal function
#'
#' @importFrom purrr map list_rbind
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom lubridate as_datetime
#' @autoglobal
#' @noRd
#' @param x what is this param Sean? some kind of tibble presumably
user_list_cleanup <- function(x) {
  x %>%
    map(~ .x[c(
      "UserName", "UserId", "Path",
      "Arn", "CreateDate", "PasswordLastUsed"
    )]) %>%
    map(\(x) map(x, \(y) ifelse(length(y) < 1, NA, y))) %>%
    map(as_tibble) %>%
    list_rbind() %>%
    mutate(CreateDate = as_datetime(CreateDate)) %>%
    mutate(PasswordLastUsed = as_datetime(PasswordLastUsed))
}

#' List Users
#'
#' @export
#' @returns A data frame with information about user accounts.
list_users <- function() {
  env64$iam$list_users()$Users %>%
    user_list_cleanup()
}

#' Create a User
#'
#' @export
#' @param username A user name
create_user <- function(username) {
  result <- env64$iam$create_user(UserName = username)

  result %>%
    user_list_cleanup()
}
