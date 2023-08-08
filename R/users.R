#' @importFrom purrr map list_rbind
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom lubridate as_datetime
user_list_cleanup <- function(x) {
  x |>
    map(~ .x[c("UserName", "UserId", "Path", "Arn", "CreateDate", "PasswordLastUsed")]) |>
    map(\(x) map(x, \(y) ifelse(length(y) < 1, NA, y))) |>
    map(as_tibble) |>
    list_rbind() |>
    mutate(CreateDate = as_datetime(CreateDate)) |>
    mutate(PasswordLastUsed = as_datetime(PasswordLastUsed))
}

#' List Users
#'
#' @export
#' @importFrom paws iam
#' @returns A data frame with information about user accounts.
list_users <- function() {
  batman <- paws::iam()

  batman$list_users()$Users |>
    user_list_cleanup()
}

#' Create a User
#'
#' @export
#' @importFrom paws iam
create_user <- function(username) {
    batman <- paws::iam()

    result <- batman$create_user(UserName = username)

    result |>
      user_list_cleanup()
}




testUser1_keys <- batman$create_access_key("testUser1")

