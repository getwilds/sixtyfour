group_policies_data <- list(
  admin = c(
    "AdministratorAccess",
    "Billing",
    "CostOptimizationHubAdminAccess",
    "AWSBillingReadOnlyAccess",
    "AWSCostAndUsageReportAutomationPolicy"
  ),
  users = c(
    "AmazonRDSReadOnlyAccess",
    "AmazonRedshiftReadOnlyAccess",
    "AmazonS3ReadOnlyAccess",
    "AWSBillingReadOnlyAccess",
    "IAMReadOnlyAccess"
  )
)

#' Preset group policies
#'
#' @export
#' @param group (character)
#' @return character vector of policy names
#' @section Admin group policies:
#' - AdministratorAccess
#' - Billing
#' - CostOptimizationHubAdminAccess
#' - AWSBillingReadOnlyAccess
#' - AWSCostAndUsageReportAutomationPolicy
#' @section User group policies:
#' - AmazonRDSReadOnlyAccess
#' - AmazonRedshiftReadOnlyAccess
#' - AmazonS3ReadOnlyAccess
#' - AWSBillingReadOnlyAccess
#' - IAMReadOnlyAccess
#' @examples
#' group_policies("admin")
#' group_policies("users")
group_policies <- function(group) {
  stop_if_not(is.character(group), "group must be character")
  stop_if_not(
    group %in% names(group_policies_data),
    "group must be one of {names(group_policies_data)}"
  )
  group_policies_data[[group]]
}

aws_whoami <- function() {
  user <- aws_user() # nolint
  cli_info("whoami: {user$user$UserName} (account: {account_id()})")
  cli_alert_info("")
}

cli_admin_setup <- list(
  users_created = c(
    "{.strong {users_group}} group created - ",
    "add users to this group that do not require admin permissions"
  ),
  users_not_created = c(
    "{.strong {users_group}} group NOT created - ",
    "a {.strong {users_group}} group already exists in your account"
  ),
  users_policies = c(
    "Added policies to the {.strong {users_group}} group: ",
    "{paste0(group_policies_data$users, collapse=", ")}"
  ),
  users_not_policies = c(
    "All policies already present in the {.strong {users_group}} group"
  ),
  admin_created = c(
    "{.strong {admin_group}} group created - ",
    "add users to this group that require admin permissions"
  ),
  admin_not_created = c(
    "{.strong {admin_group}} group NOT created - ",
    "an {.strong {admin_group}} group already exists in your account"
  ),
  admin_policies = c(
    "Added policies to the {.strong {admin_group}} group: ",
    "{paste0(group_policies_data$admin, collapse=", ")}"
  ),
  admin_not_policies = c(
    "All policies already present in the {.strong {admin_group}} group"
  )
)

#' AWS account setup for administrators
#'
#' @export
#' @param users_group (character) name for the users group. default: "users"
#' @param admin_group (character) name for the admin group. default: "admin"
#' @section What is magical:
#' - Setup a users IAM group: users that do not require admin persmissions
#' - Add policies to the users group
#' - Setup an admin IAM group: users that require admin permissions
#' - Add policies to the admin group
#' @family magicians
#' @return NULL invisibly
six_admin_setup <- function(users_group = "users", admin_group = "admin") {
  aws_whoami()

  # users
  ## create group
  if (!aws_group_exists(users_group)) {
    aws_group_create(name = users_group)
    cli_info(cli_admin_setup$users_created)
  } else {
    cli_warning(cli_admin_setup$users_not_created)
  }
  ## policies
  users <- aws_group(users_group)
  check_users_pols <- map_lgl(group_policies_data$users, \(x) has_policy(users, x))
  if (!all(check_users_pols)) {
    invisible(map(group_policies_data$users, \(p) aws_policy_attach(users, p)))
    cli_info(cli_admin_setup$users_policies)
  } else {
    cli_info(cli_admin_setup$users_not_policies)
  }
  cli_info("")

  # admin
  ## create group
  if (!aws_group_exists(admin_group)) {
    aws_group_create(name = admin_group)
    cli_info(cli_admin_setup$admin_created)
  } else {
    cli_warning(cli_admin_setup$admin_not_created)
  }
  ## policies
  admin <- aws_group(admin_group)
  check_admin_pols <- map_lgl(group_policies_data$admin, \(x) has_policy(admin, x))
  if (!all(check_admin_pols)) {
    invisible(map(group_policies_data$admin, \(p) aws_policy_attach(admin, p)))
    cli_info(cli_admin_setup$admin_policies)
  } else {
    cli_info(cli_admin_setup$admin_not_policies)
  }
  cli_info("")

  cli_info("Done!")
}
