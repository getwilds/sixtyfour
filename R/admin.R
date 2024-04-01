group_policies <- list(
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

aws_whoami <- function() {
  user <- aws_user() #nolint
  cli_info("whoami: {user$user$UserName} (account: {account_id()})")
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
    "{paste0(group_policies$users, collapse=", ")}"
  ),
  users_not_policies = c(
    "Not adding policies to the {.strong {users_group}} group"
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
    "{paste0(group_policies$admin, collapse=", ")}"
  ),
  admin_not_policies = c(
    "Not adding policies to the {.strong {admin_group}} group"
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
#' - (NOT WORKING YET) Check that a user can be added to a group and has access
#' as expected (requires additional package \pkg{callr})
#' @family magicians
#' @return NULL invisibly
six_admin_setup <- function(users_group = "users", admin_group = "admin") {
  aws_whoami()
  cli_alert_info("")

  # users
  ## creation
  if (!aws_group_exists(users_group)) {
    aws_group_create(name = users_group)
    cli_info(cli_admin_setup$users_created)
  } else {
    cli_warning(cli_admin_setup$users_not_created)
  }
  ## policies
  users <- aws_group(users_group)
  check_users_pols <- map_lgl(group_policies$users, \(x) has_policy(users, x))
  if (!all(check_users_pols)) {
    invisible(map(group_policies$users, \(p) aws_policy_attach(users, p)))
    cli_info(cli_admin_setup$users_policies)
  } else {
    cli_info(cli_admin_setup$users_not_policies)
  }
  cli_alert_info("")

  # admin
  ## creation
  if (!aws_group_exists(admin_group)) {
    aws_group_create(name = admin_group)
    cli_info(cli_admin_setup$admin_created)
  } else {
    cli_warning(cli_admin_setup$admin_not_created)
  }
  ## policies
  admin <- aws_group(admin_group)
  check_admin_pols <- map_lgl(group_policies$admin, \(x) has_policy(admin, x))
  if (!all(check_admin_pols)) {
    invisible(map(group_policies$admin, \(p) aws_policy_attach(admin, p)))
    cli_info(cli_admin_setup$admin_policies)
  } else {
    cli_info(cli_admin_setup$admin_not_policies)
  }
  cli_alert_info("")

  # user test
  # check_simulated_user(users_group) #nolint

  cli_info("Done!")
}

#' Check if a user has access to an AWS service
#' @export
#' @param fun (funcction) a function. required
#' @param ... additional named args passed to `fun`
#' @return single boolean. checks [rlang::is_null()] against `$error` result of
#' call to [purrr::safely()]
#' @details really just a generic check that any function can run with
#' its inputs; not specific to AWS or any particular function
has_access <- function(fun, ...) {
  rlang::is_null(purrr::safely(fun, FALSE)(...)$error)
}

#' @importFrom dplyr any_of
check_simulated_user <- function(group) {
  rlang::check_installed("callr")
  cli_info("Checking that a simulated user can access {.strong {group}} group")
  randuser <- random_user()
  creds <- suppm(six_user_create(randuser))
  aws_user_add_to_group(randuser, group)

  creds_mapper <- c(
    "AWS_ACCESS_KEY_ID" = "AccessKeyId",
    "AWS_SECRET_ACCESS_KEY" = "SecretAccessKey",
    "AWS_REGION" = "AwsRegion"
  )
  creds_lst <- as_tibble(creds) %>%
    rename(any_of(creds_mapper)) %>%
    select(starts_with("AWS")) %>%
    as.list()

  all_checks <- callr::r(function(creds) {
    withr::with_envvar(
      creds, {
        check_iam <- sixtyfour::has_access(sixtyfour::aws_user)
        check_rds <- sixtyfour::has_access(sixtyfour::aws_db_instance_details)
        check_rs <- sixtyfour::has_access(sixtyfour::aws_db_cluster_details)
        check_s3 <- sixtyfour::has_access(sixtyfour::aws_buckets)
        check_bil <- sixtyfour::has_access(sixtyfour::aws_billing_raw,
          date_start = Sys.Date() - 1,
          metrics = "BlendedCost"
        )
        list(
          IAM = check_iam,
          RDS = check_rds,
          Redshift = check_rs,
          S3 = check_s3,
          Billing = check_bil
        )
      }
    )
  }, args = list(creds_lst))

  if (all(unlist(all_checks))) {
    cli_success("  All checks passed!")
  } else {
    cli_warning(c(
      "  At least one check didn't pass ",
      "({names(keep(all_checks, isFALSE))}) ",
      "try again or open an issue"
    ))
  }

  cli_info("  Cleaning up simulated user")
  aws_user_remove_from_group(randuser, group)
  suppm(six_user_delete(randuser))
  cli_alert_info("") #nolint
}
