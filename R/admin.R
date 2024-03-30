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
    "{.strong users} group created - ",
    "add users to this group that do not require admin permissions"
  ),
  users_not_created = c(
    "{.strong users} group NOT created - ",
    "a users group already exists in your account"
  ),
  users_policies = c(
    "Added policies to the {.strong users} group: ",
    "{paste0(group_policies$users, collapse=", ")}"
  ),
  users_not_policies = c(
    "Not adding policies to the {.strong users} group"
  ),
  admin_created = c(
    "{.strong admin} group created - ",
    "add users to this group that require admin permissions"
  ),
  admin_not_created = c(
    "{.strong admin} group NOT created - ",
    "an admin group already exists in your account"
  ),
  admin_policies = c(
    "Added policies to the {.strong admin} group: ",
    "{paste0(group_policies$admin, collapse=", ")}"
  ),
  admin_not_policies = c(
    "Not adding policies to the {.strong admin} group"
  )
)

#' AWS account setup for administrators
#'
#' @export
#' @section What is magical:
#' - Setup a users IAM group: users that do not require admin persmissions
#' - Add policies to the users group
#' - Setup an admin IAM group: users that require admin permissions
#' - Add policies to the admin group
#' - (NOT WORKING YET) Check that a user can be added to a group and has
#' access as expected
#' @family magicians
#' @return NULL invisibly
six_admin_setup <- function() {
  aws_whoami()
  cli_alert_info("")

  # users
  users_group_name <- "uzers"
  ## creation
  if (!aws_group_exists(users_group_name)) {
    aws_group_create(name = users_group_name)
    cli_info(cli_admin_setup$users_created)
  } else {
    cli_warning(cli_admin_setup$users_not_created)
  }
  ## policies
  users <- aws_group(users_group_name)
  check_users_pols <- map_lgl(group_policies$users, \(x) has_policy(users, x))
  if (!all(check_users_pols)) {
    invisible(map(group_policies$users, \(p) aws_policy_attach(users, p)))
    cli_info(cli_admin_setup$users_policies)
  } else {
    cli_info(cli_admin_setup$users_not_policies)
  }
  cli_alert_info("")

  # admin
  if (!aws_group_exists("admin")) {
    aws_group_create(name = "admin")
    cli_info(cli_admin_setup$admin_created)
  } else {
    cli_warning(cli_admin_setup$admin_not_created)
  }
  ## policies
  admin <- aws_group("admin")
  check_admin_pols <- map_lgl(group_policies$admin, \(x) has_policy(admin, x))
  if (!all(check_admin_pols)) {
    invisible(map(group_policies$admin, \(p) aws_policy_attach(admin, p)))
    cli_info(cli_admin_setup$admin_policies)
  } else {
    cli_info(cli_admin_setup$admin_not_policies)
  }
  cli_alert_info("")

  # user test
  # check_simulated_user("uzers") #nolint
  # cli_alert_info("") #nolint

  cli_info("Done!")
}

has_access <- function(fun, ...) {
  rlang::is_null(purrr::safely(fun, FALSE)(...)$error)
}

#' @importFrom dplyr any_of
check_simulated_user <- function(group) {
  cli_info("Checking that a simulated user can access user group")
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

  withr::with_envvar(
    creds_lst,
    {
      env64$iam <- paws::iam()
      env64$rds <- paws::rds()
      env64$redshift <- paws::redshift()
      env64$s3 <- paws::s3()
      env64$costexplorer <- paws::costexplorer()

      check_iam <- has_access(aws_user)
      check_rds <- has_access(instance_details)
      check_rs <- has_access(cluster_details)
      check_s3 <- has_access(env64$s3$list_buckets)
      check_bil <- has_access(aws_billing_raw,
        date_start = Sys.Date() - 1,
        metrics = "BlendedCost"
      )

      if (all(
        check_iam, check_rds, check_rs, check_s3, check_bil
      )) {
        cli_success("  All checks passed!")
      } else {
        cli_warning(c(
          "  At least one check didn't pass - ",
          "get in touch or use other sixtyfour functions manually"
        ))
      }
    }
  )
  env64$iam <- paws::iam()
  env64$rds <- paws::rds()
  env64$redshift <- paws::redshift()
  env64$s3 <- paws::s3()
  env64$costexplorer <- paws::costexplorer()
  cli_info("  Cleaning up simulated user")
  aws_user_remove_from_group(randuser, group)
  suppm(six_user_delete(randuser))
}
