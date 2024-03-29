#' S3 actions for reading, from the AWS managed policy
#' `AmazonS3ReadOnlyAccess`
#' @keywords internal
#' @return character vector of actions
s3_actions_read <- function() {
  c(
    "s3:Get*",
    "s3:List*",
    "s3:Describe*",
    "s3-object-lambda:Get*",
    "s3-object-lambda:List*"
  )
}

#' S3 actions for full access (read and write), from the AWS
#' managed policy `AmazonS3FullAccess`
#' @keywords internal
#' @return character vector of actions
s3_actions_full <- function() {
  c(
    "s3:*",
    "s3-object-lambda:*"
  )
}

#' Create a policy document for an S3 bucket
#' @export
#' @param bucket (character) bucket name. required
#' @param sid (character) a statement id. optional
#' @inheritParams aws_policy_statement
#' @details
#' There's this separate function for creating policy docs for S3 because
#' buckets are globally unique, so AWS figures out the region and account
#' ID for you.
#' @return a policy document as JSON (of class `json`)
#' @examplesIf interactive()
#' aws_s3_policy_doc_create(
#'   bucket = "s64-test-22",
#'   action = s3_actions_read()
#' )
aws_s3_policy_doc_create <- function(bucket, action, effect = "Allow",
                                     sid = NULL, ...) {
  # FIXME: one can put more than 1 statement in the Statement slot -
  # the below line about adding a `sid` assumes there's only 1 - which
  # does work cause we hard code just 1 statement here. Should we
  # support more? probably
  doc <- list(
    Version = "2012-10-17",
    Statement = list(
      list(
        Effect = effect,
        Action = action,
        Resource = bucket_arn(bucket)
      )
    )
  )
  if (!is.null(sid)) doc$Statement[[1]]$Sid <- sid
  jsonlite::toJSON(doc, auto_unbox = TRUE, ...)
}

add_user_already <- c(
  "{.strong {username}} already",
  " has {.strong {permissions}} access",
  " to bucket {.strong {bucket}}"
)
add_user_now_has <- c(
  "{.strong {username}}",
  " now has {.strong {permissions}} access",
  " to bucket {.strong {bucket}}"
)

#' @importFrom snakecase to_upper_camel_case
#' @keywords internal
bucket_to_policy_name <- function(bucket, permissions) {
  perm <- switch(permissions, read = "ReadOnlyAccess", write = "FullAccess")
  glue("S3{perm}{to_upper_camel_case(bucket)}")
}

create_policy_if_missing <- function(bucket, permissions) {
  policy_name <- bucket_to_policy_name(bucket, permissions)
  if (aws_policy_exists(policy_name)) return(invisible())
  mydoc <- aws_s3_policy_doc_create(
    bucket = bucket,
    action = switch(permissions,
      read = s3_actions_read(),
      write = s3_actions_full()
    )
  )
  aws_policy_create(policy_name, document = mydoc)
}

#' Add a user to a bucket
#' @export
#' @param bucket (character) bucket name. required
#' @param username (character) A user name. required
#' @param permissions (character) user permissions, one of
#' read or write. write includes read
#' @section Permissions:
#' - read: read only; not allowed to write or do admin tasks
#' - write: write (in addition to read); includes deleting files; does
#' not include deleting buckets
#' - admin: change user permissions (in addition to read and write);
#' includes deleting buckets (THIS OPTION NOT ACCEPTED YET!)
#' @return invisibly returns nothing
#' @examplesIf interactive()
#' aws_bucket_add_user(
#'   bucket = "s64-test-22",
#'   username = "jane",
#'   permissions = "read"
#' )
#' \dontrun{
#' # not a valid permissions string
#' aws_bucket_add_user(
#'   bucket = "mybucket",
#'   username = "userdmgziqpt",
#'   permissions = "notavalidpermission"
#' )
#' }
aws_bucket_add_user <- function(bucket, username, permissions) {
  stopifnot(
    "permissions must be one of read or write" =
      permissions %in% c("read", "write")
  )
  stopifnot("permissions must be length 1" = length(permissions) == 1)

  policy_name <- bucket_to_policy_name(bucket, permissions)
  create_policy_if_missing(bucket, permissions)
  user_data <- aws_user(username)
  if (NROW(user_data$attached_policies) == 0) {
    aws_user(username) %>% aws_policy_attach(policy_name)
    cli::cli_alert_success(add_user_now_has)
    return(invisible())
  }
  if (policy_name %in% user_data$attached_policies$PolicyName) {
    cli::cli_alert_success(add_user_already)
  } else {
    aws_user(username) %>% aws_policy_attach(policy_name)
    cli::cli_alert_success(add_user_now_has)
  }
  invisible()
}

#' Change user permissions for a bucket
#' @export
#' @importFrom purrr discard
#' @importFrom dplyr starts_with
#' @inheritParams aws_bucket_add_user
#' @return invisibly returns nothing
#' @section Important:
#' This function is built around policies named by this package. If you use
#' your own policies that you name this function may not work.
#' @examplesIf interactive()
#' # create user
#' if (!aws_user_exists("jane")) {
#'   aws_user_create("jane")
#' }
#'
#' # user doesn't have any permissions for the bucket
#' # - use aws_bucket_add_user to add permissions
#' aws_bucket_change_user(bucket="s64-test-22",
#'   username="jane", permissions = "read")
#' aws_bucket_add_user(bucket="s64-test-22", username="jane",
#'   permissions = "read")
#'
#' # want to change to read to write, makes the change
#' aws_bucket_change_user(bucket="s64-test-22", username="jane",
#'   permissions = "write")
#'
#' # want to change to write - but already has read
#' aws_bucket_change_user(bucket="s64-test-22", username="jane",
#'   permissions = "read")
aws_bucket_change_user <- function(bucket, username, permissions) {
  stopifnot(
    "permissions must be one of read or write" =
      permissions %in% c("read", "write")
  )
  stopifnot("permissions must be length 1" = length(permissions) == 1)

  perms <- filter(aws_bucket_permissions(bucket), user == username)
  if (NROW(perms) == 0) {
    cli::cli_alert_warning(c(
      "No {.strong {bucket}} specific permissions",
      " found for {.strong {username}}"
    ))
    cli::cli_alert_info(c(
      "Use {.strong aws_bucket_add_user} to add a user to a bucket"
    ))
    return(invisible())
  }

  if (grepl(permissions, perms$permissions)) {
    cli::cli_alert_success(add_user_already)
    return(invisible())
  }

  # detach policies that do not have the target permissions
  if (glue("policy_{permissions}") %in% names(perms)) {
    perms <- perms %>%
      select(!(!!glue("policy_{permissions}")))
  }
  policies_to_detach <- perms %>%
    select(starts_with("policy")) %>%
    as.character() %>%
    discard(rlang::is_na)
  map(policies_to_detach, \(policy) {
    aws_policy_detach(aws_user(username), policy)
  })

  # create new policy if needed
  create_policy_if_missing(bucket, permissions)

  # attach new policy to the user
  policy_name <- bucket_to_policy_name(bucket, permissions)
  aws_user(username) %>% aws_policy_attach(policy_name)

  # let em know
  cli::cli_alert_success(add_user_now_has)

  invisible()
}

#' Remove a user from a bucket
#' @export
#' @inheritParams aws_bucket_add_user
#' @autoglobal
#' @details This function detaches a policy from a user for accessing
#' the bucket; the policy itself is untouched
#' @return invisibly returns nothing
#' @examplesIf interactive()
#' aws_bucket_remove_user("s64-test-22", "scott")
aws_bucket_remove_user <- function(bucket, username) {
  perms <- permissions_user_bucket(bucket) %>%
    filter(user == username)
  if (NROW(perms) == 0) {
    cli::cli_alert_warning(c(
      "No {.strong {bucket}} specific permissions",
      " found for {.strong {username}}"
    ))
    return(invisible())
  }

  userobj <- aws_user(username)
  map(perms$PolicyName, \(policy) aws_policy_detach(userobj, policy))

  cli::cli_alert_success(c(
    "{.strong {username}} access to",
    " {.strong {bucket}} has been removed"
  ))
  invisible()
}

#' Get permissions for a bucket
#' @export
#' @importFrom purrr keep
#' @importFrom dplyr case_when distinct group_by ungroup rowwise select
#' @importFrom cli cli_abort
#' @importFrom tidyr pivot_wider
#' @inheritParams aws_bucket_add_user
#' @autoglobal
#' @return tibble with a row for each user, with columns:
#' - user
#' - permissions
#'
#' Note that users with no persmissions are not shown; see [aws_users()]
#' @examplesIf interactive()
#' aws_bucket_permissions("s64-test-22")
aws_bucket_permissions <- function(bucket) {
  if (!aws_bucket_exists(bucket)) {
    cli::cli_abort("{.strong {bucket}} does not exist")
  }
  user_perms <-
    permissions_user_bucket(bucket) %>%
    mutate(
      permissions = case_when(
        grepl("read", tolower(PolicyName)) ~ "read",
        grepl("full", tolower(PolicyName)) ~ "write"
      )
    ) %>%
    select(user, permissions, PolicyName)

  group_perms <- permissions_groups()

  bind_rows(user_perms, group_perms) %>%
    group_by(user) %>%
    mutate(permissions2 = paste0(permissions, collapse = ",")) %>%
    pivot_wider(
      names_from = permissions,
      values_from = PolicyName,
      names_prefix = "policy_"
    ) %>%
    distinct() %>%
    ungroup() %>%
    rename(permissions = permissions2)
}

creds_template <- "Hi,

Here's your AWS credentials for your username {username}.

Make sure to save these in a place where you won't lose them.
For example, save them in your .Renviron file (run
`usethis::edit_r_environ()` to open your .Renviron file):

AWS_ACCESS_KEY_ID={creds$AccessKey$AccessKeyId}
AWS_SECRET_ACCESS_KEY={creds$AccessKey$SecretAccessKey}
AWS_REGION={Sys.getenv('AWS_REGION')}
"

#' Create access keys for a user
#'
#' Creates a new Amazon Web Services secret access key and
#' corresponding Amazon Web Services access key ID
#'
#' @export
#' @importFrom cli cli_alert_success cli_alert_info
#' @importFrom dplyr case_match
#' @importFrom clipr write_clip
#' @param username (character) A user name. required
#' @param copy_to_cp (logical) Copy to clipboard. Default: `FALSE`. See
#' section "Clipboard" below for more details.
#' @details A user can have more than one pair of access keys.
#' By default a user can have up to 2 pairs of access keys.
#' Using this function will not replace an existing set of keys;
#' but instead adds an additional set of keys.
#'
#' See <https://rstats.wtf/r-startup.html> for help on bringing in secrets
#' to an R session.
#' @section Important:
#' Save the secret key after running this function as it can not be
#' viewed again.
#' @section Clipboard:
#' If you set `copy_to_cp=TRUE` we'll copy to your clipboard an
#' email template with the credentials and a small amount of instructions.
#' Please do edit that email with information tailored to your
#' group and how you'd like to store secrets.
#' @section Known error behaviors:
#' - `LimitExceeded (HTTP 409). Cannot exceed quota for AccessKeysPerUser: 2`
#' - `NoSuchEntity (HTTP 404). The user with name xxx cannot be found.`
#' @return NULL invisibly
#' @seealso [aws_user_access_key()], [aws_user_access_key_delete()]
#' @examplesIf interactive()
#' if (!aws_user_exists("jane")) aws_user_create("jane")
#' aws_user_creds("jane")
#' aws_user_access_key("jane")
#' aws_user_creds("jane", copy_to_cp = TRUE)
aws_user_creds <- function(username, copy_to_cp = FALSE) {
  creds <- tryCatch(
    env64$iam$create_access_key(UserName = username),
    error = function(e) e
  )

  if (rlang::is_error(creds)) {
    help_msg <- if (grepl("LimitExceeded", creds$message)) {
      "See {.strong aws_user_access_key_delete}"
    } else if (grepl("NoSuchEntity", creds$message)) {
      "Check username spelling? Add a user with {.strong aws_user_create}"
    } else {
      ""
    }
    cli_abort(c(creds$message, help_msg))
  }

  cli_alert_success("Key pair created for {.strong {username}}")
  for (i in seq_along(creds$AccessKey)) {
    cli_alert_info("{names(creds$AccessKey)[i]}: {creds$AccessKey[[i]]}")
  }

  if (copy_to_cp) {
    cli_alert_info("Email template copied to your clipboard")
    glue(creds_template)
    clipr::write_clip(glue(creds_template))
  }

  invisible()
}

#' @autoglobal
permissions_user_bucket <- function(bucket) {
  aws_user_mem <- memoise::memoise(aws_user)
  aws_users()$UserName %>%
    keep(\(user) NROW(aws_user_mem(user)$attached_policies) > 0) %>%
    map(\(user) {
      aws_user_mem(user)$attached_policies %>%
        rowwise() %>%
        mutate(
          user = user,
          resource_arn = list(
            latest_policy_doc(PolicyArn)$Statement$Resource
          )
        ) %>%
        ungroup()
    }) %>%
    list_rbind() %>%
    filter(map_lgl(resource_arn, \(w) any(grepl(bucket, unlist(w)))))
}

#' @autoglobal
permissions_groups <- function() {
  aws_user_mem <- memoise::memoise(aws_user)
  aws_users()$UserName %>%
    keep(\(user) length(aws_user_mem(user)$groups$Groups) > 0) %>%
    map(\(user) {
      tibble(
        user = user,
        group = map_chr(aws_user_mem(user)$groups$Groups, "GroupName")
      )
    }) %>%
    list_rbind() %>%
    filter(group == "admin") %>%
    rename(permissions = group)
}

latest_policy_version_id <- memoise::memoise(function(arn) {
  vers <- env64$iam$list_policy_versions(arn)$Versions
  Filter(function(z) z$IsDefaultVersion, vers)[[1]]$VersionId
})

#' @importFrom curl curl_unescape
latest_policy_doc <- memoise::memoise(function(arn) {
  res <- env64$iam$get_policy_version(
    arn,
    latest_policy_version_id(arn)
  )
  doc <- curl::curl_unescape(res$PolicyVersion$Document)
  jsonlite::fromJSON(doc)
})