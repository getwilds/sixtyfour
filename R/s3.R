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
#' @inheritParams aws_policy_document_create
#' @details
#' There's this separate function for creating policy docs for S3 because
#' buckets are globally unique, so AWS figures out the region and account
#' ID for you.
#' @examplesIf interactive()
#' aws_s3_policy_doc_create(
#'   bucket = "s64-test-22",
#'   action = s3_actions_read()
#' )
aws_s3_policy_doc_create <- function(bucket, action, effect = "Allow",
                                     sid = NULL, ...) {
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

#' Add a user to a bucket
#' @export
#' @importFrom snakecase to_upper_camel_case
#' @param bucket (character) bucket name. required
#' @param username (character) A user name. required
#' @param permissions (character) user permissions, one of
#' read or write. write includes read
#' @section Permissions:
#' - read: read only; not allowed to write or do admin tasks
#' - write: write (in addition to read); includes deleting files; does
#' not include deleting buckets
#' - admin: change user permissions (in addition to read and write);
#' includes deleting buckets
#' @return invisibly returns nothing
#' @examplesIf interactive()
#' aws_bucket_add_user(
#'   bucket = "s64-test-22",
#'   username = "scott",
#'   permissions = "read"
#' )
#' \dontrun{
#' aws_bucket_add_user("mybucket", "scott", permissions = "stuff")
#' }
aws_bucket_add_user <- function(bucket, username, permissions) {
  stopifnot(
    "permissions must be one of read or write" =
      permissions %in% c("read", "write")
  )
  stopifnot("permissions must be length 1" = length(permissions) == 1)

  # refresh memoised policies data before proceeding
  invisible(aws_policies(refresh = TRUE))

  policy_name <- glue("S3ReadOnlyAccess{to_upper_camel_case(bucket)}")
  if (!aws_policy_exists(policy_name)) {
    mydoc <- aws_s3_policy_doc_create(
      bucket = bucket,
      action = switch(permissions,
        read = s3_actions_read(),
        write = s3_actions_full()
      )
    )
    aws_policy_create(policy_name, document = mydoc)
  }
  user_data <- aws_user(username)
  if (policy_name %in% user_data$attached_policies$PolicyName) {
    cli::cli_alert_success(c(
      "{.strong {username}} already",
      " has {.strong {permissions}} access",
      " to bucket {.strong {bucket}}"
    ))
  } else {
    aws_user(username) %>% aws_policy_attach(policy_name)
    cli::cli_alert_success(c(
      "{.strong {username}}",
      " now has {.strong {permissions}} access",
      " to bucket {.strong {bucket}}"
    ))
  }
}

#' Change user or group permissions for a bucket
#' @export
#' @inheritParams aws_bucket_add_user
#' @return xxx
#' @examplesIf interactive()
#' aws_bucket_change_user("mybucket", "sean", "write")
#' aws_bucket_change_user("mybucket", "sean", c("write", "read"))
aws_bucket_change_user <- function(bucket, username, permissions) {
  abort("not working yet")
}

#' Remove a user from a bucket
#' @export
#' @inheritParams aws_bucket_add_user
#' @autoglobal
#' @details This function detaches a policy from a user for accessing
#' the bucket; the policy itself is untouched
#' @return inviv
#' @examplesIf interactive()
#' aws_bucket_remove_user("s64-test-22", "scott")
aws_bucket_remove_user <- function(bucket, username) {
  perms <- permissions_user_bucket(bucket) %>%
    filter(user == username)
  userobj <- aws_user(username)
  if (NROW(perms) == 0) {
    cli::cli_alert_warning(c(
      "No {.strong {bucket}} specific permissions",
      " found for {.strong {username}}"
    ))
    return(invisible(NULL))
  }

  map(perms$PolicyName, \(policy) aws_policy_detach(userobj, policy))

  cli::cli_alert_success(c(
    "{.strong {username}} access to",
    " {.strong {bucket}} has been removed"
  ))
  cli::cli_alert_info(
    "Note: group permissions are unaltered; see {.strong ?aws_groups}"
  )
}

#' Get permissions for a bucket
#' @export
#' @importFrom purrr keep
#' @importFrom dplyr case_when distinct group_by ungroup rowwise select
#' @inheritParams aws_bucket_add_user
#' @autoglobal
#' @return tibble with a row for each user, with columns:
#' - user
#' - permissions
#'
#' Note that users with no persmissions are not shown; see [aws_users()]
#' @examplesIf interactive()
#' aws_bucket_get_permissions("s64-test-22")
aws_bucket_get_permissions <- function(bucket) {
  user_perms <-
    permissions_user_bucket(bucket) %>%
    mutate(
      permissions = case_when(
        grepl("read", tolower(PolicyName)) ~ "read",
        grepl("full", tolower(PolicyName)) ~ "write"
      )
    ) %>%
    select(user, permissions)

  group_perms <- permissions_groups()

  bind_rows(user_perms, group_perms) %>%
    group_by(user) %>%
    mutate(permissions = paste0(permissions, collapse = ",")) %>%
    distinct() %>%
    ungroup()
}

#' @autoglobal
permissions_user_bucket <- function(bucket) {
  aws_users()$UserName %>%
    keep(\(user) NROW(aws_user(user)$attached_policies) > 0) %>%
    map(\(user) {
      aws_user(user)$attached_policies %>%
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
  aws_users()$UserName %>%
    keep(\(user) length(aws_user(user)$groups$Groups) > 0) %>%
    map(\(user) {
      tibble(
        user = user,
        group = map_chr(aws_user(user)$groups$Groups, "GroupName")
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
