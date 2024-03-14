#' Add a user to a bucket
#' @export
#' @param bucket (character) bucket name. required
#' @param username (character) A user name. required
#' @param permissions (character) user permissions, one or more of:
#' read, write, admin
#' @section Permissions:
#' - read: read only; not allowed to write or do admin tasks
#' - write: write (in addition to read); includes deleting files; does
#' not include deleting buckets
#' - admin: change user permissions (in addition to read and write);
#' includes deleting buckets
#' @return xxx
#' @examples
#' aws_bucket_add_user("mybucket", "sean", "write")
#' aws_bucket_add_user("mybucket", "sean", c("write", "read"))
aws_bucket_add_user <- function(bucket, username, permissions) {

}

#' Change user or group permissions for a bucket
#' @export
#' @inheritParams add_user_to_bucket
#' @return xxx
#' @examples
#' aws_bucket_change_user("mybucket", "sean", "write")
#' aws_bucket_change_user("mybucket", "sean", c("write", "read"))
aws_bucket_change_user <- function(bucket, username, permissions) {

}

#' Remove a user from a bucket
#' @export
#' @inheritParams add_user_to_bucket
#' @return xxx
#' @examples
#' aws_bucket_remove_user("mybucket", "sean")
aws_bucket_remove_user <- function(bucket, username) {

}

#' Get permissions for a bucket
#' @export
#' @inheritParams add_user_to_bucket
#' @return tibble with columns:
#' - user
#' - permissions
#' @examples
#' aws_bucket_get_permissions("mybucket")
aws_bucket_get_permissions <- function(bucket) {

}
#> User     Permissions
#> Sean     read
#> Scott    write
