## TODO: not looked over this file yet (commented out below so package can run)

# sss <- paws::s3()

# sss$create_bucket(Bucket = "s64-test-3-private",
#                   CreateBucketConfiguration = list(LocationConstraint = "us-west-2"))

# sss$put_public_access_block(Bucket = "s64-test-2",
#                             PublicAccessBlockConfiguration = list(
#                               BlockPublicPolicy = FALSE,
#                               RestrictPublicBuckets = FALSE
#                             ))

# sss$put_object(
#   Body = "~/Desktop/koth.png",
#   Bucket = "s64-test-2",
#   Key = basename("~/Desktop/koth.png"),
#   Tagging = NULL)

# sss$put_object_acl(
#   AccessControlPolicy = structure(
#     list(),
#     names = character(
#       0
#     )
#   ),
#   Bucket = "s64-test-2",
#   Key = basename("~/Desktop/koth.png"),
#   GrantRead = "uri=http://acs.amazonaws.com/groups/global/AllUsers"
# )

# sss$list_objects("s64-test-2")

# policy <- '{
#   "Version":"2012-10-17",
#   "Statement":[{
#     "Sid":"PublicReadGetObject",
#     "Effect":"Allow",
#     "Principal": "*",
#     "Action":"s3:GetObject",
#     "Resource":"arn:aws:s3:::s64-test-2/*"
#   }]
# }'

# sss$put_bucket_policy(Bucket = "s64-test-2", Policy = policy)

# sss$object

# buckets <- sss$list_buckets()
# buckets[[1]] |>
#   map_chr(~ .x$Name)

# sss$put_bucket_ownership_controls(Bucket = "s64-test-2",
#                                   OwnershipControls = list(
#                                     Rules = list(
#                                       list(
#                                         ObjectOwnership = "ObjectWriter"
#                                       )
#                                     )
#                                   ))

# sss$get_bucket_ownership_controls(Bucket = "s64-test-2")
