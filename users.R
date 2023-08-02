library(paws)

batman <- paws::iam()

batman$create_user(UserName = "testUser1")

testUser1_keys <- batman$create_access_key("testUser1")

batman$list_users()
