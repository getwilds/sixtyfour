test_that("group_policies", {
  expect_error(group_policies())
  expect_error(group_policies(5))
  expect_error(group_policies("adfasd"))

  expect_type(group_policies("admin"), "character")
  expect_length(group_policies("admin"), 5)
  expect_type(group_policies("users"), "character")
  expect_length(group_policies("users"), 5)
})
