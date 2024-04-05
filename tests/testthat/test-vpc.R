skip_if_not(localstack_available(), "LocalStack Not Available")

Sys.setenv(AWS_PROFILE = "localstack")
purge_sec_grps <- function() {
  groups <- aws_vpc_security_groups()
  map(
    purrr::map_vec(groups$SecurityGroups, "GroupId"),
    \(id) con_ec2()$delete_security_group(GroupId = id)
  )
}

test_that("aws_vpcs", {
  res <- aws_vpcs()

  expect_type(res, "list")
  expect_named(res, c("Vpcs", "NextToken"))
  expect_equal(length(res$Vpcs), 1)
  expect_equal(res$Vpcs[[1]]$InstanceTenancy, "default")
})

test_that("aws_vpc", {
  vpclist <- aws_vpcs()
  res <- aws_vpc(vpclist$Vpcs[[1]]$VpcId)

  expect_type(res, "list")
  expect_named(res, c("Vpcs", "NextToken"))
  expect_equal(length(res$Vpcs), 1)
  expect_equal(res$Vpcs[[1]]$InstanceTenancy, "default")
})

test_that("aws_vpc_security_groups", {
  purge_sec_grps()
  out <- aws_vpc_security_groups()

  expect_type(out, "list")
  expect_length(out$SecurityGroups, 0)
})

test_that("aws_vpc_security_group", {
  purge_sec_grps()
  aws_vpc_security_group_create(
    name = "testing1",
    description = "Testing security group creation"
  )
  groups <- aws_vpc_security_groups()
  out <- aws_vpc_security_group(groups$SecurityGroups[[1]]$GroupId)

  expect_type(out, "list")
  expect_length(out$SecurityGroups, 1)
  expect_equal(out$SecurityGroups[[1]]$GroupName, "testing1")
})

test_that("aws_vpc_security_group_create", {
  purge_sec_grps()
  group <- aws_vpc_security_group_create(name = "atest")
  expect_type(group, "list")
  expect_named(group, c("GroupId", "Tags"))
  expect_match(group$GroupId, "sg-")
  expect_length(group$Tags, 0)

  created_group <- aws_vpc_security_group(group$GroupId)

  expect_type(created_group, "list")
  # mariadb is default `engine` value, used in description if
  # description is NULL
  expect_match(created_group$SecurityGroups[[1]]$Description, "mariadb")
})

test_that("aws_vpc_security_group_create - with tags", {
  purge_sec_grps()
  group <- aws_vpc_security_group_create(
    name = "testing4",
    tags = list(
      list(
        ResourceType = "security-group",
        Tags = list(
          list(
            Key = "sky",
            Value = "blue"
          )
        )
      )
    )
  )

  expect_type(group, "list")
  expect_length(group$Tags, 1)
  expect_named(group$Tags[[1]], c("Key", "Value"))

  created_group <- aws_vpc_security_group(group$GroupId)
  tags <- created_group$SecurityGroups[[1]]$Tags

  expect_type(created_group, "list")
  expect_length(tags, 1)
  expect_named(tags[[1]], c("Key", "Value"))
  expect_equal(tags[[1]]$Key, "sky")
  expect_equal(tags[[1]]$Value, "blue")
})

test_that("aws_vpc_security_group_ingress", {
  # ipperms <- ip_permissions_generator("mariadb")
  # ipperms$IpRanges[[1]]$CidrIp <- "123.156.222.198/32"
  # ipperms$IpRanges[[1]]$Description <- "Access for someuser from sixtyfour"
  # save(ipperms, file = "tests/testthat/ipperms.rda")
  load("ipperms.rda")

  expect_error(aws_vpc_security_group_ingress())

  purge_sec_grps()
  group <- aws_vpc_security_group_create(name = "testing423")
  out <- aws_vpc_security_group_ingress(
    id = group$GroupId,
    ip_permissions = ipperms)

  expect_type(out, "list")
  expect_named(out, c("Return", "SecurityGroupRules"))
  expect_true(out$Return)
  # security group rule starts with "sgr-"
  expect_match(out$SecurityGroupRules[[1]]$SecurityGroupRuleId, "sgr-")
  # security group starts with just "sg-"
  expect_match(out$SecurityGroupRules[[1]]$GroupId, "sg-")
})

# cleanup
Sys.unsetenv("AWS_PROFILE")
