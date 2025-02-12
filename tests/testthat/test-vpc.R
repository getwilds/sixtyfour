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
  # nolint start
  # ipperms <- ip_permissions_generator("mariadb")
  # ipperms$IpRanges[[1]]$CidrIp <- "123.156.222.198/32"
  # ipperms$IpRanges[[1]]$Description <- "Access for someuser from sixtyfour"
  # save(ipperms, file = "tests/testthat/ipperms.rda", version = 2)
  # nolint end
  load("ipperms.rda")

  expect_error(aws_vpc_security_group_ingress())

  purge_sec_grps()
  group <- aws_vpc_security_group_create(name = "testing423")
  out <- aws_vpc_security_group_ingress(
    id = group$GroupId,
    ip_permissions = ipperms
  )

  expect_type(out, "list")
  expect_named(out, c("Return", "SecurityGroupRules"))
  expect_true(out$Return)
  # security group rule starts with "sgr-"
  expect_match(out$SecurityGroupRules[[1]]$SecurityGroupRuleId, "sgr-")
  # security group starts with just "sg-"
  expect_match(out$SecurityGroupRules[[1]]$GroupId, "sg-")
})

test_that("aws_vpc_sg_with_ingress", {
  expect_error(aws_vpc_sg_with_ingress())

  res <- aws_vpc_sg_with_ingress("mariadb")
  expect_type(res, "character")
  expect_match(res, "sg-")

  out <- aws_vpc_security_group(res)
  expect_type(out, "list")
  expect_length(out$SecurityGroups, 1)
  expect_match(out$SecurityGroups[[1]]$GroupName, "mariadb-")

  res_mysql <- aws_vpc_sg_with_ingress("mysql")
  out_mysql <- aws_vpc_security_group(res_mysql)
  expect_match(out_mysql$SecurityGroups[[1]]$GroupName, "mysql-")
})

test_that("security_group_handler", {
  # missing `id` param
  expect_error(security_group_handler())
  # if `id` given and not `NULL`, returns itself
  an_id <- 123
  expect_equal(security_group_handler(an_id), an_id)
  # if `id` given and IS `NULL`, errors b/c `engine` missing
  expect_error(security_group_handler(NULL))
  # if `id` given, engine value not supported
  expect_error(security_group_handler(NULL, engine = "asdff"))
})

test_that("aws_vpc_sec_group_rules_mod works for ipv6 address", {
  local_mocked_bindings(
    .ip_address = function() {
      as.character(ipaddress::sample_ipv6(1))
    }
  )

  a_grp_name <- random_string("vpcsecgroup")
  x <- aws_vpc_security_group_create(name = a_grp_name)

  perms <- ip_permissions_generator("mariadb")

  expect_true(
    ipaddress::is_ipv6(
      ipaddress::as_ip_network(perms$Ipv6Ranges[[1]]$CidrIpv6)
    )
  )

  a_rule <- aws_vpc_security_group_ingress(
    id = x$GroupId,
    ip_permissions = perms
  )

  expect_type(a_rule, "list")
  expect_named(a_rule, c("Return", "SecurityGroupRules"))
  expect_true(a_rule$Return)
})

test_that("aws_vpc_sec_group_rules_mod works for ipv4 address", {
  local_mocked_bindings(
    .ip_address = function() {
      as.character(ipaddress::sample_ipv4(1))
    }
  )

  a_grp_name <- random_string("vpcsecgroup")
  x <- aws_vpc_security_group_create(name = a_grp_name)

  perms <- ip_permissions_generator("mariadb")

  expect_true(
    ipaddress::is_ipv4(
      ipaddress::as_ip_network(perms$IpRanges[[1]]$CidrIp)
    )
  )

  a_rule <- aws_vpc_security_group_ingress(
    id = x$GroupId,
    ip_permissions = perms
  )

  expect_type(a_rule, "list")
  expect_named(a_rule, c("Return", "SecurityGroupRules"))
  expect_true(a_rule$Return)
})

# cleanup
Sys.unsetenv("AWS_PROFILE")
