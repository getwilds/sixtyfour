#' Get a security group with one ingress rule based on the engine
#' @export
#' @param engine (character) The engine to use. default: "mariadb". required.
#' one of: mariadb, mysql, postgres, or redshift
#' @details Adds an ingress rule specific to the `engine` supplied (port
#' changes based on the engine), and your IP address. To create your own
#' security group and ingress rules see [aws_vpc_security_group_create()]
#' and [aws_vpc_security_group_ingress()]
#' @family security groups
#' @return (character) security group ID
aws_vpc_sg_with_ingress <- function(engine) {
  sg <- aws_vpc_security_group_create(
    name = glue("{engine}-{paste0(sample(1:9, size = 4), collapse = '')}"),
    engine = engine
  )
  aws_vpc_security_group_ingress(
    id = sg$GroupId,
    ip_permissions = ip_permissions_generator(engine)
  )
  sg$GroupId
}

picker <- function(msg, choices, .envir = parent.frame()) {
  cli::cli_inform(msg, .envir = .envir)
  utils::menu(choices)
}

#' @importFrom purrr map_lgl pluck
#' @autoglobal
#' @keywords internal
security_group_handler <- function(ids, engine) {
  if (!is.null(ids)) {
    return(ids)
  }
  port <- engine2port(engine) # nolint
  ip <- .ip_address() # nolint
  sgs <- aws_vpc_security_groups()
  sgsdf <- jsonlite::fromJSON(
    jsonlite::toJSON(sgs$SecurityGroups, auto_unbox = TRUE)
  )
  if (is_empty(sgsdf)) sgsdf <- tibble(IpPermissions = list())

  port_df <- dplyr::filter(
    sgsdf,
    map_lgl(
      IpPermissions,
      ~ ifelse(rlang::is_empty(.), FALSE, .$ToPort == port)
    )
  )
  if (!NROW(port_df)) {
    cli::cli_alert_danger(c(
      "No security groups with access for ",
      "{.strong {engine}} and port {.strong {port}}"
    ))
    cli::cli_alert_info(c(
      "Creating security group with access for ",
      "{.strong {engine}} and port {.strong {port}}"
    ))
    trysg <- tryCatch(aws_vpc_sg_with_ingress(engine), error = function(e) e)
    if (rlang::is_error(trysg)) {
      cli::cli_alert_danger(c(
        "An error occurred while creating the security group; ",
        "please use paramater {.strong security_group_ids}"
      ))
      return(NULL)
    } else {
      cli::cli_alert_success("Using security group {.strong {trysg}}")
      return(trysg)
    }
  }

  ip_df <- dplyr::filter(
    port_df,
    map_lgl(IpPermissions, ~ any(grepl(ip, pluck(.$IpRanges, 1, "CidrIp"))))
  )
  if (!NROW(ip_df)) {
    cli::cli_alert_danger(c(
      "Found security groups w/ access for {.strong {engine}}, ",
      "{.emph but} not with your IP address {.strong {ip}}"
    ))
    cli::cli_alert_info("Which security group do you want to modify?")
    pick_sg_options <-
      port_df %>%
      glue::glue_data(
        "Security Group: {GroupId}\n",
        "   Group Name: {GroupName}\n",
        "   Description: {Description}",
        .trim = FALSE
      ) %>%
      as.character()

    picked <- picker(
      c(
        glue("We found {length(pick_sg_options)} security groups"),
        "Which security group do you want to use?"
      ),
      pick_sg_options
    )

    if (picked == 0) {
      cli::cli_alert_danger(
        "No security group selected; please use ",
        "paramater {.strong security_group_ids}"
      )
      return(NULL)
    } else {
      picked_id <- port_df[picked, "GroupId"]
    }
    cli::cli_alert_info(c(
      "Adding your IP address {.strong {ip}} to security ",
      "group {.strong {picked_id}}"
    ))
    try_ingress <- tryCatch(
      {
        aws_vpc_security_group_ingress(
          id = picked_id,
          ip_permissions = ip_permissions_generator(engine)
        )
      },
      error = function(e) e
    )
    if (rlang::is_error(try_ingress)) {
      cli::cli_alert_danger(c(
        "An error occurred while creating the security group; ",
        "please use paramater {.strong security_group_ids}"
      ))
      return(NULL)
    } else {
      cli::cli_alert_success("Using security group {.strong {picked_id}}")
      return(picked_id)
    }
  }

  if (NROW(ip_df) == 1) {
    cli::cli_alert_success(c(
      "Found security group {.strong {ip_df$GroupId}} ",
      "w/ access for {.strong {engine}} and your IP address {.strong {ip}}"
    ))
    ip_df$GroupId
  } else {
    sgoptions <-
      ip_df %>%
      glue::glue_data(
        "Security Group: {GroupId}\n",
        "   Group Name: {GroupName}\n",
        "   Description: {Description}",
        .trim = FALSE
      ) %>%
      as.character()

    picked <- picker(
      c(
        glue("We found {length(sgoptions)} matching security groups"),
        "Which security group do you want to use?"
      ),
      sgoptions
    )

    if (picked == 0) {
      cli::cli_alert_danger(c(
        "Found security group {.strong {ip_df$GroupId}} ",
        "w/ access for {.strong {engine}},",
        "{.emph but} not with your IP address {.strong {ip}}"
      ))
      NULL
    } else {
      idtouse <- ip_df[picked, "GroupId"]
      cli::cli_alert_success("Using security group {.strong {idtouse}}")
      idtouse
    }
  }
}


#' List VPC security groups
#' @export
#' @param ... named parameters passed on to [describe_security_groups](
#' https://www.paws-r-sdk.com/docs/ec2_describe_security_groups/)
#' @return (list) list with security groups, see [aws_vpc_security_group()]
#' for details
#' @family security groups
#' @examplesIf interactive() && aws_has_creds()
#' aws_vpc_security_groups()
#' aws_vpc_security_groups(MaxResults = 6)
aws_vpc_security_groups <- function(...) {
  con_ec2()$describe_security_groups(...)
}

#' Get a security group by ID
#' @export
#' @param id (character) The id of the security group. required
#' @inheritParams aws_vpc_security_groups
#' @family security groups
#' @return (list) with fields:
#' - SecurityGroups (list) each security group
#'   - Description
#'   - GroupName
#'   - IpPermissions
#'   - OwnerId
#'   - GroupId
#'   - IpPermissionsEgress
#'   - Tags
#'   - VpcId
#' - NextToken (character) token for paginating
aws_vpc_security_group <- function(id, ...) {
  aws_vpc_security_groups(GroupIds = id, ...)
}

#' Create a security group
#' @export
#' @param name (character) The name of the new secret. required for
#' `*_create` and optional for `*_delete`
#' @param engine (character) The engine to use. default: "mariadb". required.
#' one of: mariadb, mysql, or postgres
#' @param description (character) The description of the secret. optional
#' @param vpc_id (character) a VPC id. optional. if not supplied your default
#' VPC is used. To get your VPCs, see [aws_vpcs()]
#' @param tags (character) The tags to assign to the security group. optional
#' @param ... named parameters passed on to [create_security_group](
#' https://www.paws-r-sdk.com/docs/ec2_create_security_group/)
#' @return (list) with fields:
#' - GroupId (character)
#' - Tags (list)
#' @family security groups
#' @examples \dontrun{
#' # create security group
#' grp_name1 <- random_string("vpcsecgroup")
#' x <- aws_vpc_security_group_create(
#'   name = grp_name1,
#'   description = "Testing security group creation"
#' )
#'
#' grp_name2 <- random_string("vpcsecgroup")
#' aws_vpc_security_group_create(name = grp_name2)
#'
#' grp_name3 <- random_string("vpcsecgroup")
#' aws_vpc_security_group_create(
#'   name = grp_name3,
#'   tags = list(
#'     list(
#'       ResourceType = "security-group",
#'       Tags = list(
#'         list(
#'           Key = "sky",
#'           Value = "blue"
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' # add ingress
#' aws_vpc_security_group_ingress(
#'   id = x$GroupId,
#'   ip_permissions = ip_permissions_generator("mariadb")
#' )
#'
#' # cleanup
#' aws_vpc_security_group_delete(name = grp_name1)
#' aws_vpc_security_group_delete(name = grp_name2)
#' aws_vpc_security_group_delete(name = grp_name3)
#' }
aws_vpc_security_group_create <- function(
  name,
  engine = "mariadb",
  description = NULL,
  vpc_id = NULL,
  tags = NULL,
  ...
) {
  if (is.null(description)) {
    description <- glue("Access to {engine}")
  }
  con_ec2()$create_security_group(
    Description = description,
    GroupName = name,
    VpcId = vpc_id,
    TagSpecifications = tags,
    ...
  )
}

#' @export
#' @rdname aws_vpc_security_group_create
#' @param id (character) The id of the security group. optional. provide `id`
#' or `name`
aws_vpc_security_group_delete <- function(id = NULL, name = NULL, ...) {
  stop_if_not(
    xor(!is.null(id), !is.null(name)),
    "Provide one of id or name, not both"
  )
  con_ec2()$delete_security_group(
    GroupId = id,
    GroupName = name,
    ...
  )
}

engine2port <- function(engine) {
  switch(
    engine,
    mariadb = 3306L,
    mysql = 3306L,
    postgres = 5432L,
    redshift = 5439L,
    stop(glue::glue("{engine} not currently supported"))
  )
}

#' Ip Permissions generator
#'
#' @importFrom ipaddress ip_address is_ipv6
#' @export
#' @param engine (character) one of mariadb, mysql, or postgres
#' @param port (character) port number. port determined from `engine`
#' if `port` not given. default: `NULL`
#' @param description (character) description. if not given, autogenerated
#' depending on value of `engine`
#' @return a list with slots: FromPort, ToPort, IpProtocol, and IpRanges
ip_permissions_generator <- function(engine, port = NULL, description = NULL) {
  protocol <- "tcp"
  port <- engine2port(engine)
  if (is.null(description)) {
    description <- glue("Access for {Sys.info()[['user']]} from sixtyfour")
  }
  ip <- .ip_address()
  result <- list(
    FromPort = port,
    ToPort = port,
    IpProtocol = protocol
  )
  if (is_ipv6(ip_address(ip))) {
    result$Ipv6Ranges <- list(
      list(CidrIpv6 = glue("{ip}/128"), description = description)
    )
  } else {
    result$IpRanges <- list(
      list(CidrIp = glue("{ip}/32"), description = description)
    )
  }
  result
}

#' Get your IP address using <https://ifconfig.me/ip>
#' @return (character) ip address, either ipv4 or ipv6 depending on your
#' machine
#' @keywords internal
.ip_address <- function() {
  res <- curl::curl_fetch_memory("https://ifconfig.me/ip")
  rawToChar(res$content)
}

#' Authorize Security Group Ingress
#' @export
#' @param id (character) security group id. required
#' @param ip_permissions (list) list of persmissions. see link to `paws`
#' docs below or use [ip_permissions_generator()] to generate the
#' list for this parameter
#' @param ... named parameters passed on to
#' [authorize_security_group_ingress](
#' https://www.paws-r-sdk.com/docs/ec2_authorize_security_group_ingress/)
#' @family security groups
#' @return list with slots:
#' - Return (boolean)
#' - SecurityGroupRules (list)
#'   - SecurityGroupRuleId
#'   - GroupId
#'   - GroupOwnerId
#'   - IsEgress
#'   - IpProtocol
#'   - FromPort
#'   - ToPort
#'   - CidrIpv4
#'   - CidrIpv6
#'   - PrefixListId
#'   - ReferencedGroupInfo
#'   - Description
#'   - Tags
aws_vpc_security_group_ingress <- function(id, ip_permissions = NULL, ...) {
  con_ec2()$authorize_security_group_ingress(
    GroupId = id,
    IpPermissions = list(ip_permissions),
    ...
  )
}

#' Modify security group rules
#' @export
#' @param id (character) security group id. required
#' @param rules list of rules to add/modify on the security group `id`.
#' required
#' @param ... named parameters passed on to [modify_security_group_rules](
#' https://www.paws-r-sdk.com/docs/ec2_modify_security_group_rules/)
#' @family security groups
#' @return list. if successful then `list(Return=TRUE)`
#' @examplesIf interactive() && aws_has_creds()
#' # create a security group
#' a_grp_name <- random_string("vpcsecgroup")
#' x <- aws_vpc_security_group_create(name = a_grp_name)
#' x
#'
#' # add an inbound rule
#' my_rule <- aws_vpc_security_group_ingress(
#'   id = x$GroupId,
#'   ip_permissions = ip_permissions_generator("mariadb")
#' )
#' my_rule
#'
#' # modify the rule
#' rule_id <- my_rule$SecurityGroupRules[[1]]$SecurityGroupRuleId
#' fields_to_keep <- c(
#'   "IpProtocol", "FromPort", "ToPort", "CidrIpv4",
#'   "CidrIpv6", "PrefixListId", "Description"
#' )
#' rule_old <- my_rule$SecurityGroupRules[[1]]
#' rule_new <- rule_old[fields_to_keep]
#' rule_new$Description <- "Modified description"
#'
#' aws_vpc_sec_group_rules_mod(
#'   id = x$GroupId,
#'   rules = list(
#'     SecurityGroupRuleId = rule_id,
#'     SecurityGroupRule = rule_new
#'   )
#' )
#'
#' # cleanup
#' aws_vpc_security_group_delete(name = a_grp_name)
aws_vpc_sec_group_rules_mod <- function(id, rules, ...) {
  con_ec2()$modify_security_group_rules(
    GroupId = id,
    SecurityGroupRules = list(rules),
    ...
  )
}
