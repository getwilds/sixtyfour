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
aws_vpc_sg_with_ingresss <- function(engine) {
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

# FIXME: clean this mess up!
#' @importFrom purrr map_lgl pluck
#' @autoglobal
#' @keywords internal
security_group_handler <- function(ids, engine) {
  if (!is.null(ids)) {
    return(ids)
  }
  port <- engine2port(engine) # nolint
  ip <- ip_address() # nolint
  sgs <- aws_vpc_security_groups()
  sgsdf <- jsonlite::fromJSON(
    jsonlite::toJSON(sgs$SecurityGroups, auto_unbox = TRUE)
  )

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
    trysg <- tryCatch(aws_vpc_sg_with_ingresss(engine),
      error = function(e) e
    )
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

    picked <- picker(c(
      glue("We found {length(pick_sg_options)} security groups"),
      "Which security group do you want to use?"
    ), pick_sg_options)

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
    return(ip_df$GroupId)
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

    picked <- picker(c(
      glue("We found {length(sgoptions)} matching security groups"),
      "Which security group do you want to use?"
    ), sgoptions)

    if (picked == 0) {
      cli::cli_alert_danger(c(
        "Found security group {.strong {ip_df$GroupId}} ",
        "w/ access for {.strong {engine}},",
        "{.emph but} not with your IP address {.strong {ip}}"
      ))
      return(NULL)
    } else {
      idtouse <- ip_df[picked, "GroupId"]
      cli::cli_alert_success("Using security group {.strong {idtouse}}")
      return(idtouse)
    }
  }
}


#' List VPC security groups
#' @export
#' @param ... named parameters passed on to [describe_security_groups](
#' https://www.paws-r-sdk.com/docs/ec2_describe_security_groups/)
#' @return (list) list with security groups
#' @family security groups
#' @examplesIf interactive()
#' aws_vpc_security_groups()
#' aws_vpc_security_groups(MaxResults = 6)
aws_vpc_security_groups <- function(...) {
  aws_ec2_client()
  env64$ec2$describe_security_groups(...)
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
  aws_ec2_client()
  aws_vpc_security_groups(GroupIds = id, ...)
}

#' Create a security group
#' @export
#' @param name (character) The name of the new secret. required
#' @param engine (character) The engine to use. default: "mariadb". required.
#' one of: mariadb, mysql, or postgres
#' @param description (character) The description of the secret. optional
#' @param vpc_id (character) a VPC id. optional. if not supplied your default
#' VPC is used. To get your VPCs, see [aws_vpcs()]
#' @param tags (character) The tags to assign to the security group. optional
#' @param ... named parameters passed on to [create_secret](
#' https://www.paws-r-sdk.com/docs/secretsmanager_create_secret/)
#' @return (list) with fields:
#' - GroupId (character)
#' - Tags (list)
#' @family security groups
#' @examples \dontrun{
#' # create security group
#' x <- aws_vpc_security_group_create(
#'   name = "testing1",
#'   description = "Testing security group creation"
#' )
#' # add ingress
#' aws_vpc_security_group_ingress(
#'   id = x$GroupId,
#'   ip_permissions = ip_permissions_generator("mariadb")
#' )
#' }
aws_vpc_security_group_create <- function(
    name, engine, description = NULL,
    vpc_id = NULL, tags = NULL, ...) {
  aws_ec2_client()
  if (is.null(description)) {
    description <- glue("Access to {engine}")
  }
  env64$ec2$create_security_group(
    Description = description,
    GroupName = name,
    VpcId = vpc_id,
    TagSpecifications = tags,
    ...
  )
}

engine2port <- function(engine) {
  switch(engine,
    mariadb = 3306L,
    mysql = 3306L,
    postgres = 5432L,
    redshift = 5439L,
    stop(glue::glue("{engine} not currently supported"))
  )
}

#' Ip Permissions generator
#'
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
  list(
    FromPort = port,
    ToPort = port,
    IpProtocol = protocol,
    IpRanges = list(
      list(
        CidrIp = glue("{ip_address()}/32"),
        Description = description
      )
    )
  )
}

#' Get your IP address using <https://ifconfig.me/ip>
#' @return (character) ip address
#' @keywords internal
ip_address <- function() {
  res <- curl::curl_fetch_memory("https://ifconfig.me/ip")
  rawToChar(res$content)
}

#' Authorize Security Group Ingress
#' @export
#' @param id (character) security group id
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
aws_vpc_security_group_ingress <- function(
    id = NULL,
    ip_permissions = NULL, ...) {
  aws_ec2_client()
  env64$ec2$authorize_security_group_ingress(
    GroupId = id,
    IpPermissions = list(ip_permissions),
    ...
  )
}

#' Modify security group rules
#' @export
#' @param id (character) security group id
#' @param rules list of rules to add/modify on the security group `id`
#' @param ... named parameters passed on to [modify_security_group_rules](
#' https://www.paws-r-sdk.com/docs/ec2_modify_security_group_rules/)
#' @family security groups
#' @examplesIf interactive()
#' aws_vpc_security_group_modify_rules(
#'   id = "someid",
#'   rules = list(
#'     SecurityGroupRuleId = "sgr-07de36a0521f39c8b",
#'     SecurityGroupRule = list(
#'       IpProtocol = "tcp",
#'       FromPort = 22,
#'       ToPort = 22,
#'       CidrIpv4 = "3.3.3.3/32",
#'       Description = "added ssh port"
#'     )
#'   )
#' )
aws_vpc_sec_group_rules <- function(id, rules, ...) {
  aws_ec2_client()
  env64$ec2$modify_security_group_rules(
    GroupId = id,
    SecurityGroupRules = list(rules),
    ...
  )
}
