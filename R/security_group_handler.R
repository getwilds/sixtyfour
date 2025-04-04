security_group_handler <- function(ids, engine) {
  if (!is.null(ids)) {
    return(ids)
  }
  port <- engine2port(engine) # nolint
  ip <- .ip_address() # nolint
  sgs_df <- security_groups_data()

  port_df <- filter_security_groups_by_port(sgs_df, port)
  if (!NROW(port_df)) {
    return(handle_no_matching_port_groups(engine))
  }

  ip_df <- filter_security_groups_by_ip(port_df, ip)
  if (!NROW(ip_df)) {
    return(handle_no_matching_ip_groups(ip_df, engine, ip))
  }

  select_security_group(ip_df, engine, ip)
}

make_pick_options <- function(df, ..., .envir = parent.frame()) {
  x <- glue::glue_data(df, ..., .trim = FALSE, .envir = .envir)
  as.character(x)
}

security_groups_data <- function() {
  sgs <- aws_vpc_security_groups()
  sgsdf <- jsonlite::fromJSON(
    jsonlite::toJSON(sgs$SecurityGroups, auto_unbox = TRUE)
  )
  if (is_empty(sgsdf)) sgsdf <- tibble(IpPermissions = list())
  sgsdf
}

#' @autoglobal
filter_security_groups_by_port <- function(df, port) {
  dplyr::filter(
    df,
    map_lgl(
      IpPermissions,
      ~ ifelse(rlang::is_empty(.), FALSE, .$ToPort == port)
    )
  )
}
#' @importFrom purrr map_lgl pluck
#' @autoglobal
filter_security_groups_by_ip <- function(df, ip) {
  dplyr::filter(
    df,
    map_lgl(IpPermissions, ~ any(grepl(ip, pluck(.$IpRanges, 1, "CidrIp"))))
  )
}

handle_no_matching_port_groups <- function(engine) {
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
    NULL
  } else {
    cli::cli_alert_success("Using security group {.strong {trysg}}")
    trysg
  }
}

handle_no_matching_ip_groups <- function(df, engine, ip) {
  cli::cli_alert_danger(c(
    "Found security groups w/ access for {.strong {engine}}, ",
    "{.emph but} not with your IP address {.strong {ip}}"
  ))
  cli::cli_alert_info("Which security group do you want to modify?")

  # Let user pick a security group
  pick_sg_options <- make_pick_options(
    df,
    "Security Group: {GroupId}\n",
    "   Group Name: {GroupName}\n",
    "   Description: {Description}"
  )
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
  }

  picked_id <- df[picked, "GroupId"]
  add_ip_to_security_group(picked_id, engine, ip)
}

add_ip_to_security_group <- function(security_group_id, engine, ip) {
  cli::cli_alert_info(c(
    "Adding your IP address {.strong {ip}} to security ",
    "group {.strong {security_group_id}}"
  ))

  try_ingress <- tryCatch(
    {
      aws_vpc_security_group_ingress(
        id = security_group_id,
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
    NULL
  } else {
    cli::cli_alert_success("Using security group {.strong {security_group_id}}")
    security_group_id
  }
}

select_security_group <- function(df, engine, ip) {
  if (NROW(df) == 1) {
    cli::cli_alert_success(c(
      "Found security group {.strong {df$GroupId}} ",
      "w/ access for {.strong {engine}} and your IP address {.strong {ip}}"
    ))
    return(df$GroupId)
  }

  sgoptions <- make_pick_options(
    df,
    "Security Group: {GroupId}\n",
    "   Group Name: {GroupName}\n",
    "   Description: {Description}"
  )
  picked <- picker(
    c(
      glue("We found {length(sgoptions)} matching security groups"),
      "Which security group do you want to use?"
    ),
    sgoptions
  )

  if (picked == 0) {
    cli::cli_alert_danger(c(
      "Found security group {.strong {df$GroupId}} ",
      "w/ access for {.strong {engine}},",
      "{.emph but} not with your IP address {.strong {ip}}"
    ))
    NULL
  } else {
    idtouse <- df[picked, "GroupId"]
    cli::cli_alert_success("Using security group {.strong {idtouse}}")
    idtouse
  }
}
