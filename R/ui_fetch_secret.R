#' Fetch secrets
#'
#' @param user (character) xx
#' @param password (character) password
#' @param engine (character) engine to filter secrets with. if not supplied
#' (`NULL`) all secrets are considered
#' @param id (character) the id of the instance. if supplied, we filter
#' the secret names by this id
#' @autoglobal
#' @section How the function works:
#' - If user and password are supplied they are returned immediately
#' - If user and password are not supplied, we fetch all secrets in
#' your secrets manager service, and then ask you which one you'd like
#' to use. If you choose none of them this function returns NULL for
#' both user and password
#' @keywords internal
ui_fetch_secret <- function(
  user = NULL,
  password = NULL,
  engine = NULL,
  id = NULL
) {
  # if user and password supplied return them
  if (!is.null(user) && !is.null(password)) {
    return(list(user = user, password = password))
  }

  new_secrets_df <- aws_secrets_all()

  if (!is.null(engine)) {
    new_secrets_df <- filter(new_secrets_df, engine == !!engine)
  }
  if (!is.null(id)) {
    id_match <- filter(new_secrets_df, grepl(glue("^{id}"), name))
    if (NROW(id_match) > 0) {
      new_secrets_df <- bind_rows(
        id_match,
        filter(new_secrets_df, !grepl(glue("^{id}"), name))
      )
    }
  }
  if (NROW(new_secrets_df) == 0) {
    stop("No secrets found", call. = FALSE)
  }
  dboptions <-
    new_secrets_df %>%
    glue::glue_data(
      "Secret name: {name}\n",
      "   Engine: {engine}\n",
      "   Host: {host}",
      .trim = FALSE
    ) %>%
    as.character()

  # if any db secrets found in their secrets manager, prompt user
  picked <- picker(
    c(
      "No credentials were supplied",
      glue("We found {length(dboptions)} in your AWS secrets manager"),
      "Which set of database credentials do you want to use?"
    ),
    dboptions
  )

  if (picked == 0) {
    user <- password <- NULL
  } else {
    selected_secret <- new_secrets_df[picked, ]
    user <- selected_secret$username
    password <- selected_secret$password
  }

  list(user = user, password = password)
}

picker <- function(msg, choices, .envir = parent.frame()) {
  cli::cli_inform(msg, .envir = .envir)
  utils::menu(choices)
}
