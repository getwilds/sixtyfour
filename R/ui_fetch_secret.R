#' Fetch secrets
#'
#' @param user (character) xx
#' @param password (character) password
#' @param engine (character) engine to filter secrets with. if not supplied
#' (`NULL`) all secrets are considered
#' @section How the function works:
#' - If user and password are supplied they are returned immediately
#' - If user and password are not supplied, we fetch all secrets in
#' your secrets manager service, and then ask you which one you'd like
#' to use. If you choose none of them this function returns NULL for
#' both user and password
#' @keywords internal
#' @examples \dontrun{
#' # user,pwd supplied, return them right away at top of fxn
#' ui_fetch_secret(engine = "mariadb", user = "jane", password = "apple")
#'
#' # user,pwd null
#' ui_fetch_secret(engine = "redshift")
#' ui_fetch_secret(engine = "mariadb")
#' }
ui_fetch_secret <- function(user = NULL, password = NULL, engine = NULL) {
  # if user and password supplied return them
  if (!is.null(user) && !is.null(password)) {
    return(list(user = user, password = password))
  }

  # get all secrets data
  secrets <- aws_secrets_all()

  # organize secrets
  new_secrets <- list()
  for (i in seq_along(secrets)) {
    new_secrets[[i]] <- c(
      list(name = secrets[[i]]$Name),
      jsonlite::fromJSON(secrets[[i]]$SecretString)
    )
  }
  new_secrets_df <-
    Filter(function(x) length(x$host) > 0, new_secrets) %>%
    bind_rows()
  if (!is.null(engine)) {
    new_secrets_df <- filter(new_secrets_df, engine == !!engine)
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
  picked <- picker(c(
    "No credentials were supplied",
    glue("We found {length(dboptions)} in your AWS secrets manager"),
    "Which set of database credentials do you want to use?"
  ), dboptions)

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
