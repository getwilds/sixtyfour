#' internal helper function
#' @param id (function) an RDS instance ID or Redshift cluster ID
#' @param fun (function) a function that takes an ID for an AWS RDS instance
#' or Redshift cluster, and returns a single boolean
#' @param see_fun (character) the function to point users to in the
#' message for database connection
#' @noRd
#' @keywords internal
info <- function(id, fun, see_fun = "") {
  cli::cli_alert_success("Instance is up!")
  cli::cli_alert_info("See `{see_fun}` for connection info")
  cli::cli_alert_info("Instance details:")
  con_info <- fun(id)
  for (i in seq_along(con_info)) {
    if (names(con_info)[i] == "status") next
    cli::cli_alert_info("  {names(con_info)[i]}: {con_info[[i]]}")
  }
}

which_driver <- function(engine) {
  switch(
    engine,
    "mariadb" = {
      check_for_pkg("RMariaDB")
      RMariaDB::MariaDB()
    },
    "mysql" = {
      check_for_pkg("RMariaDB")
      RMariaDB::MariaDB()
    },
    "postgres" = {
      check_for_pkg("RPostgres")
      RPostgres::Postgres()
    },
    stop(glue::glue("{engine} not currently supported"))
  )
}

random_db_id_str <- function(prefix = "-") {
  paste0(prefix, sub("-.+", "", uuid::UUIDgenerate()))
}
