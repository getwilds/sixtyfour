# wait fxn generator
wait_until <- function(fun, message) {
  function(id, sleep = 2, status_target = "available") {
    cli::cli_alert_info("Waiting for instance status: {.emph {status_target}}")
    options(cli.spinner = "simpleDots")
    on.exit(options(cli.spinner = NULL), add = TRUE)
    cli::cli_progress_bar(format = "{cli::pb_spin} {message}") # nolint
    is_not_available <- TRUE
    while (is_not_available) {
      status <- fun(id)
      if (status == "not found") break
      cli::cli_progress_update()
      Sys.sleep(sleep)
      if (status == status_target) {
        is_not_available <- FALSE
      }
    }
  }
}

#' Wait for a Redshift cluster to have a certain status
#'
#' @importFrom cli cli_progress_bar cli_progress_update pb_spin
#' @inheritParams aws_db_redshift_create
#' @param fun (function) a function to check status of something;
#' must return a single boolean, e.g., `aws_db_cluster_status` or
#' `aws_db_instance_status`
#' @param sleep (integer/numeric) number of seconds to wait between
#' checks of the cluster status (i.e., http requests)
#' @param status_target (character) status to wait for. default: "available"
#' @return nothing, exits if there's an error, or if the while
#' loop completes
#' @keywords internal
#' @examples \dontrun{
#' wait_for_cluster(id = "scotts-test-cluster-456")
#' }
wait_for_cluster <- wait_until(
  aws_db_cluster_status,
  "Redshift cluster initializing"
)

#' Wait for an RDS instance to have a certain status
#'
#' @inheritParams wait_for_cluster
#' @return nothing, exits if there's an error, or if the while
#' loop completes
#' @keywords internal
#' @examples \dontrun{
#' wait_for_instance(id = "scotts-test-cluster-456")
#' }
wait_for_instance <- wait_until(
  aws_db_instance_status,
  "RDS instance initializing"
)
