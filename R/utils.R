tibble_transpose <- function(a_list) {
  df <- lapply(a_list, function(x) ifelse(length(x) == 0, NA_character_, x)) %>%
    as_tibble(.)
  as_tibble(cbind(nms = names(df), t(df)))
}

paws_handlr <- function(...) {
  tryCatch(..., error = function(e) e)
}

# TODO: maybe use a custom switch to have more useful error messages?
# status_swap <- function(err) {
#   dplyr::case_match(
#     err$status_code,
#     404 = "Not found",
#     403 = "Not found",
#     .default = err$message
#   )
# }
