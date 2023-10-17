tibble_transpose <- function(a_list) {
  df <- lapply(a_list, function(x) ifelse(length(x) == 0, NA_character_, x)) %>%
    as_tibble(.)
  as_tibble(cbind(nms = names(df), t(df)))
}

paws_handlr <- function(...) {
  tryCatch(..., error = function(e) e)
}

# Check for an env var; stop with message if not found
env_var <- function(env_name) {
  x <- Sys.getenv(env_name, "")
  stop_msg <- sprintf("Environment variable '%s' not found", env_name)
  if (identical(x, "")) stop(stop_msg)
  return(x)
}
