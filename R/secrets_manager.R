#' List secrets
#' @export
#' @return (list) list with secrets
#' @examples \dontrun{
#' aws_secrets_list()
#' }
aws_secrets_list <- function() {
  env64$secretsmanager$list_secrets()
}

#' Get all secret values
#' @export
#' @return (list) list with secrets
#' @examples \dontrun{
#' aws_secrets_list()
#' }
aws_secrets_all <- function() {
  env64$secretsmanager$list_secrets() %>%
    .$SecretList %>%
    purrr::map(function(x) aws_secrets_get(x$Name))
}

check_secret <- function(secret) {
  if (!inherits(secret, c("raw", "character"))) {
    stop("`secret` must be of class character or raw", call. = FALSE)
  }
}

#' Get a random password
#'
#' @export
#' @param ... named parameters passed on to `get_random_password`
#' <https://www.paws-r-sdk.com/docs/secretsmanager_get_random_password/>
#' @examples \dontrun{
#' aws_secrets_pwd()
#' aws_secrets_pwd(ExcludeNumbers = TRUE)
#' }
aws_secrets_pwd <- function(...) {
  env64$secretsmanager$get_random_password(
    PasswordLength = 40L,
    ExcludePunctuation = TRUE,
    ...
  )$RandomPassword
}

#' Create a secret
#'
#' This function does not create your database username and/or password.
#' Instead, it creates a "secret", which is typically a combination
#' of credentials (username + password + other metadata)
#'
#' @export
#' @param name (character) The name of the new secret. required
#' @param secret (character/raw) The text or raw data to encrypt and store
#' in this new version of the secret. AWS recommends for text to use a JSON
#' structure of key/value pairs for your secret value (see examples below).
#' required
#' @param description (character) The description of the secret. optional
#' @param ... further named parameters passed on to `create_secret`
#' <https://www.paws-r-sdk.com/docs/secretsmanager_create_secret/>
#' @return (list) with fields:
#' - ARN
#' - Name
#' - VersionId
#' - ReplicationStatus
#' @details Note that we autogenerate a random UUID to pass to the
#' `ClientRequestToken` parameter of the `paws` function `create_secret`
#' used internally in this function.
#'
#' This function creates a new secret. See [aws_secrets_update()] to
#' update an existing secret. This function fails if you call it with
#' an existing secret with the same name or ARN
#' @examples \dontrun{
#' # Text secret
#' x <- aws_secrets_create(
#'   name = "MyTestDatabaseSecret",
#'   secret = '{"username":"david","password":"EXAMPLE-PASSWORD"}',
#'   description = "My test database secret as a string"
#' )
#'
#' # Raw secret
#' x <- aws_secrets_create(
#'   name = "MyRawDatabaseSecret",
#'   secret = charToRaw('{"username":"david","password":"EXAMPLE-PASSWORD"}'),
#'   description = "My test database secret as raw"
#' )
#' }
aws_secrets_create <- function(name, secret, description = NULL, ...) {
  check_secret(secret)
  secret_str <- secret_raw <- NULL
  if (rlang::is_raw(secret)) secret_raw <- secret
  if (rlang::is_character(secret)) secret_str <- secret
  env64$secretsmanager$create_secret(
    Name = name,
    ClientRequestToken = uuid::UUIDgenerate(),
    Description = description,
    SecretBinary = secret_raw,
    SecretString = secret_str, ...
  )
}

#' Update a secret
#' @export
#' @inheritParams aws_secrets_create
#' @param id (character) The name or ARN of the secret. required
#' @param ... further named parameters passed on to `put_secret_value`
#' <https://www.paws-r-sdk.com/docs/secretsmanager_put_secret_value/>
#' @return (list) with fields:
#' - ARN
#' - Name
#' - VersionId
#' - VersionStages
#' @autoglobal
#' @details Note that we autogenerate a random UUID to pass to the
#' `ClientRequestToken` parameter of the `paws` function used internally
#' @examples \dontrun{
#' # Create a secret
#' aws_secrets_create(
#'   name = "TheSecret",
#'   secret = '{"username":"jane","password":"cat"}',
#'   description = "A string"
#' )
#'
#' aws_secrets_get("TheSecret")
#'
#' # Update the secret
#' aws_secrets_update(
#'   id = "TheSecret",
#'   secret = '{"username":"jane","password":"kitten"}'
#' )
#'
#' aws_secrets_get("TheSecret")
#' }
aws_secrets_update <- function(id, secret, ...) {
  check_secret(secret)
  secret_str <- secret_raw <- NULL
  if (rlang::is_raw(secret)) secret_raw <- secret
  if (rlang::is_character(secret)) secret_str <- secret
  env64$secretsmanager$put_secret_value(
    SecretId = id,
    ClientRequestToken = uuid::UUIDgenerate(),
    SecretBinary = secret_raw,
    SecretString = secret_str, ...
  )
}

#' Get a secret
#' @export
#' @inheritParams aws_secrets_update
#' @param ... further named parameters passed on to `get_secret_value`
#' <https://www.paws-r-sdk.com/docs/secretsmanager_get_secret_value/>
#' @return (list) with fields:
#' - ARN
#' - Name
#' - VersionId
#' - SecretBinary
#' - SecretString
#' - VersionStages
#' - CreatedDate
#' @examples \dontrun{
#' # Does exist
#' aws_secrets_get(id = "MyTestDatabaseSecret")
#'
#' # Does not exist
#' # aws_secrets_get(id = "DoesntExist")
#' #> Error: ResourceNotFoundException (HTTP 400). Secrets Manager
#' #>   can't find the specified secret.
#' }
aws_secrets_get <- function(id, ...) {
  env64$secretsmanager$get_secret_value(SecretId = id, ...)
}

#' Delete a secret
#' @export
#' @inheritParams aws_secrets_update
#' @param ... further named parameters passed on to `delete_secret`
#' <https://www.paws-r-sdk.com/docs/secretsmanager_delete_secret/>
#' @return (list) with fields:
#' - ARN
#' - Name
#' - DeletionDate
#' @examples \dontrun{
#' # Does exist
#' aws_secrets_delete(id = "MyTestDatabaseSecret")
#'
#' # Does not exist
#' # aws_secrets_get(id = "DoesntExist")
#' #> Error: ResourceNotFoundException (HTTP 400). Secrets Manager
#' #>   can't find the specified secret.
#' }
aws_secrets_delete <- function(id, ...) {
  env64$secretsmanager$delete_secret(SecretId = id, ...)
}

#' Rotate a secret
#' @export
#' @inheritParams aws_secrets_update
#' @inherit aws_secrets_update details
#' @param lambda_arn (character) The ARN of the Lambda rotation function.
#' Only supply for secrets that use a Lambda rotation function to rotate
#' @param rules (list) asdfadf
#' @param immediately (logical) whether to rotate the secret immediately or not.
#' default: `TRUE`
#' @references <https://www.paws-r-sdk.com/docs/secretsmanager_rotate_secret/>
#' @autoglobal
#' @return (list) with fields:
#' - ARN
#' - Name
#' - VersionId
#' @examples \dontrun{
#' aws_secrets_rotate(id = "MyTestDatabaseSecret")
#' aws_secrets_rotate(id = "MyTestDatabaseSecret", rules = list(
#'     Duration = "2h",
#'     ScheduleExpression = "cron(0 16 1,15 * ? *)"
#'   )
#' }
aws_secrets_rotate <- function(id, lambda_arn = NULL, rules = NULL, immediately = TRUE) {
  env64$secretsmanager$rotate_secret(
    SecretId = id,
    ClientRequestToken = uuid::UUIDgenerate(),
    RotationLambdaARN = secret_raw,
    RotationRules = secret_str,
    RotateImmediately = immediately
  )
}

#' Construct a database secret string or raw version of it
#'
#' @param engine,host,username,password,dbname,port supply parameters to
#' go into either a json string or raw version of the json string
#' @param as (character) one of "string" or "raw"
#' @keywords internal
#' @references <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_secret_json_structure.html>
#' @examples \dontrun{
#' construct_db_secret("redshift", dbname = "hello", port = 5439)
#' construct_db_secret("mariadb", dbname = "world", port = 3306)
#' construct_db_secret("postgresql", dbname = "bears", port = 5432, as = "raw")
#' }
construct_db_secret <- function(
    engine, host = "", username = "",
    password = "", dbname = "", port = "", as = "string") {


  dat <- list(
    "engine" = engine,
    "host" = host,
    "username" = username,
    "password" = password,
    "dbname" = dbname,
    "port" = port
  )
  json_dat <- jsonlite::toJSON(dat, auto_unbox = TRUE)
  switch(as,
    string = as.character(json_dat),
    raw = charToRaw(json_dat),
    stop("`as` must be one of 'string' or 'raw'", call. = FALSE)
  )
}
