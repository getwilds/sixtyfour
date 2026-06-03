# Construct a database secret string or raw version of it

Construct a database secret string or raw version of it

## Usage

``` r
construct_db_secret(
  engine,
  host = "",
  username = "",
  password = "",
  dbname = "",
  port = "",
  as = "string"
)
```

## Arguments

- engine, host, username, password, dbname, port:

  supply parameters to go into either a json string or raw version of
  the json string

- as:

  (character) one of "string" or "raw"

## References

<https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_secret_json_structure.html>
\# nolint
