# Fetch secrets

Fetch secrets

## Usage

``` r
ui_fetch_secret(user = NULL, password = NULL, engine = NULL, id = NULL)
```

## Arguments

- user:

  (character) xx

- password:

  (character) password

- engine:

  (character) engine to filter secrets with. if not supplied (`NULL`)
  all secrets are considered

- id:

  (character) the id of the instance. if supplied, we filter the secret
  names by this id

## How the function works

- If user and password are supplied they are returned immediately

- If user and password are not supplied, we fetch all secrets in your
  secrets manager service, and then ask you which one you'd like to use.
  If you choose none of them this function returns NULL for both user
  and password
