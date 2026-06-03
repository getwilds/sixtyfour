# Configure sixtyfour settings

Configure sixtyfour settings

## Usage

``` r
aws_configure(redacted = FALSE, redact_str = "*****", verbose = TRUE)
```

## Arguments

- redacted:

  (logical) Redact secrets? Default: `FALSE`. If `TRUE`, secret values
  are redacted (replaced with `redact_str`) in certain messages and
  output from functions. See *What is Redacted* below.

- redact_str:

  (character) String to use to replace redacted values. Default:
  `"*****"`

- verbose:

  (logical) Print verbose output? Default: `TRUE`. Applies only to
  [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html),
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html),
  and
  [`cli::cli_alert_success()`](https://cli.r-lib.org/reference/cli_alert.html)
  functions that are used throughout this package. There's still a few
  places where `verbose` may not be respected.

## Value

S3 class `aws_settings`

## What is Redacted

What's redacted is currently hard-coded in the package. There's only
certain functions and certain elements in the output of those functions
that are redacted. The following is what's redacted with
`aws_configure(redacted = TRUE)` or
[`with_redacted()`](https://getwilds.org/sixtyfour/reference/with_redacted.md):

- `aws_whoami()`: AWS Account ID via
  [`account_id()`](https://getwilds.org/sixtyfour/reference/account_id.md)

- [`six_user_creds()`](https://getwilds.org/sixtyfour/reference/six_user_creds.md):
  Access Key ID

- groups functions:

  - functions:
    [`aws_groups()`](https://getwilds.org/sixtyfour/reference/aws_groups.md),
    [`aws_group()`](https://getwilds.org/sixtyfour/reference/aws_group.md),
    [`aws_group_create()`](https://getwilds.org/sixtyfour/reference/aws_group_create.md)

  - attribute: `Arn` (includes AWS Account ID)

- roles functions:

  - functions:
    [`aws_roles()`](https://getwilds.org/sixtyfour/reference/aws_roles.md),
    [`aws_role()`](https://getwilds.org/sixtyfour/reference/aws_role.md),
    [`aws_role_create()`](https://getwilds.org/sixtyfour/reference/aws_role_create.md)

  - attribute: `Arn` (includes AWS Account ID)

- user functions:

  - functions:
    [`aws_users()`](https://getwilds.org/sixtyfour/reference/aws_users.md),
    [`aws_user()`](https://getwilds.org/sixtyfour/reference/aws_user.md),
    [`aws_user_create()`](https://getwilds.org/sixtyfour/reference/aws_user_create.md),
    [`aws_user_add_to_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md),
    [`aws_user_remove_from_group()`](https://getwilds.org/sixtyfour/reference/aws_user_add_to_group.md)

  - attribute: `Arn` (includes AWS Account ID)

- [`aws_user_access_key_delete()`](https://getwilds.org/sixtyfour/reference/aws_user_access_key_delete.md):
  Access Key ID
