# Changelog

## sixtyfour 0.2.4

- depend on paws child packages instead of paws itself
  ([\#135](https://github.com/getwilds/sixtyfour/issues/135))
  ([\#136](https://github.com/getwilds/sixtyfour/issues/136))

## sixtyfour 0.2.0

CRAN release: 2025-03-31

### NEW FEATURES

- new functions
  [`aws_configure()`](https://getwilds.org/sixtyfour/reference/aws_configure.md),
  [`without_verbose()`](https://getwilds.org/sixtyfour/reference/without_verbose.md),
  and
  [`with_redacted()`](https://getwilds.org/sixtyfour/reference/with_redacted.md)
  to manage verbosity throughout package and redacting secrets
  ([\#94](https://github.com/getwilds/sixtyfour/issues/94))
  ([\#111](https://github.com/getwilds/sixtyfour/issues/111))
  ([\#113](https://github.com/getwilds/sixtyfour/issues/113))
- new function
  [`aws_has_creds()`](https://getwilds.org/sixtyfour/reference/aws_has_creds.md)
  to check if functional AWS credentials are available - main use case
  is to check if we should run function examples or not
  ([\#81](https://github.com/getwilds/sixtyfour/issues/81))
- new function
  [`six_bucket_upload()`](https://getwilds.org/sixtyfour/reference/six_bucket_upload.md)
  ([\#67](https://github.com/getwilds/sixtyfour/issues/67))
  ([\#79](https://github.com/getwilds/sixtyfour/issues/79))
- new function
  [`aws_policy_update()`](https://getwilds.org/sixtyfour/reference/aws_policy_update.md)
  to update policies
  ([\#87](https://github.com/getwilds/sixtyfour/issues/87))
- new function
  [`aws_vpc_security_group_delete()`](https://getwilds.org/sixtyfour/reference/aws_vpc_security_group_create.md)
  to delete security groups
  ([\#91](https://github.com/getwilds/sixtyfour/issues/91))
- function
  [`figure_out_policy_arn()`](https://getwilds.org/sixtyfour/reference/figure_out_policy_arn.md)
  is now exported
  ([\#87](https://github.com/getwilds/sixtyfour/issues/87))
- new function
  [`six_group_delete()`](https://getwilds.org/sixtyfour/reference/six_group_delete.md)
  to delete groups
  ([\#94](https://github.com/getwilds/sixtyfour/issues/94))
- new vignette “Managing AWS Auth”
  ([\#93](https://github.com/getwilds/sixtyfour/issues/93))
- new vignette “High level six functions”
  ([\#94](https://github.com/getwilds/sixtyfour/issues/94))
- new vignette “Managing buckets in a small group of users”
  ([\#99](https://github.com/getwilds/sixtyfour/issues/99))
- new set of `random_*` helper functions
  ([`random_bucket()`](https://getwilds.org/sixtyfour/reference/random_string.md),
  [`random_role()`](https://getwilds.org/sixtyfour/reference/random_string.md))
  for running examples, tests, vignettes and for users to test
  functionality
  ([\#104](https://github.com/getwilds/sixtyfour/issues/104))
  ([\#117](https://github.com/getwilds/sixtyfour/issues/117))

### MINOR IMPROVEMENTS

- add `filter` parameter to
  [`aws_billing()`](https://getwilds.org/sixtyfour/reference/aws_billing.md)
  ([\#72](https://github.com/getwilds/sixtyfour/issues/72))
  ([\#74](https://github.com/getwilds/sixtyfour/issues/74)) thanks
  [@ateucher](https://github.com/ateucher)
- add package namespace prefix to lazily loaded data object
  `service_map`
  ([\#83](https://github.com/getwilds/sixtyfour/issues/83))
  ([\#84](https://github.com/getwilds/sixtyfour/issues/84)) thanks
  [@ateucher](https://github.com/ateucher)
- use `webmockr` to stub
  [`aws_billing()`](https://getwilds.org/sixtyfour/reference/aws_billing.md)
  in tests ([\#76](https://github.com/getwilds/sixtyfour/issues/76))
  ([\#77](https://github.com/getwilds/sixtyfour/issues/77))
- rework all examples: to be fully reproducible, to clean up after
  themselves, to only run if credentials are available, to obfuscate
  senstive outputs in examples
  ([\#15](https://github.com/getwilds/sixtyfour/issues/15))
  ([\#80](https://github.com/getwilds/sixtyfour/issues/80))
  ([\#81](https://github.com/getwilds/sixtyfour/issues/81)) (82)
  ([\#85](https://github.com/getwilds/sixtyfour/issues/85))
  ([\#86](https://github.com/getwilds/sixtyfour/issues/86))
  ([\#87](https://github.com/getwilds/sixtyfour/issues/87))
  ([\#89](https://github.com/getwilds/sixtyfour/issues/89))
  ([\#90](https://github.com/getwilds/sixtyfour/issues/90))
  ([\#91](https://github.com/getwilds/sixtyfour/issues/91))
  ([\#92](https://github.com/getwilds/sixtyfour/issues/92))
  ([\#103](https://github.com/getwilds/sixtyfour/issues/103))
  ([\#105](https://github.com/getwilds/sixtyfour/issues/105))
- remove dependency on the `snakecase` package
  ([\#100](https://github.com/getwilds/sixtyfour/issues/100))
  ([\#102](https://github.com/getwilds/sixtyfour/issues/102))
- bump minimum dependency versions for packages `paws` and `paws.common`
  ([\#114](https://github.com/getwilds/sixtyfour/issues/114))
  ([\#120](https://github.com/getwilds/sixtyfour/issues/120))
- “Getting Started” vignettes fixes and improvements
  ([\#109](https://github.com/getwilds/sixtyfour/issues/109))
  ([\#112](https://github.com/getwilds/sixtyfour/issues/112))
- user [`set.seed()`](https://rdrr.io/r/base/Random.html) in all
  vignettes that use `random_*` functions so that inconsequential
  changes aren’t committed each time
  ([\#116](https://github.com/getwilds/sixtyfour/issues/116))
- function name change: `aws_vpc_sec_group_rules()` to
  [`aws_vpc_sec_group_rules_mod()`](https://getwilds.org/sixtyfour/reference/aws_vpc_sec_group_rules_mod.md)

## sixtyfour 0.1.0

- First release
