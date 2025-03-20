# sixtyfour 0.2.0

## NEW FEATURES

* new functions `aws_configure()`, `without_verbose()`, and `with_redacted()` to manage verbosity throughout package and redacting secrets (#94) (#111) (#113)
* new function `aws_has_creds()` to check if functional AWS credentials are available - main use case is to check if we should run function examples or not (#81)
* new function `six_bucket_upload()` (#67) (#79)
* new function `aws_policy_update()` to update policies (#87)
* new function `aws_vpc_security_group_delete()` to delete security groups (#91)
* function `figure_out_policy_arn()` is now exported (#87)
* new function `six_group_delete()` to delete groups (#94)
* new vignette "Managing AWS Auth" (#93)
* new vignette "High level six functions" (#94)
* new vignette "Managing buckets in a small group of users" (#99)
* new set of `random_*` helper functions (`random_bucket()`, `random_role()`) for running examples, tests, vignettes and for users to test functionality (#104) (#117)

## MINOR IMPROVEMENTS

* add `filter` parameter to `aws_billing()` (#72) (#74) thanks @ateucher
* add package namespace prefix to lazily loaded data object `service_map` (#83) (#84) thanks @ateucher
* use `webmockr` to stub `aws_billing()` in tests (#76) (#77)
* rework all examples: to be fully reproducible, to clean up after themselves, to only run if credentials are available, to obfuscate senstive outputs in  examples (#15) (#80) (#81) (82) (#85) (#86) (#87) (#89) (#90) (#91) (#92) (#103) (#105)
* remove dependency on the `snakecase` package (#100) (#102)
* bump minimum dependency versions for packages `paws` and `paws.common` (#114) (#120)
* "Getting Started" vignettes fixes and improvements (#109) (#112)
* user `set.seed()` in all vignettes that use `random_*` functions so that inconsequential changes aren't committed each time (#116)
* function name change: `aws_vpc_sec_group_rules()` to `aws_vpc_sec_group_rules_mod()`


# sixtyfour 0.1.0

* First release
