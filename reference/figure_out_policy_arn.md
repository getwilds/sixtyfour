# Figure out policy Arn from a name

Figure out policy Arn from a name

## Usage

``` r
figure_out_policy_arn(name)
```

## Arguments

- name:

  (character) a policy name. required.

## Value

`NULL` when not found; otherwise an ARN string

## Examples

``` r
if (FALSE) { # aws_has_creds()
# aws managed
figure_out_policy_arn("AmazonS3ReadOnlyAccess")
# aws managed, job function
figure_out_policy_arn("Billing")
figure_out_policy_arn("DataScientist")
# doesn't exist
figure_out_policy_arn("DoesNotExist")
}
```
