# Preset group policies

Preset group policies

## Usage

``` r
group_policies(group)
```

## Arguments

- group:

  (character)

## Value

character vector of policy names

## Admin group policies

- AdministratorAccess

- Billing

- CostOptimizationHubAdminAccess

- AWSBillingReadOnlyAccess

- AWSCostAndUsageReportAutomationPolicy

## User group policies

- AmazonRDSReadOnlyAccess

- AmazonRedshiftReadOnlyAccess

- AmazonS3ReadOnlyAccess

- AWSBillingReadOnlyAccess

- IAMReadOnlyAccess

## Examples

``` r
group_policies("admin")
#> [1] "AdministratorAccess"                  
#> [2] "Billing"                              
#> [3] "CostOptimizationHubAdminAccess"       
#> [4] "AWSBillingReadOnlyAccess"             
#> [5] "AWSCostAndUsageReportAutomationPolicy"
group_policies("users")
#> [1] "AmazonRDSReadOnlyAccess"      "AmazonRedshiftReadOnlyAccess"
#> [3] "AmazonS3ReadOnlyAccess"       "AWSBillingReadOnlyAccess"    
#> [5] "IAMReadOnlyAccess"           
```
