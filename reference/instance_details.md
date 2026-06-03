# Get information for all RDS instances

Get information for all RDS instances

## Usage

``` r
instance_details()
```

## Value

a list of RDS instance details, see link below for format, with slots:

- Marker (for pagination)

- DBInstances (each instance; empty list if no instances)

## References

<https://www.paws-r-sdk.com/docs/rds_describe_db_instances/>
