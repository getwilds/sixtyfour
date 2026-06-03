# Wait for a Redshift cluster to have a certain status

Wait for a Redshift cluster to have a certain status

## Usage

``` r
wait_for_cluster(id, sleep = 2, status_target = "available")
```

## Arguments

- id:

  (character) an RDS instance identifier, or a Redshift cluster
  identifier. required

- sleep:

  (integer/numeric) number of seconds to wait between checks of the
  cluster status (i.e., http requests)

- status_target:

  (character) status to wait for. default: "available"

## Value

nothing, exits if there's an error, or if the while loop completes
