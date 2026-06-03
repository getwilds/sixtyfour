# Get a random string, bucket name, user name or role name

Get a random string, bucket name, user name or role name

## Usage

``` r
random_string(prefix, size = 8)

random_bucket(prefix = "bucket-", size = 16)

random_user()

random_role()
```

## Arguments

- prefix:

  (character) any string. required.

- size:

  (character) length of the random part (not including `prefix`)

## Value

- `random_string`: (character) a string with `prefix` at beginning

- `random_bucket`: (character) a bucket name prefixed with `prefix`
  (default: "bucket-")

- `random_user`/`random_role`: (character) a user or role name with a
  random adjective plus a random noun combined into one string,
  shortened to no longer than 16 characters, if longer than 16

## Examples

``` r
random_string("group-")
#> group-kzeulbfd
replicate(10, random_string("group-"))
#>  [1] "group-pbcvkgfi" "group-vjubqtaf" "group-bsnaoreh" "group-alspkcbj"
#>  [5] "group-cfdjagys" "group-ybhawgli" "group-jbfglnew" "group-slqexmfa"
#>  [9] "group-amrekzdg" "group-wxbhisvf"
random_bucket()
#> bucket-mzucyrtkdbiqnwlv
replicate(10, random_bucket())
#>  [1] "bucket-qjlmpgdacusfkxzn" "bucket-jhvqltorxzcdmfbi"
#>  [3] "bucket-esrzjldyqvhginxm" "bucket-syzoqwekuhlgcpvn"
#>  [5] "bucket-phlbgrawiktvxqdm" "bucket-ayjktorbxecdnsql"
#>  [7] "bucket-fsqhvwncroxlpkug" "bucket-zuhoepsdambgqcjx"
#>  [9] "bucket-cqaipjukrtshbdeg" "bucket-mtydsupvnrlwiefg"
random_user()
#> [1] "CommutingMouthpi"
replicate(10, random_user())
#>  [1] "AccountantIceber" "PipedDucking"     "JoiningAllegianc" "KnowingPointer"  
#>  [5] "MolecularChasm"   "AcuteSophisticat" "NauseousHeight"   "MisguidedBasin"  
#>  [9] "DoingRetention"   "ConcentratedMart"
random_role()
#> [1] "FoggyScouring"
replicate(10, random_role())
#>  [1] "AmbivalentSomewh" "BruisingDoorstep" "RetaliatorySuspe" "FumingHomer"     
#>  [5] "NorthClimber"     "DisillusionedImm" "PredictedIllness" "DottedClearing"  
#>  [9] "PrintedParson"    "GleamingSharpnes"
```
