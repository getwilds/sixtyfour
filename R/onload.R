# internal sixtyfour package environment
env64 <- new.env()

.onLoad <- function(libname, pkgname) {
  # sets creds for paws and s3fs for the S3 service
  env64$s3 <- set_s3_interface("aws")

  # iam and costexplorer services
  env64$iam <- paws::iam()
  env64$costexplorer <- paws::costexplorer()
  env64$secretsmanager <- paws::secretsmanager()
}
