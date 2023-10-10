# internal sixtyfour package environment
env64 <- new.env()

.onLoad <- function(libname, pkgname) {
	env64$s3 <- paws::s3()
	env64$iam <- paws::iam()
	env64$costexplorer <- paws::costexplorer()
}