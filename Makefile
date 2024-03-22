PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file
FILE_TARGET := "R/${FILE}"

.PHONY: docs

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

docs:
	${RSCRIPT} -e "pkgdown::build_site(); pkgdown::preview_site(preview=TRUE)"

eg:
	${RSCRIPT} -e "devtools::run_examples(run_dontrun = TRUE)"

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

vign_getting_started:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('sixtyfour.Rmd.og', output = 'sixtyfour.Rmd')";\
	cd ..

vign_billing:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('billing.Rmd.og', output = 'billing.Rmd')";\
	cd ..

vign_db:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('databases.Rmd.og', output = 'databases.Rmd')";\
	cd ..

test:
	${RSCRIPT} -e "devtools::test()"

readme:
	${RSCRIPT} -e "knitr::knit('README.Rmd')"

lint_package:
	${RSCRIPT} -e "lintr::lint_package()"

# use: `make style_file FILE=stuff.R`
# ("R/" is prepended); accepts 1 file only
style_file:
	${RSCRIPT} -e 'styler::style_file(${FILE_TARGET})'

style_package:
	${RSCRIPT} -e "styler::style_pkg()"

update_data:
	${RSCRIPT} -e "source('data-raw/service-mapping.R')"

scan_secrets:
	@echo "scanning for leaks in commits\n"
	gitleaks detect --source . -v
	@echo "\n\n\n"
	@echo "scanning for leaks in uncommitted files\n"
	gitleaks protect --source . -v
