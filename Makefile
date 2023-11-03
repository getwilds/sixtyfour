PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file
FILE_TARGET := "R/${FILE}"

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples(run_dontrun = TRUE)"

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

test:
	${RSCRIPT} -e "devtools::test()"

readme:
	${RSCRIPT} -e "knitr::knit('README.Rmd')"

lint_package:
	${RSCRIPT} -e "lintr::lint_package()"

style_file:
	# use: make style_file FILE=stuff.R ("R/" is prepended)
	# accepts 1 file only
	${RSCRIPT} -e "styler::style_file('${FILE_TARGET}')"

style_package:
	${RSCRIPT} -e "styler::style_pkg()"
