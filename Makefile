PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file
FILE_TARGET := "R/${FILE}"
MAN_TARGET := "man/${TOPIC}.Rd"

.PHONY: docs

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

docs:
	${RSCRIPT} -e "pkgdown::build_site(); pkgdown::preview_site(preview=TRUE)"

egs:
	${RSCRIPT} -e "devtools::run_examples(run_dontrun = TRUE)"

# use: `make eg TOPIC=aws_file_exists`
# ("man/" is prepended); accepts 1 file only
eg:
	${RSCRIPT} -e 'pkgload::load_all(); pkgload::run_example(${MAN_TARGET})'

check: build
	_R_CHECK_SYSTEM_CLOCK_=0 _R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

vign_getting_started:
	cd vignettes;\
	CLIPR_ALLOW=TRUE ${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('sixtyfour.Rmd.og', output = 'sixtyfour.Rmd')";\
	cd ..

vign_s3:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('s3.Rmd.og', output = 's3.Rmd')";\
	cd ..

vign_billing:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('billing.Rmd.og', output = 'billing.Rmd')";\
	cd ..

vign_db:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('databases.Rmd.og', output = 'databases.Rmd')";\
	cd ..

vign_six:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('six.Rmd.og', output = 'six.Rmd')";\

vign_auth:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('auth.Rmd.og', output = 'auth.Rmd')";\
	cd ..

vign_s3iam:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true')" \
	-e "knitr::knit('s3iam.Rmd.og', output = 's3iam.Rmd')";\
	sed "s/${AWS_ACCOUNT_ID}/*****/g" s3iam.Rmd > tmp && mv tmp s3iam.Rmd;\
	cd ..

test:
	SIXTYFOUR_RUN_LOCAL_ONLY_TESTS=true ${RSCRIPT} -e "devtools::test()"

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

minio_start:
	MINIO_USER=${MINIO_USER} ;\
	MINIO_PWD=${MINIO_PWD} ;\
	MINIO_ENDPOINT=${MINIO_ENDPOINT} ;\
	minio server /tmp/minio --console-address :9090

localstack_start:
ifeq (, $(shell command -v localstack))
	$(error "No localstack detected. See https://docs.localstack.cloud/getting-started/installation/")
endif
	localstack start
