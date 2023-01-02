
R=R
REXEC=$(R) -q -e
package=W4MRUtils
version = $(shell grep -Po "Version: .*" ./DESCRIPTION |cut -d \  -f 2)
use_kind = Imports
PACKAGE_FILE_NAME = $(package)_$(version).tar.gz

MAKE:=$(MAKE) --no-print-directory

man_rd_files=$(shell ls man/*.Rd)
man_html_files=$(man_rd_files:%.Rd=%.html)
sources_files=$(shell ls R/*.R)
test_files=$(shell ls tests/testthat/*.R)
all_R_files=$(sources_files) $(test_files)
description=DESCRIPTION
namespace=NAMESPACE
readmemd=README.md
readmermd=README.Rmd

vignettes_rd_files=$(shell ls vignettes/*.Rmd)
vignettes_html_files=$(vignettes_rd_files:%.Rmd=%.html)

# TARGET = CRAN
# TARGET = BIOC

CHECK = $(REXEC) "devtools::check(pkg = '$(shell pwd)')"
ifeq ($(TARGET),CRAN)
	CHECK = $(R) CMD check --as-cran $(PACKAGE_FILE_NAME)
endif
ifeq ($(TARGET),BIOC)
	CHECK = $(R) CMD BiocCheck $(PACKAGE_FILE_NAME)
endif


all: clean doc test build check install

help		:
	@printf -- "$(help_string)"

new_vignette_%	:
	$(MAKE) new_vignette name=$*

new_vignette		:
	@$(REXEC) 'usethis::use_vignette("$(name)")'

render_vign:
	@$(MAKE) render_doc name=vignettes ext=Rmd
render_man:
	@$(MAKE) render_doc name=man ext=Rd
render_doc:
	@$(MAKE) $(${name}_html_files) ${name}/index.html

${name}/%.html: ${name}/%.${ext}
	@echo Rendering ${name} page for $^
	@$(REXEC) 'devtools::build_rmd("$^")'

${name}/index.html:
	@echo "<!doctype HTML><html><body><ul>" > ${name}/index.html
	@ls ${name}/*.html \
		| xargs -I : echo "<li><a href='../:'>:</a></li>" >> ${name}/index.html
	@echo "</ul></body></html>" >> ${name}/index.html

docfiles: $(sources_files)
	@$(REXEC) "devtools::document('.')"
	@$(REXEC) 'devtools::build_rmd(file.path("man", list.files(pattern="man/*.Rd")))'
	@$(REXEC) 'devtools::build_rmd(file.path("vignettes", list.files(pattern="vignettes/*.Rmd")))'

$(readmemd):$(readmermd)
	$(REXEC) "rmarkdown::render('README.Rmd')"

doc		: $(readmemd) docfiles

build	: $(package)_$(version).tar.gz

check: build
	@(	\
		cp $(package)_$(version).tar.gz /tmp/ ;	\
		cd /tmp/ ;	\
		$(CHECK) ;	\
	) ;
	@echo "Checked."

$(package)_$(version).tar.gz: \
	$(all_R_files) \
	vignettes/*.Rmd \
	man/*.Rd \
	$(readmemd) \
	.Rbuildignore \
	inst/extdata/* \
	DESCRIPTION \
	NAMESPACE
	@echo "Building package..."
	@$(R) CMD build "."
	@echo "Built."

test		: quick_test report

report: coverage

coverage:
	@$(R) -e 'covr::report(covr::package_coverage(), "coverage.html")'

quick_test: $(all_R_files)
	@echo "Running tests..."
	@$(REXEC) "devtools::test('.')"
	@echo "Finished."

install: test quick_install

quick_install:
	$(REXEC) 'devtools::install(".", dependencies=FALSE, repos=NULL, type="source")'

uninstall:
	@$(REXEC) 'if ("$(package)" %in% rownames(installed.packages()))remove.packages("$(package)")'

clean: clean_html

clean_html: clean_man_html clean_vign_html

clean_man_html:
	@- rm man/*.html

clean_vign_html:
	@- rm vignettes/*.html

lint:
	$(REXEC) 'lintr::lint_dir("R")'

remove_%.html:
	rm $*


# one of bioc, cran, github, svn, local, url
use_%_package:
	@$(REXEC) 'devtools::install_$*("$(name)")'
	@$(REXEC) 'usethis::use_package("$(name)", type="$(use_kind)")'

create_test	:
	@$(REXEC) 'usethis::use_test("$(name)")'

