
# R=/home/lpavot/R-versions/R-4.1.2/bin/R
R=R
REXEC=$(R) -q -e
package=W4MRUtils
version = $(shell grep -Po "Version: .*" ./DESCRIPTION |cut -d \  -f 2)
use_kind = Imports
PACKAGE_FILE_NAME = $(package)_$(version).tar.gz

man_rd_files=$(shell ls man/*.Rd)
man_html_files=$(man_rd_files:%.Rd=%.html)
sources_files=$(shell ls R/*.R)
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


help_string=\n \
usage: make command1 [command2 [...]] [name=...] [use_kind=Suggest|Imports|Depends] \n \
\n \
standalone commands\n \
\tall: Runs the whole workflow clean doc test build check install.\n\
\ttest: runs all unit tests. \n\
\t\tImplies doc  \n \
\tquick_test: runs all unit tests, without rebuilding. \n\
\t\tImplies quick install. \n \
\tcheck: runs R checks against the packages. \n\
\t\tImplies build. \n \
\tbuild: build the package in a tar.gz (or zip). \n\
\t\tImplies clean and doc. \n \
\tquick_build: build the package. Does not update the doc nor clean the package. \n \
\tclean: remove the result of "build" \n \
\tdoc: generate the documentation from roxygen comments\n \
\tquick_install: installs the package. Do not do checks. \n \
\tremove_package: uninstall the packages. \n \
\n \
 commands that needs \"name\" to be set \n \
\tnew_doc: create a new vignette documentation with the given name \n \
\tuse_%%_package: \n \
\t\t%% is either bioc, cran, github, svn, local or url. \n \
\t\tinstalls the given package and add it to the package dependencies. \n \
\t\tthe \"use_kind\" variable can be set to either Suggest, Imports, Enhances or Depends\n \
\t\tthe default value for use_kind is $(use_kind)\n \
\tcreate_test: create a unit test file with the given name \n \
\n \
\n \
author: $(USER) \n \
mail: $(USER)@inrae.fr \n \
version: $(version) \n \
creation date: $(shell date --rfc-email)\n \

help		:
	@printf -- "$(help_string)"


new_vignette_%	:
	$(MAKE) new_vignette name=$*

new_vignette		:
	@$(REXEC) 'usethis::use_vignette("$(name)")'

render_vign: $(vignettes_html_files)

vignettes/%.html: vignettes/%.Rmd
	@echo Rendering vignettes page for $^
	@$(REXEC) 'devtools::build_rmd("$^")'

render_man: $(man_html_files)

man/%.html: man/%.Rd
	@echo Rendering man page for $^
	@$(REXEC) 'tools::Rd2HTML("$^", out="$@")'


build	: doc $(package)_$(version).tar.gz

check: build
	@(	\
		cp $(package)_$(version).tar.gz /tmp/ ;	\
		cd /tmp/ ;	\
		$(CHECK) ;	\
	) ;
	@echo "Checked."


$(package)_$(version).tar.gz:
	@echo "Building package..."
	@$(R) CMD build "."
	@echo "Built."

doc		: $(readmemd) docfiles render_vign render_man

docfiles: $(sources_files)
	@$(REXEC) "devtools::document('.')"

$(readmemd):$(readmermd)
	$(REXEC) "rmarkdown::render('README.Rmd')"

test		: quick_test
	@$(REXEC) 'covr::report(covr::package_coverage(), "coverage.html")'

quick_test	: quick_install
	@echo "Running tests..."
	@$(REXEC) "devtools::test('.')"
	@echo "Finished."

quick_install: remove_package
	@$(REXEC) 'devtools::install(".", dependencies=FALSE, repos=NULL, type="source")'

remove_package:
	@$(REXEC) 'if ("$(package)" %in% rownames(installed.packages()))remove.packages("$(package)")'



# # one of bioc, cran, github, svn, local, url
# use_%_package:
# 	@$(REXEC) 'devtools::install_$*("$(name)")'
# 	@$(REXEC) 'usethis::use_package("$(name)", type="$(use_kind)")'

# create_test	:
# 	@$(REXEC) 'usethis::use_test("$(name)")'

# all: clean doc test build check quick_install

# test		:doc quick_test coverage
# quick_test	: quick_install
# 	@echo "Running tests..."
# 	@$(REXEC) "devtools::test('.')"
# 	@echo "Finished."

# build		:	clean doc quick_build
# quick_build	: $(package)_$(version).tar.gz

# $(package)_$(version).tar.gz:
# 	@echo "Building package..."
# 	@$(R) CMD build "."
# 	@echo "Built."

# check: quick_build
# 	@(	\
# 		cp $(package)_$(version).tar.gz /tmp/ ;	\
# 		cd /tmp/ ;	\
# 		$(CHECK) ;	\
# 	) ;
# 	@echo "Checked."

# clean		:
# 	@-ls $(package)_$(version).tar.gz  2> /dev/null | xargs -I file echo "Deleting "file
# 	@-$(RM) $(package)_$(version).tar.gz 2> /dev/null
# 	@-$(RM) NAMESPACE 2> /dev/null

# doc			: clean quick_install
# 	@echo "Generating doc..."
# 	@$(REXEC) "rmarkdown::render('README.Rmd')"
# 	@$(REXEC) "devtools::document('.')"
# 	@$(REXEC) 'library("roxygen2");roxygenize(".");warnings()'
# 	@echo "Generated."

# install: build
# 	@$(MAKE) quick_install
# quick_install: remove_package
# 	@$(REXEC) 'devtools::install(".", dependencies=FALSE, repos=NULL, type="source")'

# coverage:
# 	@$(REXEC) 'covr::report(covr::package_coverage(), "coverage.html")'

# lint:
# 	$(REXEC) 'lintr::lint_dir("R")'

# remove_package:
# 	@$(REXEC) 'if ("$(package)" %in% rownames(installed.packages()))remove.packages("$(package)")'
