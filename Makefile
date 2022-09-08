R=/home/lpavot/R-versions/R-4.1.2/bin/R
package=W4MRUtils


version = $(shell grep -Po "Version: .*" ./DESCRIPTION |cut -d \  -f 2)
use_kind = Imports


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


new_doc		:
	@$(R) -q -e 'devtools::use_vignette("$(name)")'


# one of bioc, cran, github, svn, local, url
use_%_package:
	@$(R) -q -e 'devtools::install_$*("$(name)")'
	@$(R) -q -e 'usethis::use_package("$(name)", type="$(use_kind)")'

create_test	:
	@$(R) -q -e 'usethis::use_test("$(name)")'

all: clean doc test build check quick_install

test		:doc quick_test coverage
quick_test	: quick_install
	@echo "Running tests..."
	@$(R) -q -e "devtools::test('.')"
	@echo "Finished."

build		:	clean doc quick_build
quick_build	:
	@echo "Building package..."
	@$(R) CMD build "."

check		: quick_build
	@(	\
		cp $(package)_$(version).tar.gz /tmp/ ;	\
		cd /tmp/ ;	\
		$(R) CMD check $(package)_$(version).tar.gz ;	\
		$(R) CMD BiocCheck $(package)_$(version).tar.gz	\
	)
	@echo "Built."

clean		:
	@-ls $(package)_$(version).tar.gz  2> /dev/null | xargs -I file echo "Deleting "file
	@-$(RM) $(package)_$(version).tar.gz 2> /dev/null

doc			: quick_install
	@echo "Generating doc..."
	@$(R) -q -e "rmarkdown::render('README.Rmd')"
	@$(R) -q -e "devtools::document('.')"
	@$(R) -q -e 'library("roxygen2");roxygenize(".");warnings()'
	@echo "Generated."

install: build
	@$(MAKE) quick_install
quick_install: remove_package
	@$(R) -q -e 'devtools::install(".", dependencies=FALSE, repos=NULL, type="source")'

coverage:
	@$(R) -q -e 'covr::report(covr::package_coverage(), "coverage.html")'

lint:
	$(R) -q -e 'lintr::lint_dir("R")'

remove_package:
	@$(R) -q -e 'if ("$(package)" %in% rownames(installed.packages()))remove.packages("$(package)")'
