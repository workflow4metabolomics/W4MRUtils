
R=R
REXEC=$(R) -q -e
MAKE:=$(MAKE) --no-print-directory

package=W4MRUtils
version = $(shell grep -Po "Version: .*" ./DESCRIPTION |cut -d \  -f 2)
use_kind = Imports

sources_files=$(shell ls R/*.R)
test_files=$(shell ls tests/testthat/*.R)
all_R_files=$(sources_files) $(test_files)

package_tgz=$(package)_$(version).tar.gz

man_rd_files=$(shell ls man/*.Rd)
man_html_files=$(man_rd_files:%.Rd=%.html)

vignettes_rd_files=$(shell ls vignettes/*.Rmd)
vignettes_html_files=$(vignettes_rd_files:%.Rmd=%.html)

TARGET = CRAN
# TARGET = BIOC

CHECK = $(REXEC) "devtools::check(pkg = '$(shell pwd)')"
ifeq ($(TARGET),CRAN)
	CHECK = $(R) CMD check --as-cran $(package_tgz)
endif
ifeq ($(TARGET),BIOC)
	CHECK = $(R) CMD BiocCheck $(package_tgz)
endif


all: clean doc test build check lint install

clean: clean_html clean_namespace

clean_html: clean_man_html clean_vign_html

install: doc test quick_install

test: quick_install quick_test report

build: $(package_tgz)

doc: README.md docfiles

NAMESPACE: simple_document

docfiles: simple_document
	$(REXEC) 'pkgdown::build_site()'

simple_document:
	@$(REXEC) "devtools::document('.')"

README.md: README.Rmd
	$(REXEC) "rmarkdown::render('README.Rmd')"

check: build
	@echo "Checking package..."
	@-(	\
		cp $(package_tgz) /tmp/ ;	\
		cd /tmp/ ;	\
		$(CHECK) ;	\
	) ;
	@echo "Checked."

$(package_tgz):		\
	$(all_R_files)	\
	vignettes/*.Rmd	\
	man/*.Rd				\
	README.md				\
	.Rbuildignore		\
	inst/extdata/*	\
	DESCRIPTION			\
	NAMESPACE
	@echo "Building package..."
	@$(R) CMD build "."
	@echo "Built."

ifneq ("$(NO_REPORT)","")
report:
	@echo "[NO_REPORT=$(NO_REPORT)]: reporting cancelled."
else
report: coverage.html
endif

coverage.html: $(all_R_files) $(package_tgz)
	@echo "Doing reporting:"
	@$(REXEC) 'covr::report(covr::package_coverage(), "coverage.html")'

quick_test: $(all_R_files)
	@echo "Running tests..."
	@$(REXEC) "devtools::test('.')"
	@echo "Finished."

quick_install:
	$(REXEC) 'devtools::install(".", dependencies=FALSE, repos=NULL, type="source")'

uninstall:
	@$(REXEC) 'if ("$(package)" %in% rownames(installed.packages()))remove.packages("$(package)")'

clean_namespace:
	@- rm NAMESPACE

clean_man_html:
	@- rm man/*.html

clean_vign_html:
	@- rm vignettes/*.html

lint:
	@$(REXEC) 'lintr::lint_dir("R")'

remove_%.html:
	rm $*

help:
	@echo  ""
	@echo  "Les variables en haut du Makefile peuvent être modifiées en fonction"
	@echo  "de votre configuration."
	@echo  ""
	@echo  "Par exemple, si pour appeller R, vous avez besoin de lancer"
	@echo " la commande \`/var/R-4.2.3/bin/R\`"
	@echo  "alors vous pouvez redéfinir la variable R comme suit:"
	@echo  "R=/var/R-4.2.3/bin/R"
	@echo  ""
	@echo  "Les différentes règles les plus intéressantes:"
	@echo  ""
	@echo  "  - check:"
	@echo  "    Build le package, puis lance les checks."
	@echo  "    Peut-être l'une des commandes les plus" "importantes avec la"
	@echo  "    commande \`make doc\`"
	@echo  "    Pour checker selon les standards du cran ou de bioconductor,"
	@echo  "    vous pouvez le lancer comme tel:"
	@echo  "      - \`TARGET=CRAN make check\`"
	@echo  "      - \`TARGET=BIOC make check\`"
	@echo  ""
	@echo  "  - doc:"
	@echo  "    Produit tous les fichiers de doc"
	@echo  "    et transforme le README.Rmd en README.md"
	@echo  ""
	@echo  "  - README.md:"
	@echo  "    Génère ce fichier à partir du .Rmd"
	@echo  ""
	@echo  "  - install:"
	@echo  "    Génère la documentation, lance les testes unitaires et"
	@echo  "    installe le package en local"
	@echo  ""
	@echo  "  - test:"
	@echo  "    Lance les testes unitaires,"
	@echo  "    et produit le rapport de couverture de code"
	@echo  ""
	@echo  "  - build: "
	@echo  "    Créé le .tar.gz du package"
	@echo  ""
	@echo  "  - uninstall:"
	@echo  "   Désinstalle ce package."
	@echo  ""
	@echo  "  - lint:"
	@echo  "    Lance le linteur R sur le package"
	@echo  ""
	@echo  "  - clean:"
	@echo  "    Supprime les fichiers générés par ce makefile"
	@echo  ""
	@echo  "  - all: (ou juste \`make\`, suivi de rien du tout)"
	@echo  "  Lances les commandes suivantes:"
	@echo  "    - clean"
	@echo  "    - doc"
	@echo  "    - test"
	@echo  "    - build"
	@echo  "    - check"
	@echo  "    - lint"
	@echo  "    - install"