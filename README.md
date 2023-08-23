
# W4MRUtils

<!-- badges: start -->

<!-- badges: end -->

W4MRUtils is a R packages provided by W4M to ease galaxy tools writing.
It contains some utility functions

## Installation

You can install the development version of W4MRUtils like so:

``` bash
$ git clone https://github.com/workflow4metabolomics/W4MRUtils
$ cd W4MRUtils
```

then

``` bash
$ make install
```

or

``` r
> rmarkdown::render("README.Rmd")
> devtools::document(".")
> roxygen2::roxygenize(".")
> devtools::test(".")
> devtools::install(".", dependencies = FALSE, repos = NULL, type = "source")
```

or

``` bash
$ R -q -e "install.packages('W4MRUtils', repos='https://cran.irsn.fr');"
```

## Uninstallation

You can uninstall the version of W4MRUtils you installed with:

``` bash
$ make remove_package
```

or

``` r
> remove.packages("W4MRUtils")
```

## XML Wrapper

Please follow [the
guidelines](https://galaxy-iuc-standards.readthedocs.io/en/latest/best_practices/tool_xml.html)
during the redaction of the xml wrapper.

Read [the doc](https://docs.galaxyproject.org/en/latest/dev/schema.html)
in case of problems.
