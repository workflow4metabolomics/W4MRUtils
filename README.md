
# W4MRUtils

<!-- badges: start -->

<!-- badges: end -->

W4MRUtils is a R packages provided by W4M to ease galaxy tools writing.
It contains some utility functions

## Installation

You can install the development version of W4MRUtils like so:

``` bash
git clone https://github.com/workflow4metabolomics/W4MRUtils
cd W4MRUtils
```

then

``` bash
make install
```

or

``` r
rmarkdown::render("README.Rmd")
devtools::document(".")
roxygen2::roxygenize(".")
devtools::test(".")
devtools::install(".", dependencies = FALSE, repos = NULL, type = "source")
```

## Uninstallation

You can install the development version of W4MRUtils like so:

``` bash
make remove_package
```

or

``` r
remove.packages("W4MRUtils")
```

## Usages

### parsing parameters

R script command line:

``` bash
Rscript my_script.R --a-integer 42 --a-float 3.14 --a-boolean FALSE --a-list 1,2,3
```

R script content:

``` r
param_printer <- function(name, args) {
  cat(sprintf(
    "%s[%s] %s",
    name,
    class(args[[name]])[1],
    paste(args[[name]], collapse = " ")
  ))
}
args <- W4MRUtils::parse_args(commandArgs())
#> Warning in W4MRUtils::parse_args(commandArgs()): Please, use the 'optparse'
#> library instead of the 'parse_args' function.
param_printer("a-integer", args)
#> a-integer[numeric] 42
param_printer("a-float", args)
#> a-float[numeric] 3.14
param_printer("a-boolean", args)
#> a-boolean[logical] FALSE
param_printer("a-list", args)
#> a-list[character] 1,2,3
args$`a-list` <- as.numeric(strsplit(args$`a-list`, ",")[[1]])
param_printer("a-list", args)
#> a-list[numeric] 1 2 3
```

## XML Wrapper

Please follow [the
guidelines](https://galaxy-iuc-standards.readthedocs.io/en/latest/best_practices/tool_xml.html)
during the redaction of the xml wrapper.

Read [the doc](https://docs.galaxyproject.org/en/latest/dev/schema.html)
in case of problems.
