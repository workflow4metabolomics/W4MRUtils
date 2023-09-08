
# W4MRUtils

  - **VERSION**: 1.0.0

<!-- badges: start -->

<!-- badges: end -->

W4MRUtils is a R packages provided by W4M to ease galaxy tools writing.
It contains some utility functions that will help you in common tasks.

## Parameters

  - [Parsing parameters with
    parse\_args](./docs/reference/parse_args.html) (easier)
  - [Parsing parameters with
    optparse](./docs/reference/optparse_parameters.html) (better)
  - [Enforce checking on
    parameters](./docs/reference/check_param_type_n_length.html)

## R script chores

  - [Silently load a package](./docs/reference/shy_lib.html)
  - [Sourcing relative file](./docs/reference/source_local.html)

## Galaxy

  - [Am I in a galaxy env?](./docs/reference/in_galaxy_env.html)
  - [Show log headers](./docs/reference/show_galaxy_header.html)
  - [Show log footer](./docs/reference/show_galaxy_footer.html)
  - [Execute a function](./docs/reference/run_galaxy_processing.html)
  - [Restore parameters names, modified by
    galaxy](./docs/reference/unmangle_galaxy_param.html)

## Logfiles

  - [What is a logger?](./docs/articles/logging.html#what-is-a-logger)
  - [How to create a
    logger?](./docs/articles/logging.html#how-to-create-a-logger)
  - [How to create a log
    file?](./docs/articles/logging.html#how-to-create-a-log-file)

## TODO

Do the documentation and the referencing of the documentation for:

  - stock\_id
  - reproduce\_id
  - check\_err
  - match2
  - match3
  - df\_is
  - df\_force\_numeric
  - df\_read\_table

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
