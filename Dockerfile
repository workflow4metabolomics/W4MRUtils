
FROM rocker/tidyverse:latest

MAINTAINER Lain Pavot <lain.pavot@inrae.fr>

ENV dependencies=c("\
    'DT',           \
    'covr',         \
    'qpdf',         \
    'knitr',        \
    'rmarkdown',    \
    'testthat'      \
)"

RUN [ "$(which qpdf)" = "" ]        \
    && apt-get update               \
    && apt-get install -y qpdf      \
    &&  apt-get clean autoclean     \
    &&  apt-get autoremove --yes    \
    &&  rm -rf /var/lib/apt/lists/* \
    &&  rm -rf /tmp/*               ;

RUN R -q -e "                               \
    options(repos='https://cran.irsn.fr');  \
    install.packages($dependencies)         \
"                                           ;

CMD []

