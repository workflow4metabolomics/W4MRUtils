
FROM debian:latest

MAINTAINER Lain Pavot <lain.pavot@inrae.fr>

WORKDIR /

ENV R_VERSION 4.3.0
ENV ICU_VERSION 58
ENV BASE_ICU_URL=https://github.com/unicode-org/icu/releases/download/
ENV ICU_URL=${BASE_ICU_URL}/release-${ICU_VERSION}-2/icu4c-${ICU_VERSION}_2-src.tgz
ENV R_URL=https://cran.r-project.org/src/base/R-4/R-${R_VERSION}.tar.gz

ENV R_COMPILE_FLAGS="           \
    -g                          \
    -O2                         \
    -fstack-protector-strong    \
    -Wformat                    \
    -Werror=format-security     \
    -Wdate-time                 \
    -D_FORTIFY_SOURCE=2         \
    -g                          \
"

ENV BUILD_DEPS=" \
    qpdf                                    \
    wget                                    \
    gcc                                     \
    gfortran                                \
    g++                                     \
    file                                    \
    zlib1g                                  \
    zlib1g-dev                              \
    bzip2                                   \
    libbz2-dev                              \
    liblzma5                                \
    liblzma-dev                             \
    libpcre2-posix2                         \
    libpcre2-dev                            \
    libcairo2-dev                           \
    libcurl4                                \
    libcurl4-openssl-dev                    \
    libpng-dev                              \
    libjpeg-dev                             \
    libreadline8                            \
    libreadline-dev                         \
    libtiff5-dev                            \
    libx11-dev                              \
    libxt-dev                               \
    make                                    \
"

ENV REMOVE_DEPS=" \
    libbz2-dev \
    libcurl4-openssl-dev \
    libpcre2-dev \
    libpng-dev \
    libreadline-dev \
    libtiff5-dev \
    liblzma-dev \
    libx11-dev \
    libxt-dev \
"

RUN apt-get update                              \
    && apt-get install -y ${BUILD_DEPS}         \
    && wget "${ICU_URL}"                        \
    && wget "${R_URL}"                          \
    && tar -xf icu4c-${ICU_VERSION}_2-src.tgz   \
    && mv /icu/ /opt/icu                        \
    && tar -xf "R-${R_VERSION}.tar.gz"          \
    && cd "R-${R_VERSION}"                      \
    &&                                          \
    CFLAGS="${R_COMPILE_FLAGS}"                 \
    CXXFLAGS="${R_COMPILE_FLAGS}"               \
    ./configure                                 \
        --enable-R-shlib                        \
        --enable-memory-profiling               \
        --with-blas                             \
        --with-lapack                           \
        --with-cairo                            \
        --with-x=no                             \
        --with-readline                         \
        --prefix=/opt/R-${R_VERSION}            \
    && make                                     \
        CFLAGS=-fPIC                            \
        LDFLAGS=-L/opt/icu/source/lib/          \
    && make install                             \
    && cd /                                     \
    && rm -rf /icu4c-${ICU_VERSION}_2-src.tgz   \
    && rm -rf /R-${R_VERSION}.tar.gz            \
    && rm -rf /R-${R_VERSION}                   \
    && apt-get purge -y ${REMOVE_DEPS}          \
    && apt-get clean autoclean                  \
    && apt-get autoremove --yes                 \
    && rm -rf /tmp/*                            \
    && rm -rf /var/log/*                        \
;

RUN ln -s /opt/R-${R_VERSION}/bin/R /bin/R
RUN ln -s /opt/R-${R_VERSION}/bin/Rscript /bin/Rscript

RUN R -q -e "install.packages('W4MRUtils', repos='https://cran.irsn.fr');"

CMD ["R", "-q"]

