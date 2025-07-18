% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logging.R
\name{get_logger}
\alias{get_logger}
\title{Instantiate a Logger}
\usage{
get_logger(name, recreate = FALSE, ...)
}
\arguments{
\item{name}{the name of the logger to get or create. This name will
be used in logs to differentiate from which part of you program
comes which lines of log. See the example of usage bellow.}

\item{recreate}{logical=FALSE tells whether to recreate the logger
of the given name or not. Preferably, one should not recreate
a new logger each time.}

\item{...}{Parameters passed out to the W4MLogger() function}
}
\value{
A new W4MLogger instance if it did not exist or if recreate is
TRUE. Otherwise, a new W4MLogger instance.
}
\description{
Create a logger of the given name. You can call again \code{get_logger} and
provide the same name to get the same logger. It will not be recreated
unless recreate is TRUE.
}
\details{
get_logger
}
\examples{
## let's say our program is divided in three big tasks:
##   - the parsing
##   - the processing
##   - the output writing
parser_logger <- W4MRUtils::get_logger("Parser")
process_logger <- W4MRUtils::get_logger("Processing")
write_logger <- W4MRUtils::get_logger("Writer")
input_path <- "/home/anyone/input.csv"
parser_logger$info(paste("Parsing the input file at", input_path))
parser_logger$debug("Input extension detected: csv")
parser_logger$debug("The csv parser program will be used")
## do the parsing...
input <- list(a=1:5, b=5:10, c=8:2)
parser_logger$info("Parsing succeed")
process_logger$info("Starting the processing of:", input)
process_logger$debug("The processing has started at...")
result <- as.list(input)
process_logger$debug("The processing has finished at...")
process_logger$info("Processing finished in x seconds.")
outfile <- "/home/anyone/output.tsv"
write_logger$info(paste("Creating the output in", outfile))

## we detected that the input was csv and the out was tsv:
## but it is not a blocking problem
write_logger$warning("The input and output file's extensions are different")
write_logger$debug("The output will be casted from csv to tsv")

## we try to write the file, but it fails
tryCatch({
  ## writing the output file failed with this error:
  stop(sprintf("I/O Error: \%s is not writable.", outfile))
}, error = function(e) {
  write_logger$error(e$message)
  write_logger$error("Writing output file aborted.")
  ## quit(save = "no", status = 42)
})

## note that debug output were not written. To show debug logs
## we have to active it (disabled by default):

write_logger$set_debug()
write_logger$debug("The debug outputs are now visible!")

}
\author{
L.Pavot
}
