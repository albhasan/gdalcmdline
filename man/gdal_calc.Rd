% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdal_util.R
\name{gdal_calc}
\alias{gdal_calc}
\title{Do calculations on images.}
\usage{
gdal_calc(
  input_files,
  out_filename = tempfile(pattern = "gdal_calc_out_", fileext = ".tif"),
  expression,
  band_number = NULL,
  dstnodata = NULL,
  data_type = NULL,
  out_format = NULL,
  creation_option = NULL,
  all_bands = NULL,
  overwrite = FALSE,
  verbose = FALSE,
  quiet = FALSE,
  dry_run = FALSE
)
}
\arguments{
\item{input_files}{A character. Paths to the image files.}

\item{out_filename}{A length-one character. The path to the
destination file.}

\item{expression}{A character. The expresion to compute on the
images using upper case A-Z letters to represent the images in the same order
as input_files.}

\item{band_number}{An integer. The band numbers in input_files.}

\item{dstnodata}{A length-one integer. Output nodata value.}

\item{data_type}{A length-one character. Ouput datatype. It must
be one of c('Int32', 'Int16', 'Float64', 'UInt16', 'Byte', 'UInt32',
'Float32').}

\item{out_format}{A length-one character. The output format.}

\item{creation_option}{A character. Creation options for output file
e.g. c('NAME1=VALUE1', 'NAME2=VALUE2')}

\item{all_bands}{A character. Process all bands of given raster
(A-Z).}

\item{overwrite}{A length-one logical. The default is FALSE.}

\item{verbose}{A length-one logical. Generate a verbose output.
The default is FALSE.}

\item{quiet}{A length-one logical. The default is FALSE.}

\item{dry_run}{A length-one logical. Do not run, just print the sytem call.}
}
\value{
out_filename A length-one character.
}
\description{
R wrapper for gdal_calc.py
}
\author{
Alber Sanchez, \email{alber.ipia@inpe.br}
}
