% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CASdf.R
\name{as.casDataFrame}
\alias{as.casDataFrame}
\title{Convert an \R Data Frame to a CAS Data Frame}
\usage{
as.casDataFrame(
  df,
  name = "",
  label = "",
  title = "",
  attrs = list(),
  col.labels = "",
  col.formats = "",
  col.attrs = list(),
  col.sizes = list(),
  col.types = "",
  col.widths = 0
)
}
\value{
casDataFrame
}
\description{
This function is rarely used for programming. It is
used by the package to associate CAS metadata with 
tabular data that is returned by CAS actions.
}
\examples{
\dontrun{
cdf2 = as.casDataFrame(df3[1:4])
cdf = as.casDataFrame(iris)
}
}
% Copyright SAS Institute
