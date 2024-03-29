% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CASdf.R
\docType{class}
\name{casDataFrame-class}
\alias{casDataFrame-class}
\alias{casDataFrame}
\title{CAS Data Frame Class}
\usage{
casDataFrame(
  ...,
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
A casDataFrame is a superset of data.frame. The class is used primarily 
by functions within the package to store tabular data from CAS action 
results and to associate CAS metadata with the tabular data.
}
\details{
A casDataFrame instance is a data object in \R (the client).
}
\section{Slots}{

\describe{
\item{\code{name}}{An optional \code{character} string that specifies CAS metadata 
for a table name.}

\item{\code{label}}{An optional \code{character} string that specifies CAS metadata 
for a table label.}

\item{\code{title}}{An optional \code{character} string that specifies CAS metadata 
for a table title.}

\item{\code{attrs}}{An optional \code{list} of key-value pairs the specify 
user-defined CAS metadata.}

\item{\code{col.labels}}{An optional \code{character} string that specifies CAS 
metadata for column labels.}

\item{\code{col.formats}}{An optional \code{character} string that specifies CAS 
metadata for column formats.}

\item{\code{col.attrs}}{An optional \code{list} that specifies CAS metadata for
column attributes.}

\item{\code{col.sizes}}{An optional \code{list} that specifies CAS metadata for
the number of bytes in the widest row.}

\item{\code{col.types}}{An optional \code{character} that specifies CAS metadata
for the column data types.}

\item{\code{col.widths}}{An optional \code{numeric} that specifies CAS metadata
for column widths.}

\item{\code{df}}{The data.frame to encapsulate in the casDataFrame.}
}}

\seealso{
\code{casDataFrame}
}
% Copyright SAS Institute
