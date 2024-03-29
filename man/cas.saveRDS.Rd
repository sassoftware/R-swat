% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_write.R
\name{cas.saveRDS}
\alias{cas.saveRDS}
\title{Write a CAS Table to RDS}
\usage{
cas.saveRDS(
  CASTable,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL
)
}
\arguments{
\item{CASTable}{The instance of the CASTable to save as
as an RDS file.}

\item{file}{An \code{character} string that 
specifies the filename for the RDS file. If you do not
specify the file, then the in-memory table name is used
with an .rds suffix. This value is passed to \code{saveRDS}.}

\item{ascii}{An optional \code{logical} value. When set to
TRUE or NA, then an ASCII representation is written.
Otherwise, a binary is written.  This value is passed to
\code{saveRDS}.}

\item{version}{An optional \code{numeric} value. This value
is passed to \code{saveRDS}.}

\item{compress}{An optional \code{logical} value. When set
to one of TRUE, "gzip", "bzip2", or "xz", then compression is
used. TRUE performs gzip. This value is passed to \code{saveRDS}.}

\item{refhook}{An optional value that is passed to
\code{saveRDS}.}
}
\description{
This function downloads an in-memory table from the
CAS server and saves it as an RDS file that is accessible
to \R (the client). This function is a convenience
wrapper for the \R \code{saveRDS} function.
}
\examples{
\dontrun{
cas.saveRDS(myCasTable, file="/path/to/data_out.rds")
}
}
\seealso{
Other functions for saving in-memory data: 
\code{\link{cas.write.csv}()},
\code{\link{cas.write.table}()},
\code{\link{cas.write.xlsx}()}
}
\concept{functions for saving in-memory data}
% Copyright SAS Institute
