% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CAStab.R
\name{defCasTable}
\alias{defCasTable}
\title{Create a CASTable Object for an Existing CAS Table}
\usage{
defCasTable(
  conn,
  tablename,
  caslib = "",
  columns = "",
  where = "",
  orderby = list(),
  groupby = list(),
  gbmode = ""
)
}
\arguments{
\item{conn}{A \code{\link{CAS}} object that represents
a connection and session in CAS.}

\item{tablename}{A \code{character} that specifies the in-memory 
table name. You can run the cas.table.tableInfo function to
list the in-memory tables.}

\item{caslib}{An optional \code{character} string that
identifies the caslib for the in-memory table. Specify
this parameter to override the active caslib.}

\item{columns}{A \code{list} of column names.}

\item{where}{A \code{character} string that specifies a filter for the
rows to process. The filter uses syntax that is specific to SAS.}

\item{orderby}{A \code{list} of column names. Rows are partitioned according
to the columns in the groupby parameter and then ordered according to
the values of the columns specified in this parameter.}

\item{groupby}{A \code{list} of column names. If you specify this parameter
when you load an in-memory table, then the table is partitioned by the
columns.  If you specify this parameter when running an action, then 
BY-groups are formed temporarily for the duration of the action.}

\item{gbmode}{A \code{character} string. Values are NOSORT (default) or
REDISTRIBUTE. See the CAS product documentation for more information.}
}
\value{
\code{\link{CASTable}}
}
\description{
Creates a \code{\link{CASTable}} object to reference
an existing in-memory table in CAS. You can use this
function to reference tables that were loaded by other
SAS products, other scripts, or from server-side loads
with the cas.table.loadTable function.
}
\examples{
\dontrun{
irisct <- as.casTable(s, iris, casOut="irisct")

# Create another CASTable instance to the same in-memory table,
# but specify that CAS actions are performed by groups of species.
irisct.grouped <- defCasTable(s, tablename="irisct", groupby=list("species"))
}
}
% Copyright SAS Institute
