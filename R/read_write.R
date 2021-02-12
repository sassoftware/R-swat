#
# Copyright SAS Institute
#
#  Licensed under the Apache License, Version 2.0 (the License);
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

# Many of these functions are here for backwards compatibility.
# You can use the builtin R write.* functions on CASTables themselves.
# In those cases, the CASTable is cast to a data.frame which automatically
# downloads the data from the server to write.

#' Construct a filename based on the CAS table name
#'
#' @param x    CASTable object
#' @param file Input date file name
#' @param ext  Output file extension
#'
#' @return character
#'
#' @keywords internal
.output_file <- function(x, file, ext) {
  if (nchar(file) == 0) {
    return(paste(x@tname, ext, sep = ""))
  }
  return(file)
}

#' Construct a casOut parameter based on the given parameters
#'
#' @param out List containing given casOut parameter
#' @param file Input data file name
#'
#' @return List contaning new casOut parameter
#'
#' @keywords internal
.casout <- function(out, file) {
  out <- c(out)
  if (is.character(out)) {
    out <- list(name = out, replace = FALSE)
  }
  else if (is.null(out)) {
      out <- list(replace = FALSE)
  }
  if (is.null(out$name) || nchar(out$name) == 0) {
    out$name <- gsub("\\.\\w+$", "", (basename(file)))
  }
  return(out)
}

setGeneric("cas.write.csv",
  function(x, file = "", ...) {
    standardGeneric("cas.write.csv")
  }
)

#' @export
setMethod(
  "cas.write.csv",
  signature(x = "CASTable"),
  function(x, file = "", ...) {
    write.csv(as.data.frame(x), file = .output_file(x, file, ".csv"), ...)
  }
)

setGeneric("cas.write.csv2",
  function(x, file = "", ...) {
    standardGeneric("cas.write.csv2")
  }
)

#' @export
setMethod(
  "cas.write.csv2",
  signature(x = "CASTable"),
  function(x, file = "", ...) {
    write.csv2(as.data.frame(x), file = .output_file(x, file, ".csv"), ...)
  }
)

setGeneric("cas.write.xlsx",
  function(x, file = "", ...) {
    standardGeneric("cas.write.xlsx")
  }
)

#' @export
setMethod(
  "cas.write.xlsx",
  signature(x = "CASTable"),
  function(x, file = "", ...) {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("xlsx package is needed for this function to work.", call. = FALSE)
    }
    write.xlsx(as.data.frame(x), file = .output_file(x, file, ".xlsx"), ...)
  }
)

setGeneric("cas.saveRDS",
  function(x, file = "", ...) {
    standardGeneric("cas.saveRDS")
  }
)

#' @export
setMethod(
  "cas.saveRDS",
  signature(x = "CASTable"),
  function(x, file = "", ...) {
    saveRDS(as.data.frame(x), file = .output_file(x, file, ".rds"), ...)
  }
)

setGeneric("cas.write.table",
  function(x, file = "", ...) {
    standardGeneric("cas.write.table")
  }
)

#' @export
setMethod(
  "cas.write.table",
  signature(x = "CASTable"),
  function(x, file = "", ...) {
    write.table(as.data.frame(x), file = .output_file(x, file, ".tbl"), ...)
  }
)

setGeneric("cas.read.csv",
  function(x, file, ..., casOut = NULL) {
    standardGeneric("cas.read.csv")
  }
)

#' Read a CSV File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for
#' the \R \code{read.csv} and \code{as.CASTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in
#' CAS (the server).
#'
#' @param x An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that specifies
#'   the filename or connection for the data to read.
#' @param \dots Optional parameters that are passed to
#'   \code{read.csv}.
#' @param casOut An optional \code{character} or list. If
#'   you specify a string, then the string is used as the
#'   in-memory table name. A list can be used to specify
#'   properties for the in-memory table as follows:
#'   \describe{
#'     \item{\code{name}}{An optional \code{character} that
#'       specifies the name for the in-memory table. By
#'       default, the name of the data frame is used.}
#'     \item{\code{caslib}}{An optional \code{character} that
#'       specifies the caslib. Specify this parameter to
#'       override the active caslib.}
#'     \item{\code{label}}{An optional \code{character} that
#'       specifies a descriptive label for the data.}
#'     \item{\code{replace}}{An optional \code{logical}. When
#'       set to TRUE, you can replace an existing in-memory
#'       table with the same name in the same caslib. The
#'       default value is FALSE.}
#'     \item{\code{promote}}{An optional \code{logical}. When
#'       set to TRUE, the in-memory table has global scope and
#'       can be available to other CAS sessions (subject to
#'       access controls). The default value is FALSE and
#'       the in-memory table has session scope so that it is
#'       accessible with the session that uploaded the table
#'       only. Session-scope tables are ideal for data analysis.
#'       Global-scope tables are better suited for reporting.}
#'     \item{\code{replication}}{An optional \code{numeric} that
#'       specifies the number of redundant copies of in-memory
#'       blocks. This parameter applies to distributed servers
#'       only. The default value is 1.}
#'    }
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' # Upload a CSV, the in-memory table is named HEART
#' heartct <- cas.read.csv(s, "http://support.sas.com/documentation/
#'   onlinedoc/viya/exampledatasets/heart.csv")
#'
#' # Upload the same CSV, name the in-memory table HEARTCT
#' heartct <- cas.read.csv(s, "http://support.sas.com/documentation/
#'   onlinedoc/viya/exampledatasets/heart.csv",
#'   casOut = list(name = "heartct", replace = TRUE)
#' )
#' }
#'
#' @export
setMethod(
  "cas.read.csv",
  signature(x = "CAS"),
  function(x, file, ..., casOut = "") {
    return(as.CASTable(x, read.csv(file, ...), casOut = .casout(casOut, file)))
  }
)

setGeneric("cas.read.xlsx",
  function(x, file, ..., casOut = "") {
    standardGeneric("cas.read.xlsx")
  }
)

#' Read an XLSX File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for
#' the \R \code{read.xlsx} and \code{as.CASTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in
#' CAS (the server).
#'
#' @param x An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that
#'   specifies the filename for the XLSX file.
#'   This value is passed to \code{read.xslx}.
#' @param \dots Parameters sent to the \code{read.xlsx} function.
#' @param casOut An optional \code{character} or list. If
#'   you specify a string, then the string is used as the
#'   in-memory table name. A list can be used to specify
#'   properties for the in-memory table as follows:
#'   \describe{
#'     \item{\code{name}}{An optional \code{character} that
#'       specifies the name for the in-memory table. By
#'       default, the name of the data frame is used.}
#'     \item{\code{caslib}}{An optional \code{character} that
#'       specifies the caslib. Specify this parameter to
#'       override the active caslib.}
#'     \item{\code{label}}{An optional \code{character} that
#'       specifies a descriptive label for the data.}
#'     \item{\code{replace}}{An optional \code{logical}. When
#'       set to TRUE, you can replace an existing in-memory
#'       table with the same name in the same caslib. The
#'       default value is FALSE.}
#'     \item{\code{promote}}{An optional \code{logical}. When
#'       set to TRUE, the in-memory table has global scope and
#'       can be available to other CAS sessions (subject to
#'       access controls). The default value is FALSE and
#'       the in-memory table has session scope so that it is
#'       accessible with the session that uploaded the table
#'       only. Session-scope tables are ideal for data analysis.
#'       Global-scope tables are better suited for reporting.}
#'     \item{\code{replication}}{An optional \code{numeric} that
#'       specifies the number of redundant copies of in-memory
#'       blocks. This parameter applies to distributed servers
#'       only. The default value is 1.}
#'    }
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' myCasTable <- cas.read.xlsx(s,
#'   file = "/path/to/data_out.xlsx",
#'   sheetIndex = 1,
#'   casOut = list(name = "mycastable", replace = TRUE)
#' )
#' }
#'
#' @export
setMethod(
  "cas.read.xlsx",
  signature(x = "CAS"),
  function(x, file, ..., casOut = "") {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("xlsx package is needed for this function to work.", call. = FALSE)
    }
    return(as.CASTable(x, read.xlsx(file, ...), casOut = .casout(casOut, file)))
  }
)

setGeneric("cas.readRDS",
  function(x, file, ..., casOut = "") {
    standardGeneric("cas.readRDS")
  }
)

#' Read an RDS File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for
#' the \R \code{readRDS} and \code{as.CASTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in
#' CAS (the server).
#'
#' @param x An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that
#'   specifies the filename for the RDS file.
#'   This value is passed to \code{readRDS}.
#' @param \dots Parameters sent to the \code{readRDS} function.
#' @param casOut An optional \code{character} or list. If
#'   you specify a string, then the string is used as the
#'   in-memory table name. A list can be used to specify
#'   properties for the in-memory table as follows:
#'   \describe{
#'     \item{\code{name}}{An optional \code{character} that
#'       specifies the name for the in-memory table. By
#'       default, the name of the data frame is used.}
#'     \item{\code{caslib}}{An optional \code{character} that
#'       specifies the caslib. Specify this parameter to
#'       override the active caslib.}
#'     \item{\code{label}}{An optional \code{character} that
#'       specifies a descriptive label for the data.}
#'     \item{\code{replace}}{An optional \code{logical}. When
#'       set to TRUE, you can replace an existing in-memory
#'       table with the same name in the same caslib. The
#'       default value is FALSE.}
#'     \item{\code{promote}}{An optional \code{logical}. When
#'       set to TRUE, the in-memory table has global scope and
#'       can be available to other CAS sessions (subject to
#'       access controls). The default value is FALSE and
#'       the in-memory table has session scope so that it is
#'       accessible with the session that uploaded the table
#'       only. Session-scope tables are ideal for data analysis.
#'       Global-scope tables are better suited for reporting.}
#'     \item{\code{replication}}{An optional \code{numeric} that
#'       specifies the number of redundant copies of in-memory
#'       blocks. This parameter applies to distributed servers
#'       only. The default value is 1.}
#'    }
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' myCasTable <- cas.readRDS(s,
#'   file = "/path/to/data_out.rds",
#'   casOut = list(name = "mycastable")
#' )
#' }
#'
#' @export
setMethod(
  "cas.readRDS",
  signature(x = "CAS"),
  function(x, file, ..., casOut = "") {
    return(as.CASTable(x, readRDS(file, ...), casOut = .casout(casOut, file)))
  }
)

setGeneric("cas.read.table",
  function(x, file, ..., casOut = "") {
    standardGeneric("cas.read.table")
  }
)

#' Read a File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for
#' the \R \code{read.table} and \code{as.CASTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in
#' CAS (the server).
#'
#' @param x An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that
#'   specifies the filename. This value is passed to \code{read.table}.
#' @param \dots Parameters sent to the \code{read.table} function.
#' @param casOut An optional \code{character} or list. If
#'   you specify a string, then the string is used as the
#'   in-memory table name. A list can be used to specify
#'   properties for the in-memory table as follows:
#'   \describe{
#'     \item{\code{name}}{An optional \code{character} that
#'       specifies the name for the in-memory table. By
#'       default, the name of the data frame is used.}
#'     \item{\code{caslib}}{An optional \code{character} that
#'       specifies the caslib. Specify this parameter to
#'       override the active caslib.}
#'     \item{\code{label}}{An optional \code{character} that
#'       specifies a descriptive label for the data.}
#'     \item{\code{replace}}{An optional \code{logical}. When
#'       set to TRUE, you can replace an existing in-memory
#'       table with the same name in the same caslib. The
#'       default value is FALSE.}
#'     \item{\code{promote}}{An optional \code{logical}. When
#'       set to TRUE, the in-memory table has global scope and
#'       can be available to other CAS sessions (subject to
#'       access controls). The default value is FALSE and
#'       the in-memory table has session scope so that it is
#'       accessible with the session that uploaded the table
#'       only. Session-scope tables are ideal for data analysis.
#'       Global-scope tables are better suited for reporting.}
#'     \item{\code{replication}}{An optional \code{numeric} that
#'       specifies the number of redundant copies of in-memory
#'       blocks. This parameter applies to distributed servers
#'       only. The default value is 1.}
#'    }
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' myCasTable <- cas.read.table(s, "/path/to/data.tsv",
#'   header = TRUE,
#'   sep = "\\t", casOut = list(name = "mycastable")
#' )
#' }
#'
#' @export
setMethod(
  "cas.read.table",
  signature(x = "CAS"),
  function(x, file, ..., casOut = "") {
    return(as.CASTable(x, read.table(file, ...), casOut = .casout(casOut, file)))
  }
)

setGeneric("cas.read.sas7bdat",
  function(x, file, ..., casOut = "") {
    standardGeneric("cas.read.sas7bdat")
  }
)

#' Upload a SAS Data Set to a CAS Table
#'
#' This function transfers a SAS data set (.sas7bdat)
#' file from \R (the client) to the CAS server. The
#' server imports the data and \R returns a CASTable
#' object to reference the in-memory table in CAS.
#'
#' @param x An instance of a CAS object that represents
#'   a connection and CAS session.
#' @param file An \code{character} string that specifies
#'   the SAS data set (.sas7bdat file).
#' @param \dots Parameters sent to the \code{table.upload} action.
#' @param casOut An optional \code{character} or list. If
#'   you specify a string, then the string is used as the
#'   in-memory table name. A list can be used to specify
#'   properties for the in-memory table as follows:
#'   \describe{
#'     \item{\code{name}}{An optional \code{character} that
#'       specifies the name for the in-memory table. By
#'       default, the name of the data frame is used.}
#'     \item{\code{caslib}}{An optional \code{character} that
#'       specifies the caslib. Specify this parameter to
#'       override the active caslib.}
#'     \item{\code{label}}{An optional \code{character} that
#'       specifies a descriptive label for the data.}
#'     \item{\code{replace}}{An optional \code{logical}. When
#'       set to TRUE, you can replace an existing in-memory
#'       table with the same name in the same caslib. The
#'       default value is FALSE.}
#'     \item{\code{promote}}{An optional \code{logical}. When
#'       set to TRUE, the in-memory table has global scope and
#'       can be available to other CAS sessions (subject to
#'       access controls). The default value is FALSE and
#'       the in-memory table has session scope so that it is
#'       accessible with the session that uploaded the table
#'       only. Session-scope tables are ideal for data analysis.
#'       Global-scope tables are better suited for reporting.}
#'     \item{\code{replication}}{An optional \code{numeric} that
#'       specifies the number of redundant copies of in-memory
#'       blocks. This parameter applies to distributed servers
#'       only. The default value is 1.}
#'    }
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' gold_medals <- cas.read.sas7bdat(s, "/path/to/gold_medals.sas7bdat")
#' }
#'
#' @export
setMethod(
  "cas.read.sas7bdat",
  signature(x = "CAS"),
  function(x, file, ..., casOut = "") {
    res <- x$upload(file, ..., casout = .casout(casOut, file), stop.on.error = TRUE)
    return(CASTable(x, res$results$tableName, caslib = res$results$caslib))
  }
)

setGeneric("cas.read.jmp",
  function(x, file, ..., casOut = "") {
    standardGeneric("cas.read.jmp")
  }
)

#' Upload a JMP File to a CAS Table
#'
#' This function transfers a JMP file (.jmp) from \R
#' (the client) to the CAS server. The
#' server imports the data and \R returns a CASTable
#' object to reference the in-memory table in CAS.
#'
#' @param x An instance of a CAS object that represents
#'   a connection and CAS session.
#' @param file An \code{character} string that specifies
#'   the JMP file.
#' @param \dots Parameters sent to the \code{table.upload} action.
#' @param casOut An optional \code{character} or list. If
#'   you specify a string, then the string is used as the
#'   in-memory table name. A list can be used to specify
#'   properties for the in-memory table as follows:
#'   \describe{
#'     \item{\code{name}}{An optional \code{character} that
#'       specifies the name for the in-memory table. By
#'       default, the name of the data frame is used.}
#'     \item{\code{caslib}}{An optional \code{character} that
#'       specifies the caslib. Specify this parameter to
#'       override the active caslib.}
#'     \item{\code{label}}{An optional \code{character} that
#'       specifies a descriptive label for the data.}
#'     \item{\code{replace}}{An optional \code{logical}. When
#'       set to TRUE, you can replace an existing in-memory
#'       table with the same name in the same caslib. The
#'       default value is FALSE.}
#'     \item{\code{promote}}{An optional \code{logical}. When
#'       set to TRUE, the in-memory table has global scope and
#'       can be available to other CAS sessions (subject to
#'       access controls). The default value is FALSE and
#'       the in-memory table has session scope so that it is
#'       accessible with the session that uploaded the table
#'       only. Session-scope tables are ideal for data analysis.
#'       Global-scope tables are better suited for reporting.}
#'     \item{\code{replication}}{An optional \code{numeric} that
#'       specifies the number of redundant copies of in-memory
#'       blocks. This parameter applies to distributed servers
#'       only. The default value is 1.}
#'    }
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' spring_example <- cas.read.jmp(s, "/path/to/Spring\\ Example.jmp",
#'                                casOut = list(name = "spring_example"))
#' }
#'
#' @export
setMethod(
  "cas.read.jmp",
  signature(x = "CAS"),
  function(x, file, ..., casOut = "") {
    res <- x$upload(file, ..., casout = .casout(casOut, file), stop.on.error = TRUE)
    return(CASTable(x, res$results$tableName, caslib = res$results$caslib))
  }
)

