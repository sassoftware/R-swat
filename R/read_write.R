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


#' Write a CAS Table to a CSV File
#' 
#' This function downloads an in-memory table from the
#' CAS server and saves it as a CSV file that is accesible
#' to \R (the client). This function is a convenience
#' wrapper for the \R \code{write.csv} function.
#'
#' This function saves the file on the \R client. As an
#' alternative, you can use the \code{cas.table.save}
#' generated function to save a server-side CSV file.
#'
#' @param CASTable The instance of the CASTable to save as
#'   as a CSV file.
#' @param file An \code{character} string that 
#'   specifies the filename for the CSV file. If you do not
#'   specify the file, then the in-memory table name is used
#'   with a .csv suffix. This value is passed to \code{write.csv}.
#' @param append An optional \code{logical} value This value
#'   is passed to \code{write.csv}.
#' @param quote An optional \code{logical} value or numeric
#'   vector. This value is passed to \code{write.csv}. 
#' @param sep An optional \code{character} that is used to separate
#'   values in the CSV file. This value is passed to \code{write.csv}.
#' @param eol An optional \code{character} string that is used as the
#'   end-of-line character or characters. This value is passed to 
#'   \code{write.csv}.
#' @param na An optional \code{character} string to represent
#'   missing values. This value is passed to \code{write.csv}.
#' @param dec An optional \code{character} to represent the decimal
#'   separator. This value is passed to \code{write.csv}.
#' @param row.names An optional \code{logical} value or a 
#'   \code{character} vector of row names. This value is passed to
#'   \code{write.csv}.
#' @param col.names An optional \code{logical} value or a
#'   \code{character} vector of column names. This value is passed
#'   to \code{write.csv}.
#' @param qmethod An optional \code{chracter} string that describes
#'   how to write embedded quotation marks. This value is passed
#'   to \code{write.csv}.
#' @param fileEncoding An optional \code{character} string that
#'   specifies the encoding to use for writing the CSV file. This
#'   value is passed to \code{write.csv}.
#'
#' @family functions for saving in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' # upload a SAS data set to an in-memory table
#' gold_medals <- cas.read.sas7bdat(s, "/path/to/gold_medals.sas7bdat")
#'
#' # download the in-memory table as a CSV file
#' cas.write.csv(gold_medals, "~/gold_medals.csv")
#'
#' }
cas.write.csv <-
  function(CASTable,
           file = "",
           quote = TRUE,
           eol = "\n",
           na = "NA",
           row.names = TRUE,
           fileEncoding = "") {
    if (nchar(file) == 0) {
      file = paste(CASTable@tname, ".csv", sep = '')
    }
    write.csv(
      to.casDataFrame(CASTable)@df,
      file = file,
      quote = quote,
      eol = eol,
      na = na,
      row.names = row.names,
      fileEncoding = fileEncoding
    )
  }


#' Write a CAS Table to a CSV File
#' 
#' This function is identical to \code{\link{cas.write.csv}}
#' except that it wraps the \R \code{write.csv2}  function. 
#' The \code{write.csv2} function uses a comma 
#' for the decimal separator and a semicolon for the field
#' delimiter.
#'
#' @export
#' @rawRd % Copyright SAS Institute
#'
cas.write.csv2 <-
  function(CASTable,
           file = "",
           quote = TRUE,
           eol = "\n",
           na = "NA",
           row.names = TRUE,
           fileEncoding = "") {
    if (nchar(file) == 0) {
      file = paste(CASTable@tname, ".csv", sep = '')
    }
    write.csv2(
      to.casDataFrame(CASTable)@df,
      file = file,
      quote = quote,
      eol = eol,
      na = na,
      row.names = row.names,
      fileEncoding = fileEncoding
    )
  }


#' Write a CAS Table to a Microsoft Excel Workbook
#' 
#' This function downloads an in-memory table from the
#' CAS server and saves it as an XLSX file that is accesible
#' to \R (the client). This function is a convenience
#' wrapper for the \R \code{write.xslx} function.
#'
#' @param CASTable The instance of the CASTable to save as
#'   as a CSV file.
#' @param file An \code{character} string that 
#'   specifies the filename for the XLSX file. If you do not
#'   specify the file, then the in-memory table name is used
#'   with an .xslx suffix. This value is passed to \code{write.xlsx}.
#' @param sheetName An optional \code{character} string that 
#'   specifies the sheet name in the workbook. This value is
#'   passed to \code{write.xlsx}.
#' @param col.names An optional \code{logical} value that specifies
#'   whether column names are included in the workbook. This value
#'   is passed to \code{write.xlsx}.
#' @param row.names An optional \code{logical} value that specifies
#'   whether row names are included in the workbook. This value
#'   is passed to \code{write.xlsx}.
#' @param append An optional \code{logical} value. This value
#'   is passed to \code{write.xlsx}.
#' @param showNA An optional \code{logical} value. This value
#'   is passed to \code{write.xlsx}.
#'
#' @family functions for saving in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' cas.write.xlsx(myCasTable, file="/path/to/data_out.xlsx")
#' }
cas.write.xlsx <- function(CASTable,
                           file = "",
                           sheetName = "Sheet1",
                           col.names = TRUE,
                           row.names = TRUE,
                           append = FALSE,
                           showNA = TRUE) {
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    stop("xlsx is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (nchar(file) == 0) {
    file = paste(CASTable@tname, ".xlsx", sep='')
  }
  write.xlsx(
    to.casDataFrame(CASTable),
    file = file,
    sheetName = sheetName,
    col.names = col.names,
    row.names = row.names,
    append = append,
    showNA = showNA
  )
}


#' Write a CAS Table to RDS
#'
#' This function downloads an in-memory table from the
#' CAS server and saves it as an RDS file that is accessible
#' to \R (the client). This function is a convenience
#' wrapper for the \R \code{saveRDS} function.
#'
#' @param CASTable The instance of the CASTable to save as
#'   as an RDS file.
#' @param file An \code{character} string that 
#'   specifies the filename for the RDS file. If you do not
#'   specify the file, then the in-memory table name is used
#'   with an .rds suffix. This value is passed to \code{saveRDS}.
#' @param ascii An optional \code{logical} value. When set to
#'   TRUE or NA, then an ASCII representation is written.
#'   Otherwise, a binary is written.  This value is passed to
#'   \code{saveRDS}.
#' @param version An optional \code{numeric} value. This value
#'   is passed to \code{saveRDS}.
#' @param compress An optional \code{logical} value. When set
#'   to one of TRUE, "gzip", "bzip2", or "xz", then compression is
#'   used. TRUE performs gzip. This value is passed to \code{saveRDS}.
#' @param refhook An optional value that is passed to
#'   \code{saveRDS}.
#'
#' @family functions for saving in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' cas.saveRDS(myCasTable, file="/path/to/data_out.rds")
#' }

cas.saveRDS <- function(CASTable, 
                        file = "", 
                        ascii = FALSE, 
                        version = NULL, 
                        compress = TRUE,
                        refhook = NULL) {
  if (nchar(file) == 0) {
    file = paste(CASTable@tname, ".rds", sep='')
  }
  saveRDS(to.casDataFrame(CASTable)@df, 
          file = file, 
          ascii = ascii, 
          version = version,
          compress = compress,
          refhook=refhook)
  
}


#' Write a CAS Table to a Table
#'
#' This function downloads an in-memory table from the
#' CAS server and saves it as a file that is accesible
#' to \R (the client). This function is a convenience
#' wrapper for the \R \code{write.table} function.
#'
#' @param CASTable The instance of the CASTable to save as
#'   as a file.
#' @param file An \code{character} string that 
#'   specifies the filename for the file. If you do not
#'   specify the file, then the table is printed to the 
#'   terminal.
#' @param append An optional \code{logical} value. This value
#'   is passed to \code{write.table}.
#' @param quote An optional \code{logical} value or numeric
#'   vector. This value is passed to \code{write.table}. 
#' @param sep An optional \code{character} that is used to separate
#'   values in the file. This value is passed to \code{write.table}.
#' @param eol An optional \code{character} string that is used as the
#'   end-of-line character or characters. This value is passed to 
#'   \code{write.table}.
#' @param na An optional \code{character} string to represent
#'   missing values. This value is passed to \code{write.table}.
#' @param dec An optional \code{character} to represent the decimal
#'   separator. This value is passed to \code{write.table}.
#' @param row.names An optional \code{logical} value or a 
#'   \code{character} vector of row names. This value is passed to
#'   \code{write.table}.
#' @param col.names An optional \code{logical} value or a
#'   \code{character} vector of column names. This value is passed
#'   to \code{write.table}.
#' @param qmethod An optional \code{chracter} string that describes
#'   how to write embedded quotation marks. This value is passed
#'   to \code{write.table}.
#' @param fileEncoding An optional \code{character} string that
#'   specifies the encoding to use for writing the file. This
#'   value is passed to \code{write.table}.
#'
#' @family functions for saving in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' # 
#' cas.write.table(myCasTable, file="/path/to/data_out.txt", na="")
#' }
cas.write.table <- function(CASTable, 
                            file = "", 
                            append = FALSE, 
                            quote = TRUE, 
                            sep = " ",
                            eol = "\n", 
                            na = "NA", 
                            dec = ".", 
                            row.names = TRUE, 
                            col.names = TRUE,
                            qmethod = c("escape", "double"), 
                            fileEncoding = "") {
  write.table(to.casDataFrame(CASTable)@df, 
              file = file, 
              append = append, 
              quote = quote, 
              sep = sep,
              eol = eol, 
              na = na, 
              dec = dec, 
              row.names = row.names, 
              col.names = col.names,
              qmethod = qmethod, 
              fileEncoding = fileEncoding )
}

#' Read a CSV File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for 
#' the \R \code{read.csv} and \code{as.casTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in 
#' CAS (the server).
#'
#' @param conn An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that specifies
#'   the filename or connection for the data to read.
#' @param header An optional \code{logical} that specifies
#'   whether the first line of the file contains variable
#'   names.
#' @param sep A \code{character} that specifies the field
#'   delimiter. This value is passed to \code{read.csv}.
#' @param quote A \code{character} string that specifies
#'   the characters that enclose character data type
#'   variables. This value is passed to \code{read.csv}.
#' @param dec An optional \code{character} to represent the decimal
#'   separator. This value is passed to \code{read.csv}.
#' @param fill An optional \code{logical} value. When set to
#'   TRUE, blank fields are implicitly added for rows that have
#'   unequal length. This value is passed to \code{read.csv}.
#' @param comment.char An optional \code{character} that 
#'   specifies the character to interpret as the beginning of a
#'   comment. This value is passed to \code{read.csv}.
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
#' @param \dots Optional parameters that are passed to
#'   \code{read.csv}.
#'
#' @return \code{\link{CASTable}}
#' @family functions for loading in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' # Upload a CSV, the in-memory table is named HEART
#' heartct <- cas.read.csv(s, "http://support.sas.com/documentation/
#'   onlinedoc/viya/exampledatasets/heart.csv")
#'
#' # Upload the same CSV, name the in-memory table HEARTCT
#' heartct <- cas.read.csv(s, "http://support.sas.com/documentation/
#'   onlinedoc/viya/exampledatasets/heart.csv",
#'   casOut=list(name="heartct", replace=TRUE))
#' }
cas.read.csv <- function(conn,
                         file,
                         header = TRUE,
                         sep = ",",
                         quote = "\"",
                         dec = ".",
                         fill = TRUE,
                         comment.char = "",
                         casOut = list(name='', replace=FALSE),
                         ...) {
  
  if (nchar(file) == 0) {
    stop("You must provide a valid file name")
  }
  df <-
    read.csv(
      file = file,
      header = header,
      sep = sep,
      quote = quote,
      dec = dec,
      fill = fill,
      comment.char = comment.char,
      ...
    )
  if (is.character(casOut)) {
    cn <- casOut
    casOut <- list(name=cn, replace=FALSE)
  }
  if (is.null(casOut$name) || nchar(casOut$name)==0) {
    casOut$name = tools::file_path_sans_ext(basename(file))
  }
  return (as.casTable(conn, df, casOut = casOut))
}

#' Read an XLSX File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for
#' the \R \code{read.xlsx} and \code{as.casTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in 
#' CAS (the server).
#'
#' @param conn An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that 
#'   specifies the filename for the XLSX file.
#'   This value is passed to \code{read.xslx}.
#' @param sheetIndex An optional \code{numeric} that specifies
#'   the sheet in the workbook. This value is passed to
#'   \code{read.xlsx}.
#' @param sheetName An optional \code{character} string that 
#'   specifies the sheet name in the workbook. This value is
#'   passed to \code{read.xlsx}.
#' @param rowIndex An optional \code{numeric} vector that
#'   specifies the rows to read from the workbook. By default,
#'   all rows are read. This value is passed to \code{read.xlsx}.
#' @param startRow An optional \code{numeric} that specifies
#'   the first row of the workbook to read. This value is
#'   ignored of rowIndex is specified. This value is passed to
#'   \code{read.xlsx}.
#' @param endRow An optional \code{numeric} that specifies the
#'   last row of the workbook to read. This value is ignored if
#'   rowIndex is specified. This value is passed to \code{read.xlsx}.
#' @param colIndex An optional \code{numeric} vector that specifies
#'   the variables to read from the workbook. By default,
#'   all variables are read. This value is passed to \code{read.xlsx}.
#' @param as.data.frame An optional \code{logical} value that 
#'   specifies whether the data should be coerced into a data frame.
#'   This value is passed to \code{read.xlsx}.
#' @param header An optional \code{logical} that specifies
#'   whether the first line of the file contains variable
#'   names.
#' @param colClasses An optional \code{character} vector that
#'   specifies the classes for the columns. This value is passed
#'   to \code{read.xlsx}.
#' @param keepFormulas An optional \code{logical} value that specifies
#'   whether Excel formulas are included as text or if they are 
#'   evaluated and the result is read as data. This value is passed
#'   to \code{read.xlsx}.
#' @param encoding An optional \code{character} string that specifies
#'   the encoding for character data. This value is passed to 
#'   \code{read.xlsx}.
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
#' @family functions for loading in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' myCasTable <- cas.read.xlsx(s, file="/path/to/data_out.xlsx", 
#'   sheetIndex = 1, 
#'   casOut=list(name="mycastable", replace=TRUE))
#' }
cas.read.xlsx <- function(conn,
                          file,
                          sheetIndex = 1,
                          sheetName = NULL,
                          rowIndex = NULL,
                          startRow = NULL,
                          endRow = NULL,
                          colIndex = NULL,
                          as.data.frame = TRUE,
                          header = TRUE,
                          colClasses = NA,
                          keepFormulas = FALSE,
                          encoding = "unknown",
                          casOut = list(name='', replace=FALSE)) {
  if (nchar(file) == 0) {
    stop("You must provide a valid file name")
  }
  if (is.character(casOut)) {
    cn <- casOut
    casOut <- list(name=cn, replace=FALSE)
  }
  if (is.null(casOut$name) || nchar(casOut$name)==0) {
    casOut$name = tools::file_path_sans_ext(basename(file))
  }
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    stop("xlsx is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  df <- read.xlsx(
    file = file,
    sheetIndex = sheetIndex,
    sheetName = sheetName,
    rowIndex = rowIndex,
    startRow = startRow,
    endRow = endRow,
    colIndex = colIndex,
    as.data.frame = as.data.frame,
    header = header,
    colClasses = colClasses,
    keepFormulas = keepFormulas,
    encoding = encoding
  )
  
  return(as.casTable(conn, df, casOut = casOut))
}

#' Read an RDS File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for
#' the \R \code{readRDS} and \code{as.casTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in 
#' CAS (the server).
#'
#' @param conn An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that 
#'   specifies the filename for the RDS file.
#'   This value is passed to \code{readRDS}.
#' @param refhook An optional value that is passed to
#'   \code{readRDS}.
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
#' @family functions for loading in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' myCasTable <- cas.readRDS(s, file="/path/to/data_out.rds", 
#'   casOut=list(name="mycastable"))
#' }
cas.readRDS <- function(conn, file, refhook = NULL, casOut = list(name='', replace=FALSE)){
  if (nchar(file) == 0) {
    stop("You must provide a valid file name")
  }
  
  df <- readRDS(file = file, refhook = refhook)
  
  if (is.null(casOut$name) || nchar(casOut$name)==0) {
    casOut$name = tools::file_path_sans_ext(basename(file))
  }
  return(as.casTable(conn, df, casOut = casOut))
}


#' Read a File and Upload to a CAS Table
#'
#' This function is a convenience wrapper for
#' the \R \code{read.table} and \code{as.casTable} functions.
#' After reading the file that is accessible to the \R
#' client, it is uploaded to an in-memory table in 
#' CAS (the server).
#'
#' @param conn An instance of a CAS object that represents
#'  a connection and CAS session.
#' @param file An \code{character} string that 
#'   specifies the filename. This value is passed to \code{read.table}.
#' @param header An optional \code{logical} that specifies
#'   whether the first line of the file contains variable
#'   names.
#' @param sep An optional \code{character} that is used to specify
#'   the field delimiter for the file. This value is passed to 
#'   \code{write.table}.
#' @param quote An optional \code{character} string that specifies
#'   the characters that enclose character data. The value is 
#'   passed to \code{read.table}.
#' @param dec An optional \code{character} that specifies the decimal
#'   separator. This value is passed to \code{read.table}.
#' @param numerals An optional \code{character} string that specifies
#'   how to interpret numbers that can lose precision due to the
#'   conversion from text to double precision. This value is passed
#'   to \code{read.table}.
#' @param row.names An optional \code{character} vector of row
#'   names. This value is passed to \code{read.table}.
#' @param col.names An optional \code{character} vector of names 
#'   for the variables. The default is to use "V" followed by the 
#'   column number. This value is passed to \code{read.table}.
#' @param as.is An optional vector of \code{logical}, \code{numeric},
#'   or \code{character} indices that specify the columns that are not
#'   converted to factors. This value is passed to \code{read.table}.
#' @param na.strings An optional \code{character} vector that specifies
#'   the characters to interpret as NA. This value is passed to
#'   \code{read.table}.
#' @param colClasses An optional \code{character} vector that
#'   specifies the classes for the columns. This value is passed
#'   to \code{read.table}.
#' @param nrows An optional \code{numeric} that specifies the maximum
#'   number of rows to read. This value is passed to \code{read.table}.
#' @param skip An optional \code{numeric} that specifies the number of
#'   lines to skip in the file before reading data. This value is
#'   passed to \code{read.table}.
#' @param check.names An optional \code{logical} that specifies
#'   whether variable names from the file are valid names. This value
#'   is passed to \code{read.table}.
#' @param fill An optional \code{logical} value. When set to
#'   TRUE, blank fields are implicitly added for rows that have
#'   unequal length. This value is passed to \code{read.table}.
#' @param strip.white An optional \code{logical} that specifies
#'   whether white space characters are stripped from character data
#'   that are not enclosed with quotation marks. This value is 
#'   ignored unless sep is specified. This value is passed to 
#'   \code{read.table}.
#' @param blank.lines.skip An optional \code{logical} that specifies
#'   whether blank lines in the file are ignored. This value is
#'   passed to \code{read.table}.
#' @param comment.char An optional \code{character} that 
#'   specifies the character to interpret as the beginning of a
#'   comment. This value is passed to \code{read.table}.
#' @param allowEscapes An optional \code{logical} that specifies
#'   whether C-style escape characters such as \code{\\n} are
#'   interpreted or are read verbatim. This value is passed to
#'   \code{read.table}.
#' @param flush An optional \code{logical} that specifies whether
#'   input that follows the last field to read is flushed. Setting
#'   this argument to TRUE enables adding comments to the end of
#'   data lines. This value is passed to \code{read.table}.
#' @param stringsAsFactors An optional \code{logical} that specifies
#'   whether character vectors are converted to factors. This argument
#'   is overridden by as.is and colClasses. This value is passed to
#'   \code{read.table}.
#' @param fileEncoding An optional \code{character} string that
#'   specifies the encoding to use for reading the file. This
#'   value is passed to \code{read.table}.
#' @param encoding An optional \code{character} string that specifies
#'   the encoding for character data. This value is passed to 
#'   \code{read.table}.
#' @param text An optional \code{character} string. This value is
#'   passed to \code{read.table}.
#' @param skipNul An optional \code{logical} that is passed to
#'   \code{read.table}.
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
#' @family functions for loading in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' myCasTable <- cas.read.table(s, "/path/to/data.tsv", header=TRUE,
#'                              sep="\t", casOut=list(name="mycastable"))
#' }
cas.read.table <- function (conn, file, header = FALSE, sep = "", quote = "\"'", dec = ".",
                            numerals = c("allow.loss", "warn.loss", "no.loss"), row.names,
                            col.names, as.is = !stringsAsFactors, na.strings = "NA",
                            colClasses = NA, nrows = -1, skip = 0, check.names = TRUE,
                            fill = !blank.lines.skip, strip.white = FALSE, blank.lines.skip = TRUE,
                            comment.char = "#", allowEscapes = FALSE, flush = FALSE,
                            stringsAsFactors = default.stringsAsFactors(), fileEncoding = "",
                            encoding = "unknown", text, skipNul = FALSE, 
                            casOut = list(name='', replace=FALSE)
) {
  
  if (nchar(file) == 0) {
    stop("You must provide a valid file name")
  }
  
  df <- read.table(file, header = header, sep = sep, quote = quote,
                   dec = dec, numerals = numerals,
                   row.names=row.names, col.names=col.names, as.is = as.is,
                   na.strings = na.strings, colClasses = colClasses, nrows = nrows,
                   skip = skip, check.names = check.names, fill = fill,
                   strip.white = strip.white, blank.lines.skip = blank.lines.skip,
                   comment.char = comment.char,
                   allowEscapes = allowEscapes, flush = flush,
                   stringsAsFactors = stringsAsFactors,
                   fileEncoding = fileEncoding, encoding = encoding, text=text, skipNul = skipNul)
  
  if (is.character(casOut)) {
    cn <- casOut
    casOut <- list(name=cn, replace=FALSE)
  }
  if (is.null(casOut$name) || nchar(casOut$name)==0) {
    casOut$name = tools::file_path_sans_ext(basename(file))
  }
  return(as.casTable(conn, df, casOut = casOut))
}

#' Upload a SAS Data Set to a CAS Table
#'
#' This function transfers a SAS data set (.sas7bdat)
#' file from \R (the client) to the CAS server. The 
#' server imports the data and \R returns a CASTable
#' object to reference the in-memory table in CAS.
#'
#' @param conn An instance of a CAS object that represents
#'   a connection and CAS session.
#' @param file An \code{character} string that specifies 
#'   the SAS data set (.sas7bdat file).
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
#' @family functions for loading in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' gold_medals <- cas.read.sas7bdat(s, "/path/to/gold_medals.sas7bdat")
#' }
cas.read.sas7bdat <- function(conn, file, casOut = list(name='', replace=FALSE) ) {
  
  if (nchar(file) == 0) {
    stop("You must provide a valid file name")
  }
  if (is.character(casOut)) {
    cn <- casOut
    casOut <- list(name=cn, replace=FALSE)
  }
  if (is.null(casOut$name) || nchar(casOut$name)==0) {
    casOut$name = tools::file_path_sans_ext(basename(file))
  } 
  
  res <- conn$upload(file, casout=casOut)
  check_for_cas_errors(res, stop.on.error=FALSE)
  if ( !is.null(res$results$tableName) )
  {
     return(swat::defCasTable(conn, res$results$tableName, caslib=res$results$caslib))
  }
  else
  {
     return(swat::defCasTable(conn, casOut$name, caslib=casOut$caslib))
  }
}

#' Upload a JMP File to a CAS Table
#'
#' This function transfers a JMP file (.jmp) from \R
#' (the client) to the CAS server. The 
#' server imports the data and \R returns a CASTable
#' object to reference the in-memory table in CAS.
#'
#' @param conn An instance of a CAS object that represents
#'   a connection and CAS session.
#' @param file An \code{character} string that specifies 
#'   the JMP file.
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
#' @family functions for loading in-memory data
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' spring_example <- cas.read.jmp(s, "/path/to/Spring\ Example.jmp", 
#'                                casOut=list(name="spring_example"))
#' }
cas.read.jmp <- function(conn, file, casOut=NULL){
  
  if (nchar(file) == 0) {
    stop("You must provide a valid file name")
  }
  if (is.character(casOut)) {
    cn <- casOut
    casOut <- list(name=cn, replace=FALSE)
  }
  
  tablename = tools::file_path_sans_ext(basename(file))
  if (is.null(casOut)){
    casOut=list(name=tablename)
  } 
  
  res <- conn$upload(file, casout=casOut)
  check_for_cas_errors(res, stop.on.error=FALSE)
  if ( !is.null(res$results$tableName) )
  {
     return(swat::defCasTable(conn, res$results$tableName, caslib=res$results$caslib))
  }
  else
  {
     return(swat::defCasTable(conn, casOut$name, caslib=casOut$caslib))
  }
}
