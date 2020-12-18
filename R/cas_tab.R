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


#' CAS Table Object
#'
#' CASTable objects contain a reference to a data table loaded in a 
#' CAS server. This reference can also include filtering applied by
#' an expression, as well as additional computed columns. The filtering
#' and additional columns are only applied when using this object to
#' access data in the server-side table. In this way, CASTable objects
#' can be used like a client-side view of the table.
#'
#' @param conn A \code{\link{CAS}} object that represents the connection and
#'   session on the server.
#' @param tname An optional \code{character} string for the table name.
#' @param caslib An optional \code{character} string that identifies the caslib for the
#'   in-memory table. Specify this parameter to override the active caslib.
#' @param where An optional \code{character} string that specifies a filter for the
#'   rows to process. The filter uses syntax that is specific to SAS.
#' @param orderby An optional \code{list} of column names. Rows are partitioned according
#'   to the columns in the groupby parameter and then ordered according to
#'   the values of the columns specified in this parameter.
#' @param groupby An optional \code{list} of column names. If you specify this parameter
#'   when you load an in-memory table, then the table is partitioned by the
#'   columns. If you specify this parameter when running an action, then
#'   BY-groups are formed temporarily for the duration of the action.
#' @param gbmode An optional \code{character} string. Values are NOSORT (default) or
#'   REDISTRIBUTE. See the CAS product documentation for more information.
#' @param computedOnDemand An optional \code{logical} flag that indicates whether to
#'   the computed variables are created when the table is loaded (False) or to compute
#'   them when an action begins (True).
#' @param computedVars An optional \code{character} string list that identifies the
#'   name and optional information such as a format and label.
#' @param computedVarsProgram An optional \code{character} string list. Specify the
#'   expression to use for computing each of the computed variables.
#'
#' @slot conn A \code{\link{CAS}} object that represents the connection and
#'   session on the server.
#' @slot tname An optional \code{character} string for the table name.
#' @slot caslib An optional \code{character} string that identifies the caslib for the
#'   in-memory table. Specify this parameter to override the active caslib.
#' @slot where An optional \code{character} string that specifies a filter for the
#'   rows to process. The filter uses syntax that is specific to SAS.
#' @slot orderby An optional \code{list} of column names. Rows are partitioned according
#'   to the columns in the groupby parameter and then ordered according to
#'   the values of the columns specified in this parameter.
#' @slot groupby An optional \code{list} of column names. If you specify this parameter
#'   when you load an in-memory table, then the table is partitioned by the
#'   columns. If you specify this parameter when running an action, then
#'   BY-groups are formed temporarily for the duration of the action.
#' @slot gbmode An optional \code{character} string. Values are NOSORT (default) or
#'   REDISTRIBUTE. See the CAS product documentation for more information.
#' @slot computedOnDemand An optional \code{logical} flag that indicates whether to
#'   the computed variables are created when the table is loaded (False) or to compute
#'   them when an action begins (True).
#' @slot computedVars An optional \code{character} string list that identifies the
#'   name and optional information such as a format and label.
#' @slot computedVarsProgram An optional \code{character} string list. Specify the
#'   expression to use for computing each of the computed variables.
#' @slot names An optional \code{list} of column names.
#'
#' @return \code{CASTable}
#'
#' @seealso \code{as.casTable}, \code{defCasTable}
#'
#' @export
CASTable <- setClass("CASTable",
  slots = list(
    conn = "CAS",
    tname = "character",
    caslib = "character",
    where = "character",
    orderby = "ANY",
    groupby = "ANY",
    gbmode = "character",
    computedOnDemand = "logical",
    computedVars = "character",
    computedVarsProgram = "character",
    XcomputedVarsProgram = "character",
    XcomputedVars = "character",
    names = "character",
    compcomp = "logical"
  )
)

setMethod("initialize", "CASTable", function(.Object, conn, tname, caslib, columns,
                                             where = "", orderby = list(),
                                             groupby = list(), gbmode = "",
                                             computedOnDemand = FALSE, computedVars = "",
                                             computedVarsProgram = "") {
  .Object@conn <- conn
  .Object@tname <- tname
  .Object@caslib <- caslib
  .Object@names <- columns
  .Object@where <- where
  .Object@orderby <- orderby
  .Object@groupby <- groupby
  .Object@gbmode <- gbmode
  .Object@computedOnDemand <- computedOnDemand
  .Object@computedVars <- computedVars
  .Object@computedVarsProgram <- computedVarsProgram
  .Object@XcomputedVarsProgram <- ""
  .Object@XcomputedVars <- ""
  .Object@compcomp <- FALSE

  .Object
})

#' Upload an Object to a CAS Table
#'
#' Uploads an \R data frame to CAS and returns a
#' \code{\link{CASTable}} object. The CASTable object
#' is a reference in \R (the client) to the in-memory
#' table that is in CAS (the server).
#'
#' @param conn A \code{\link{CAS}} object that represents
#'   a connection and session in CAS.
#' @param df A \code{data.frame} object with the data to
#'   upload to CAS.
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
#' s <- CAS("cloud.example.com", 5570)
#' irisct <- as.casTable(s, iris)
#'
#' # Specify a name for the in-memory table.
#' mtcarsct <- as.casTable(s, mtcars, casOut = "mtcarsct")
#'
#' # Avoid replacing an existing in-memory table.
#' mtcarsct <- as.casTable(s, mtcars, casOut = list(name = "mtcarsct", replace = FALSE))
#' }
#'
#' @export
as.casTable <- function(conn, df, casOut = "") {
  if (class(conn) != "CAS") {
    stop("The first parameter must be a CAS object")
  }

  caslib <- ""

  if (nchar(casOut[1]) == 0) {
    tablename <- deparse(substitute(df))
    casOut <- list(name = tablename)
  }
  else
  if (typeof(casOut) == "character") {
    tablename <- casOut
  } else {
    if (typeof(casOut) == "list") {
      if (length(casOut$name)) {
        tablename <- casOut$name
      } else {
        tablename <- deparse(substitute(df))
        casOut$name <- tablename
      }

      if (length(casOut$caslib)) {
        caslib <- casOut$caslib
      }
    }
    else {
      stop("casOut parameter must either be a list, or just a single string for the table name")
    }
  }

  tblres <- conn$upload(
    casOut = casOut, data = df,
    "_messagelevel" = as.character(getOption("cas.message.level.ui")),
    "_apptag" = "UI"
  )
  if (tblres$disposition$severity > 1) {
    if (grepl("LOADTABLE_EXISTS", tblres$disposition$debug, fixed = TRUE)) {
      stop("table with the same name exists; use casOut=list(replace=TRUE) to overwrite")
    }
    .check_for_cas_errors(tblres)
  }

  if (!is.null(tblres$results$tableName)) {
    tablename <- tblres$results$tableName
    caslib <- tblres$results$caslib
  }

  res <- casRetrieve(conn, "table.columnInfo", table = list(name = tablename, caslib = caslib))
  if (res$disposition$severity > 1) {
    .check_for_cas_errors(res)
  }

  columns <- res$results$ColumnInfo$Column

  new("CASTable", conn, tablename, caslib, columns)
}

#' Create a CASTable object for an existing CAS table
#'
#' Creates a \code{\link{CASTable}} object to reference
#' an existing in-memory table in CAS. You can use this
#' function to reference tables that were loaded by other
#' SAS products, other scripts, or from server-side loads
#' with the cas.table.loadTable function.
#'
#' @param conn A \code{\link{CAS}} object that represents
#'   a connection and session in CAS.
#' @param tablename A \code{character} that specifies the in-memory
#'   table name. You can run the cas.table.tableInfo function to
#'   list the in-memory tables.
#' @param caslib An optional \code{character} string that
#'   identifies the caslib for the in-memory table. Specify
#'   this parameter to override the active caslib.
#' @param columns A \code{list} of column names.
#' @param where   A \code{character} string that specifies a filter for the
#'   rows to process. The filter uses syntax that is specific to SAS.
#' @param orderby A \code{list} of column names. Rows are partitioned according
#'   to the columns in the groupby parameter and then ordered according to
#'   the values of the columns specified in this parameter.
#' @param groupby A \code{list} of column names. If you specify this parameter
#'   when you load an in-memory table, then the table is partitioned by the
#'   columns.  If you specify this parameter when running an action, then
#'   BY-groups are formed temporarily for the duration of the action.
#' @param gbmode  A \code{character} string. Values are NOSORT (default) or
#'   REDISTRIBUTE. See the CAS product documentation for more information.
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' irisct <- as.casTable(s, iris, casOut = "irisct")
#'
#' # Create another CASTable instance to the same in-memory table,
#' # but specify that CAS actions are performed by groups of species.
#' irisct.grouped <- defCasTable(s, tablename = "irisct", groupby = list("species"))
#' }
#'
#' @export
defCasTable <- function(conn, tablename, caslib = "", columns = "", where = "",
                        orderby = list(), groupby = list(), gbmode = "") {
  if (class(conn) != "CAS") {
    stop("The first parameter must be a CAS object")
  }

  if (columns[1] == "") {
    tab <- list(name = tablename)

    if (caslib != "") {
      tab <- c(tab, caslib = caslib)
    }

    res <- casRetrieve(conn, "table.columnInfo", table = tab)
    .check_for_cas_errors(res)
    columns <- res$results$ColumnInfo$Column
  }

  new("CASTable", conn, tablename, caslib, columns, where, orderby, groupby, gbmode)
}

#' Return a CASTable with filtering applied
#'
#' @param x      \code{\link{CASTable}} object.
#' @param i      Row accessor specification.
#' @param j      Column accessor specification.
#' @param \ldots Additional arguments. Currently, not used.
#' @param drop   Logical.
#'
#'
#' @return New CASTable object with filtering options applied.
#'
#' @export
setMethod(
  "[",
  signature(x = "CASTable"),
  function(x, i, j, ...) {
    n <- nargs() - !missing(drop)
    rows <- FALSE

    if (!missing(j)) {
      if (!missing(i)) {
        rows <- TRUE
      } else {
        i <- j
      }
    } else
    if (n > 2) {
      rows <- TRUE
    }

    xcompvars <- ""

    if (rows) {
      if (class(i) == "CASTable") {
        where <- i@XcomputedVarsProgram
        xcompvars <- i@XcomputedVars
      }
      else {
        where <- CASwhere(x, deparse(substitute(i)))
      }

      # No columns passed
      if (missing(j)) {
        if (x@where != "") {
          if (where != "") {
            where <- paste("(", x@where, " AND ", where, ")", sep = "")
          }
          else {
            where <- x@where
          }
        }

        return(new(
          "CASTable", x@conn, x@tname, x@caslib, x@names, where, x@orderby,
          x@groupby, x@gbmode, FALSE, x@computedVars, x@computedVarsProgram
        ))
      }
      else {
        ci <- j
      }
    }
    else {
      ci <- i
      where <- ""
    }

    vars <- ""
    compvars <- ""
    compvpgm <- ""
    nn <- length(x@names[x@names != ""])

    # Numeric list of columns
    if (is.numeric(ci)) {
      neg <- FALSE
      pos <- FALSE

      for (vnum in ci) {
        # Select
        if (vnum > 0) {
          pos <- TRUE
          if (neg) {
            stop("Only 0's may be mixed with negative subscripts")
          }

          if (vnum <= nn) {
            if (length(vars) > 1 || nchar(vars)) {
              vars <- c(vars, x@names[vnum])
            } else {
              vars <- x@names[vnum]
            }
          } else if (length(compvars) > 1 || nchar(compvars)) {
            compvars <- c(compvars, x@computedVars[vnum - nn])
            compvpgm <- c(compvpgm, x@computedVarsProgram[vnum - nn])
          } else {
            compvars <- x@computedVars[vnum - nn]
            compvpgm <- x@computedVarsProgram[vnum - nn]
          }
        }

        # Exclude
        if (vnum < 0) {
          if (!neg) {
            vars <- x@names
            compvars <- x@computedVars
            compvpgm <- x@computedVarsProgram
            dn <- FALSE
            dcv <- FALSE
          }
          neg <- TRUE
          vnum <- abs(vnum)

          if (pos) {
            stop("Only 0's may be mixed with negative subscripts")
          }

          if (vnum <= nn) {
            vars[vnum] <- ""
            dn <- TRUE
          } else {
            compvars[vnum - nn] <- ""
            compvpgm[vnum - nn] <- ""
            dcv <- TRUE
          }
        }
      }

      if (neg) {
        if (dn) {
          vars <- vars[vars != ""]
        }
        if (length(vars[vars != ""]) == 0) {
          x@names <- ""
        }
        if (dcv) {
          compvars <- compvars[compvars != ""]
          compvpgm <- compvpgm[compvpgm != ""]
        }
        if (length(compvars[compvars != ""]) == 0) {
          compvars <- ""
          compvpgm <- ""
        }
      }
    }

    # Named list of columns or CAStable
    else {
      if (class(ci) == "CASTable") {
        ci <- c(ci@names, ci@computedVars)
        ci <- ci[ci != ""]
      }

      for (vname in ci) {
        idx <- match(vname, x@names)
        if (is.na(idx)) {
          idx <- match(vname, x@computedVars)
          if (is.na(idx)) {
            stop("Column name not in existing columns")
          } else {
            if (length(compvars) > 1 || nchar(compvars)) {
              compvars <- c(compvars, vname)
              compvpgm <- c(compvpgm, x@computedVarsProgram[idx])
            }
            else {
              compvars <- vname
              compvpgm <- x@computedVarsProgram[idx]
            }
          }
        } else if (length(vars) > 1 || nchar(vars)) {
          vars <- c(vars, vname)
        } else {
          vars <- vname
        }
      }
    }

    if (sum(nchar(compvars)) || where != "") {
      compvpgm <- x@computedVarsProgram
    } else {
      compvpgm <- ""
    }

    if (x@where != "") {
      if (where != "") {
        where <- paste("(", x@where, " AND ", where, ")", sep = "")
      } else {
        where <- x@where
      }
    }

    ret <- new(
      "CASTable", x@conn, x@tname, x@caslib, vars, where, x@orderby,
      x@groupby, x@gbmode, FALSE, compvars, compvpgm
    )
    ret@XcomputedVars <- xcompvars
    return(ret)
  }
)

#' Subset assignment
#'
#' @param x      \code{\link{CASTable}} object.
#' @param i      Row accessor object.
#' @param j      Column accessor object
#' @param \ldots Additional arguments.
#' @param value  \code{\link{CASTable}} object or character vector.
#'
#' @return \code{\link{CASTable}} object.
#'
#' @aliases [,CASTable-method
#' @export
setMethod(
  "[<-",
  signature(x = "CASTable"),
  function(x, i, j, ..., value) {
    q <- list(...)
    n <- nargs() - !missing(value)
    rows <- FALSE
    if (!missing(j)) {
      if (!missing(i)) {
        rows <- TRUE
      } else {
        i <- j
      }
    } else
    if (n > 2) {
      rows <- TRUE
    }

    if (rows) {
      stop("Row indexing is not supported for CASTable objects")
    }

    # Drop column case
    if ((!missing(value)) && is.null(value[[1]])) {
      dn <- FALSE
      dcv <- FALSE
      nn <- length(x@names[x@names != ""])
      tn <- length(x)

      for (coln in i) {
        if (is.numeric(coln)) {
          if (coln < 1 || coln > tn) {
            stop("Index out of range of existing columns")
          }
          if (coln <= nn) {
            x@names[coln] <- ""
            dn <- TRUE
          } else {
            x@computedVars[coln - nn] <- ""
            dcv <- TRUE
          }
        } else {
          idx <- match(coln, x@names)
          if (is.na(idx)) {
            idx <- match(coln, x@computedVars)
            if (is.na(idx)) {
              stop("Column name not in existing columns")
            }
            x@computedVars[idx] <- ""
            dcv <- TRUE
          } else {
            x@names[idx] <- ""
            dn <- TRUE
          }
        }
      }

      if (dn) {
        x@names <- x@names[x@names != ""]
      }
      if (length(x@names[x@names != ""]) == 0) {
        x@names <- ""
      }
      if (dcv) {
        x@computedVars <- x@computedVars[x@computedVars != ""]
      }
      if (length(x@computedVars[x@computedVars != ""]) == 0) {
        x@computedVars <- ""
      }
    }
    # Add computed column(s) case
    else {
      replace <- FALSE
      for (coln in i) {
        v <- c(x@names, x@computedVars)
        nvars <- length(v[v != ""])
        if (is.numeric(coln)) {
          # Can't replace permanent column
          if (coln <= length(x@names[x@names != ""])) {
            stop(paste(
              paste("Cannot redefine an existing permanent column.",
                    "You can add a new column this way though; use an index value of"),
              toString(nvars + 1, "\n")
            ))
          } else {
            # Replace existing computed column
            if (coln <= nvars) {
              replace <- TRUE
              idx <- coln - length(x@names[x@names != ""])
              colname <- x@computedVars[idx]
            }
            # Create new computed column
            else {
              colname <- paste("_", toString(coln), sep = "")
            }
          }
        } else {
          idx <- match(coln, x@names)
          if (is.na(idx)) {
            idx <- match(coln, x@computedVars)
            if (!(is.na(idx))) { # replace column
              replace <- TRUE
            }
            colname <- coln
          }
          # Can't replace permanent column
          else {
            stop(paste("Cannot redefine an existing column.",
                       "You can add a new column this way though; use an unused column name."))
          }
        }

        # Figure out what the program for this col is
        if (class(value) == "CASTable") {
          # Expresion, else col name
          if (sum(nchar(value@XcomputedVarsProgram))) {
            pgm <- paste(colname, " = ", value@XcomputedVarsProgram, sep = "")
          } else {
            vname <- c(value@names, value@computedVars)
            vname <- vname[vname != ""]
            pgm <- paste(colname, " = ", vname, sep = "")
          }
        } else {
          if (length(i) == 1 &&
            class(value) == "character" &&
            strsplit(value, "=", fixed = TRUE) != value) {
            pgm <- value
          } else if (class(value) == "character") {
            pgm <- paste(colname, " = ", '"', value, '"', sep = "")
          } else {
            pgm <- paste(colname, " = ", as.character(value), sep = "")
          }
        }

        if (!replace) {
          if (sum(nchar(x@computedVars))) {
            x@computedVars <- c(x@computedVars, colname)
          } else {
            x@computedVars <- colname
          }
        }

        if (sum(nchar(x@computedVarsProgram))) {
          x@computedVarsProgram <- c(x@computedVarsProgram, pgm)
        } else {
          x@computedVarsProgram <- c(pgm)
        }
      }
    }

    return(x)
  }
)

#' Return a CASTable with filtering applied
#'
#' @param x \code{\link{CASTable}} object.
#' @param i Numeric or character item accessor.
#'
#' @return \code{\link{CASTable}} object.
#'
#' @export
setMethod(
  "[[",
  signature(x = "CASTable"),
  function(x, i) {
    vars <- ""
    compvars <- ""
    compvpgm <- ""
    nn <- length(x@names[x@names != ""])

    # Numeric list of columns
    if (is.numeric(i)) {
      for (vnum in i) {
        if (vnum <= nn) {
          if (length(vars) > 1 || nchar(vars)) {
            vars <- c(vars, x@names[vnum])
          } else {
            vars <- x@names[vnum]
          }
        } else if (length(compvars) > 1 || nchar(compvars)) {
          compvars <- c(compvars, x@computedVars[vnum - nn])
          compvpgm <- c(compvpgm, x@computedVarsProgram[vnum - nn])
        } else {
          compvars <- x@computedVars[vnum - nn]
          compvpgm <- x@computedVarsProgram[vnum - nn]
        }
      }
    } else {
      if (class(i) == "CASTable") {
        i <- c(i@names, i@computedVars)
        i <- i[i != ""]
      }

      for (vname in i) {
        idx <- match(vname, x@names)
        if (is.na(idx)) {
          idx <- match(vname, x@computedVars)
          if (is.na(idx)) {
            stop("Column name not in existing columns")
          } else {
            if (length(compvars) > 1 || nchar(compvars)) {
              compvars <- c(compvars, vname)
              compvpgm <- c(compvars, x@computedVarsProgram[idx])
            }
            else {
              compvars <- vname
              compvpgm <- x@computedVarsProgram[idx]
            }
          }
        } else if (length(vars) > 1 || nchar(vars)) {
          vars <- c(vars, vname)
        } else {
          vars <- vname
        }
      }
    }

    if (sum(nchar(compvars))) {
      compvpgm <- x@computedVarsProgram
    } else {
      compvpgm <- ""
    }

    rct <- new(
      "CASTable", x@conn, x@tname, x@caslib, vars, x@where, x@orderby,
      x@groupby, x@gbmode, FALSE, compvars, compvpgm
    )
    rct@XcomputedVars <- x@XcomputedVars
    return(rct)
  }
)

#' Access CASTable column by name
#'
#' @param x \code{\link{CASTable}} object.
#' @param name String containing column name.
#'
#' @return \code{\link{CASTable}} with just specified column visible.
#'
#' @export
setMethod(
  "$",
  signature(x = "CASTable"),
  function(x, name) {
    idx <- match(name, x@names)
    if (is.na(idx)) {
      idx <- match(name, x@computedVars)
      if (is.na(idx)) {
        stop("Column name not in existing columns.\n")
      }
      else {
        new(
          "CASTable", x@conn, x@tname, x@caslib, "", x@where, x@orderby,
          x@groupby, x@gbmode, FALSE, name, x@computedVarsProgram
        )
      }
    }
    else {
      new(
        "CASTable", x@conn, x@tname, x@caslib, name, x@where, x@orderby,
        x@groupby, x@gbmode
      )
    }
  }
)

#' Assign data to column by name
#'
#' @param x     \code{\link{CASTable}} object.
#' @param name  String containing column name.
#' @param value Value to assign.
#'
#' @return \code{\link{CASTable}} object.
#'
#' @aliases $,CASTable-method
#' @export
setMethod(
  "$<-",
  signature(x = "CASTable"),
  function(x, name, value) {
    if ((!missing(value)) && is.null(value)) {
      dn <- FALSE
      dcv <- FALSE
      idx <- match(name, x@names)
      if (is.na(idx)) {
        idx <- match(name, x@computedVars)
        if (is.na(idx)) {
          stop("Column name not in existing columns\n")
        }

        x@computedVars[idx] <- ""
        dcv <- TRUE
      }
      else {
        x@names[idx] <- ""
        dn <- TRUE
      }
      if (dn) {
        x@names <- x@names[x@names != ""]
      }
      if (length(x@names[x@names != ""]) == 0) {
        x@names <- ""
      }
      if (dcv) {
        x@computedVars <- x@computedVars[x@computedVars != ""]
      }
      if (length(x@computedVars[x@computedVars != ""]) == 0) {
        x@computedVars <- ""
      }
    }
    # Add computed column(s) case
    else {
      idx <- match(name, x@names)
      if (!is.na(idx)) {
        stop(paste("Cannot redefine an permanent column.",
                   "You can add a new column this way though; use an unused column name."))
      }

      idx <- match(name, x@computedVars)
      # Replace compvar
      if (!is.na(idx)) {
          replace <- TRUE
      }
      # New compvar
      else {
        replace <- FALSE
      }

      # Figure out what the program for this col is
      if (class(value) == "CASTable") {
        if (sum(nchar(value@XcomputedVarsProgram))) { # expresion, else col name
          pgm <- paste(name, " = ", value@XcomputedVarsProgram, sep = "")
        } else {
          vname <- c(value@names, value@computedVars)
          vname <- vname[vname != ""]
          pgm <- paste(name, " = ", vname, sep = "")
        }
      }
      else {
        if (class(value) == "character" &&
          strsplit(value, "=", fixed = TRUE) != value) {
          pgm <- value
        } else
        if (class(value) == "character") {
          pgm <- paste(name, " = ", '"', value, '"', sep = "")
        } else {
          pgm <- paste(name, " = ", as.character(value), sep = "")
        }
      }

      if (!replace) {
        if (sum(nchar(x@computedVars))) {
          x@computedVars <- c(x@computedVars, name)
        } else {
          x@computedVars <- name
        }
      }
      if (sum(nchar(x@computedVarsProgram))) {
        x@computedVarsProgram <- c(x@computedVarsProgram, pgm)
      } else {
        x@computedVarsProgram <- c(pgm)
      }
    }
    return(x)
  }
)

#' Print a sample of the CASTable to the screen
#'
#' @param object \code{\link{CASTable}} object.
#'
#' @export
setMethod(
  "show",
  signature(object = "CASTable"),
  function(object) {
    print(head(object, n = getOption("max.print")))
  }
)

#' Names of a CAS Table
#'
#' Returns the list of column names for the in-memory
#' table that is referenced by the \code{\link{CASTable}} object.
#'
#' @param x A CASTable object.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' names(ct1)
#' }
#'
#' @export
setMethod(
  "names",
  signature(x = "CASTable"),
  function(x) {
    return(colnames(x))
  }
)

#' Dimensions of a CAS Table
#'
#' Returns the number of rows and columns for the in-memory
#' table that is referenced by the \code{\link{CASTable}} object.
#'
#' @param x A CASTable object.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' dim(ct1)
#' }
#'
#' @export
setMethod(
  "dim",
  signature(x = "CASTable"),
  function(x) {
    return(as.integer(c(swat::nrow(x), swat::ncol(x))))
  }
)

#' Number of Rows in a CAS Table
#'
#' Returns the number of rows in an in-memory table
#' that is referenced by the \code{\link{CASTable}} object.
#'
#' @param x A CASTable object.
#'
#' @return scalar
#'
#' @examples
#' \dontrun{
#' nrow(ct1)
#' nrow(ct[1:4])
#' }
#'
#' @export
setMethod(
  "nrow",
  signature(x = "CASTable"),
  function(x) {
    tp <- .gen_table_param(x)
    if (length(x@orderby)) {
      tp$orderby <- NULL
      tp <- tp[tp != ""]
    }
    res <- casRetrieve(x@conn, "simple.numRows", table = tp)
    as.integer(res$results$numrows)
  }
)

#' Number of Columns in a CAS Table
#'
#' Returns the number of columns in an in-memory table
#' that is referenced by the \code{\link{CASTable}} object.
#'
#' @param x A CASTable object.
#'
#' @return scalar
#'
#' @seealso \code{length,CASTable-method}
#'
#' @examples
#' \dontrun{
#' ncol(ct1)
#' ncol(ct[1:4])
#' }
#'
#' @export
setMethod(
  "ncol",
  signature(x = "CASTable"),
  function(x) {
    length(x)
  }
)

#' Number of Columns in a CAS Table
#'
#' Returns the number of columns in an in-memory table
#' that is referenced by the \code{\link{CASTable}} object.
#'
#' @param x A CASTable object.
#'
#' @return scalar
#'
#' @seealso \code{ncol,CASTable-method}
#'
#' @examples
#' \dontrun{
#' length(ct1)
#' length(ct[1:4])
#' }
#'
#' @export
setMethod(
  "length",
  signature(x = "CASTable"),
  function(x) {
    vars <- c(x@names, x@computedVars)
    vars <- vars[vars != ""]
    return(length(vars))
  }
)

#' Column Names in a CAS Table
#'
#' Returns the column names from the in-memory table
#' that is referenced by the \code{\link{CASTable}} object.
#'
#' @section Note:
#' You cannot use this function to set the column names.
#'
#' @param x A CASTable object.
#'
#' @return vector
#'
#' @seealso \code{names,CASTable-method}
#'
#' @examples
#' \dontrun{
#' colnames(ct1)
#' colnames(ct[1:4])
#' }
#'
#' @export
setMethod(
  "colnames",
  signature(x = "CASTable"),
  function(x) {
    vars <- c(x@names, x@computedVars)
    vars <- vars[vars != ""]
    return(vars)
  }
)

#' Dimension Names of a CAS Table
#'
#' @param x A \code{\link{CASTable}} object.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' dimnames(ct1)
#' dimnames(ct[2:4])
#' }
#'
#' @export
setMethod(
  "dimnames",
  signature(x = "CASTable"),
  function(x) {
    r <- rownames(x)
    cn <- c(x@names, x@computedVars)
    cn <- cn[cn != ""]
    list(r, cn)
  }
)

#' Row Names of a CAS Table
#'
#' @param x A \code{\link{CASTable}} object.
#'
#' @return list of strings
#'
#' @examples
#' \dontrun{
#' rownames(ct1)
#' rownames(ct[2:4])
#' }
#'
#' @export
setMethod(
  "rownames",
  signature(x = "CASTable"),
  function(x) {
    tp <- .gen_table_param(x)
    if (length(x@orderby)) {
      tp$orderby <- NULL
      tp <- tp[tp != ""]
    }
    res <- casRetrieve(x@conn, "simple.numRows", table = tp)
    sapply(1:as.integer(res$results$numrows), toString)
  }
)

setGeneric(
  "dropTable",
  function(x) {
      return(invisible(NULL))
  }
)

#' Remove a CAS Table
#'
#' Drops the in-memory table on the server that is
#' referenced by the \code{\link{CASTable}} object.
#'
#' @section Note:
#' This function drops the in-memory table but does
#' not affect the original source file that the
#' in-memory table was loaded from.
#'
#' @param x A CASTable object.
#'
#' @examples
#' \dontrun{
#' dropTable(ct1)
#' }
#'
#' @export
setMethod(
  "dropTable",
  signature(x = "CASTable"),
  function(x) {
    if (class(x) == "CASTable") {
      if (x@caslib == "") {
        res <- casRetrieve(x@conn, "table.dropTable", table = x@tname)
      } else {
        res <- casRetrieve(x@conn, "table.dropTable", table = x@tname, caslib = x@caslib)
      }
      return(invisible(res))
    }
  }
)

#' Test if an object is a CAS Table
#'
#' @param x An \code{R} object.
#'
#' @return boolean
#'
#' @examples
#' \dontrun{
#' is.CASTable(ct1) # TRUE
#' is.CASTable(iris) # FALSE
#' }
#'
#' @export
is.CASTable <- function(x) {
  return(class(x) == "CASTable")
}
