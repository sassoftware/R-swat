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
#' @seealso \code{as.CASTable}
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
    names = "character",
    .computedVarsProgram = "character",
    .computedVars = "character",
    .compcomp = "logical"
  )
)

setMethod("initialize", "CASTable", function(.Object, conn, tname, caslib = "",
                                             columns = character(0),
                                             where = "", orderby = list(),
                                             groupby = list(), gbmode = "",
                                             computedOnDemand = FALSE, computedVars = "",
                                             computedVarsProgram = "") {
  if (class(conn) != "CAS") {
    stop("The first parameter must be a CAS object")
  }
  if (class(tname) != "character") {
    stop("The table name must be character")
  }
  if (tname[1] == "") {
    stop("The table name can not be empty")
  }

  .Object@conn <- conn
  .Object@tname <- tname
  .Object@caslib <- caslib
  .Object@where <- where
  .Object@orderby <- orderby
  .Object@groupby <- groupby
  .Object@gbmode <- gbmode
  .Object@computedOnDemand <- computedOnDemand
  .Object@computedVars <- computedVars
  .Object@computedVarsProgram <- computedVarsProgram
  .Object@.computedVarsProgram <- ""
  .Object@.computedVars <- ""
  .Object@.compcomp <- FALSE

  if (is.null(columns)) {
    .Object@names <- ""
  } 
  else if (length(columns) == 0) {
    tab <- list(name = tname)
    if (caslib != "") {
      tab <- c(tab, caslib = caslib)
    }
    res <- cas.retrieve(conn, "table.columnInfo", table = tab)
    .check_for_cas_errors(res)
    .Object@names <- res$results$ColumnInfo$Column
  }
  else {
    .Object@names <- columns
  }

  .Object
})

setGeneric("as.CASTable",
  function (x, frame, casOut = "") {
    standardGeneric("as.CASTable")
  }
)

#' Upload an Object to a CAS Table
#'
#' Uploads an \R data frame to CAS and returns a
#' \code{\link{CASTable}} object. The CASTable object
#' is a reference in \R (the client) to the in-memory
#' table that is in CAS (the server).
#'
#' @param conn A \code{\link{CAS}} object that represents
#'   a connection and session in CAS.
#' @param frame A \code{data.frame} object with the data to
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
#' irisct <- as.CASTable(s, iris)
#'
#' # Specify a name for the in-memory table.
#' mtcarsct <- as.CASTable(s, mtcars, casOut = "mtcarsct")
#'
#' # Avoid replacing an existing in-memory table.
#' mtcarsct <- as.CASTable(s, mtcars, casOut = list(name = "mtcarsct", replace = FALSE))
#' }
#'
#' @export
setMethod(
  "as.CASTable",
  signature(x = "CAS"),
  function(x, frame, casOut = "") {
    caslib <- ""

    if (nchar(casOut[1]) == 0) {
      tablename <- deparse(substitute(frame))
      casOut <- list(name = tablename)
    }
    else if (typeof(casOut) == "character") {
      tablename <- casOut
    } else {
      if (typeof(casOut) == "list") {
        if (length(casOut$name)) {
          tablename <- casOut$name
        } else {
          tablename <- deparse(substitute(frame))
          casOut$name <- tablename
        }
        if (length(casOut$caslib)) {
          caslib <- casOut$caslib
        }
      }
      else {
        stop("casOut parameter must either be a list or single string")
      }
    }

    return(cas.upload.frame(x, frame, casOut = casOut,
      "_messagelevel" = as.character(getOption("cas.message.level.ui")),
      "_apptag" = "UI", stop.on.error = TRUE
    ))
  }
)

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
        where <- i@.computedVarsProgram
        xcompvars <- i@.computedVars
      }
      else {
        where <- .cas_where(x, deparse(substitute(i)))
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

    vars <- vars[vars != ""]
    if (length(vars) == 0) {
      vars <- NULL
    }

    ret <- new(
      "CASTable", x@conn, x@tname, x@caslib, vars,
      where, x@orderby, x@groupby, x@gbmode, FALSE, compvars, compvpgm
    )
    ret@.computedVars <- xcompvars
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
          if (sum(nchar(value@.computedVarsProgram))) {
            pgm <- paste(colname, " = ", value@.computedVarsProgram, sep = "")
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
    rct@.computedVars <- x@.computedVars
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
        if (sum(nchar(value@.computedVarsProgram))) { # expresion, else col name
          pgm <- paste(name, " = ", value@.computedVarsProgram, sep = "")
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

#' Set names of a CAS Table
#'
#' @param x A CASTable object.
#'
#' @export
setMethod(
  "names<-",
  signature(x = "CASTable"),
  function(x) {
    stop("column names of a CAS table can not be changed")
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
    tp$orderby <- NULL
    tp$groupby <- NULL
    res <- cas.retrieve(x@conn, "simple.numRows", table = tp)
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
    res <- cas.retrieve(x@conn, "simple.numRows", table = tp)
    sapply(1:as.integer(res$results$numrows), toString)
  }
)

setGeneric(
  "drop.table",
  function(x, ...) {
    standardGeneric("drop.table")
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
#' drop.table(ct1)
#' }
#'
#' @export
setMethod(
  "drop.table",
  signature(x = "CASTable"),
  function(x) {
    if (x@caslib == "") {
      res <- cas.retrieve(x@conn, "table.dropTable", table = x@tname)
    } else {
      res <- cas.retrieve(x@conn, "table.dropTable", table = x@tname, caslib = x@caslib)
    }
    return(invisible(res))
  }
)

setGeneric(
  "as.view",
  function(x, ...) {
    standardGeneric("as.view")
  }
)

#' Create a server-side view using the CASTable attributes
#'
#' @param x \code{CASTable} object.
#'
#' @return \code{CASTable} object referencing the view.
#'
#' @export
setMethod(
  "as.view",
  signature(x = "CASTable"),
  function(x) {
    tp <- .gen_table_param(x)
    tp$computedOnDemand <- NULL
    v <- cas.table.view(x@conn, name = .unique_table_name("view"), tables = list(tp))
    return(CASTable(x@conn, v$viewName, caslib = v$caslib))
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

#' @export
as.data.frame.CASTable <- function(x, max.rows = getOption("cas.max.download.rows"),
                                   sample = FALSE, sample.seed = 0, ...) {
  drop <- FALSE
  nrows <- nrow(x)

  if (nrows > max.rows && sample) {
    tp <- .gen_table_param(x)
    tp$computedOnDemand <- NULL
    tp$computedVars <- NULL
    tp$computedVarsProgram <- NULL
    tp$.computedVars <- NULL
    tp$.computedVarsProgram <- NULL
    name <- .unique_table_name(x@tname)
    vars <- x@names
    tp$vars <- NULL

    if (!is.null(x@groupby) && length(x@groupby) > 0) {
      cat("Using sampling (stratified) to reduce downloaded rows to", max.rows)
      action.name <- "sampling.stratified"
    }
    else {
      cat("Using sampling (SRS) to reduce downloaded rows to", max.rows)
      action.name <- "sampling.srs"
    }

    cas.retrieve(x@conn, action.name, stop.on.error = TRUE, seed = sample.seed,
                 samppct = (max.rows / nrow(x) * 100), table = tp,
                 output = list(casOut = list(name = name, replace = TRUE),
                               copyVars = vars))$results

    drop <- TRUE

    y <- CASTable(x@conn, name, where = x@where, orderby = x@orderby,
                  groupby = x@groupby, gbmode = x@gbmode, computedVars = x@computedVars,
                  computedVarsProgram = x@computedVarsProgram,
                  computedOnDemand = x@computedOnDemand)
    y@.computedVars <- x@.computedVars
    y@.computedVarsProgram <- x@.computedVarsProgram

    x <- y
  }

  tp <- .gen_table_param(x)
  fv <- c(tp$vars, tp$computedVars)
  fv <- fv[fv != ""]
  if (sum(nchar(x@.computedVars))) {
    for (Xcmp in x@.computedVars) {
      if (!(Xcmp %in% x@computedVars)) {
        fv <- fv[fv != Xcmp]
      }
    }
  }

  if (length(tp$orderby)) {
    res <- cas.retrieve(x@conn, "table.fetch", table = tp, fetchVars = fv, sastypes = FALSE,
                        index = FALSE, from = 1, to = max.rows, maxRows = 10, sortby = tp$orderby)
  } else {
    res <- cas.retrieve(x@conn, "table.fetch", table = tp, fetchVars = fv, sastypes = FALSE,
                        index = FALSE, from = 1, to = max.rows, maxRows = 10)
  }

  if (drop) {
    drop.table(x)
  }

  fetch <- res$results$Fetch

  name <- attributes(fetch)$table.name
  label <- attributes(fetch)$table.label
  title <- attributes(fetch)$table.title
  attrs <- attributes(fetch)$table.attrs
  col.labels <- attributes(fetch)$col.labels
  col.formats <- attributes(fetch)$col.formats
  col.attrs <- attributes(fetch)$col.attrs
  col.sizes <- attributes(fetch)$col.sizes
  col.types <- attributes(fetch)$col.types
  col.widths <- attributes(fetch)$col.widths

  out <- list()
  for (i in seq_len(length(res$results))) {
    if (i == 1) {
      keyname <- "Fetch"
    } else {
      keyname <- paste("Fetch", i - 1, sep = "")
    }
    if (is.null(res$results[keyname])) {
      break
    }
    out[[i]] <- res$results[[keyname]]
  }

  out <- do.call("rbind", out)
  row.names(out) <- NULL

  if (length(list(...)) > 0) {
     out <- as.data.frame(out, ...)
  }

  attributes(out)$table.name <- name
  attributes(out)$table.label <- label
  attributes(out)$table.title <- title
  attributes(out)$table.attrs <- attrs
  attributes(out)$col.labels <- col.labels
  attributes(out)$col.formats <- col.formats
  attributes(out)$col.attrs <- col.attrs
  attributes(out)$col.sizes <- col.sizes
  attributes(out)$col.types <- col.types
  attributes(out)$col.widths <- col.widths

  return(out)
}

#' Convert a CAS Table to a Data Frame (Download)
#'
#' Downloads the in-memory table that is referenced by
#' the CASTable object and stores it as a data.frame
#' in R. This function is used primarily by the package
#' to store the results of a CAS action.
#'
#' @param x   The CAS table data to download.
#' @param obs Optional integer indicating the maximum number of rows
#'   of data to download.
#'
#' @return Returns a data.frame object that contains
#'         a copy of the in-memory data.
#'
#' @examples
#' \dontrun{
#' cdf <- as.data.frame(CASTable)
#' }
#'
#' @export
setMethod(
  "as.data.frame",
  signature(x = "CASTable"),
  as.data.frame.CASTable
)
