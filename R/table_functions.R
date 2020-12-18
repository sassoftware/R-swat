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


#' Return the First Part of a CAS Table
#'
#' Returns the first part of an in-memory table that is referenced
#' by a \code{\link{CASTable}} object.
#'
#' @section Note:
#' The \code{head} function is not deterministic between reloads of data
#' or if nodes or added or removed from a distributed server.
#'
#' @param x \code{\link{CASTable}} object.
#' @param n An optional positive integer that specifies the number of
#'   rows to return.
#'
#' @return A \code{\link{casDataFrame}} object with the first n rows.
#'
#' @examples
#' \dontrun{
#' head(ct1)
#' head(ct[1:4], 10)
#' }
#'
#' @export
setMethod(
  "head",
  signature(x = "CASTable"),
  function(x, n = 6L) {
    tp <- .gen_table_param(x)
    fv <- c(tp$vars, tp$computedVars)
    fv <- fv[fv != ""]
    if (sum(nchar(x@XcomputedVars))) {
      for (Xcmp in x@XcomputedVars) {
        if (!(Xcmp %in% x@computedVars)) {
          fv <- fv[fv != Xcmp]
        }
      }
    }

    if (length(tp$orderby)) {
      res <- casRetrieve(x@conn, "table.fetch", table = tp, fetchVars = fv,
                         index = FALSE, to = n, from = 1, sortby = tp$orderby)
    } else {
      res <- casRetrieve(x@conn, "table.fetch", table = tp, fetchVars = fv,
                         index = FALSE, to = n, from = 1)
    }
    .check_for_cas_errors(res)
    rownames(res$results$Fetch) <- 1:min(nrow(res$results$Fetch), n)
    return(res$results$Fetch)
  }
)

#' Return the Last Part of a CAS Table
#'
#' Returns the last part of an in-memory table that is referenced
#' by a \code{\link{CASTable}} object.
#'
#' @section Note:
#' The \code{tail} function is not deterministic between reloads of data
#' or if nodes or added or removed from a distributed server.
#'
#' @param x \code{\link{CASTable}} object.
#' @param n An optional positive integer that specifies the number of
#'   rows to return.
#'
#' @return A \code{\link{casDataFrame}} object with the last n rows.
#'
#' @examples
#' \dontrun{
#' tail(ct1)
#' tail(ct[1:4], 10)
#' }
#'
#' @export
setMethod(
  "tail",
  signature(x = "CASTable"),
  function(x, n = 6L) {
    # get number of rows
    r <- nrow(x)
    tp <- .gen_table_param(x)
    tp <- .gen_table_param(x)
    fv <- c(tp$vars, tp$computedVars)
    fv <- fv[fv != ""]
    if (sum(nchar(x@XcomputedVars))) {
      for (Xcmp in x@XcomputedVars) {
        if (!(Xcmp %in% x@computedVars)) {
          fv <- fv[fv != Xcmp]
        }
      }
    }

    if (length(tp$orderby)) {
      res <- casRetrieve(x@conn, "table.fetch", table = tp, fetchVars = fv,
                         index = FALSE, to = r, from = r - n + 1, sortby = tp$orderby)
    } else {
      res <- casRetrieve(x@conn, "table.fetch", table = tp, fetchVars = fv,
                         index = FALSE, to = r, from = r - n + 1)
    }

    n <- min(n, dim(res$results$Fetch)[1])
    rownames(res$results$Fetch) <- (r - n + 1):r
    return(res$results$Fetch)
  }
)

#' Return a Subset of Rows and Columns from a CAS Table
#'
#' Return a subset of rows and columns from a \code{\link{CASTable}} that meet
#' subsetting criteria.
#'
#' @param x      \code{\link{CASTable}} object.
#' @param subset Logical expression indicating elements or rows to keep.
#' @param select Expression indicating columns to select from CAS table.
#' @param drop   Passed to the \code{[} indexing operator.
#' @param \ldots Further arguments to be passed to or from other methods.
#'
#' @return A CASTable object with the rows and columns that meet the subset criteria.
#'
#' @examples
#' \dontrun{
#' subset(ct, subset = ct[4] > 15, select = c("n1", "n4", "s"), drop = FALSE)
#' subset(ct, subset = ct$n4 > 15, select = c(1, 4, 5), drop = FALSE)
#' }
#'
#' @export
subset.CASTable <- function(x, subset, select = NULL, drop = FALSE, ...) {
  if (is.null(select)) {
    vars <- c(x@names, x@computedVars)
    vars <- vars[vars != ""]
    select <- vars
  }
  out <- x[select, drop = drop, ...]
  if (!missing(subset)) {
    out@where <- CASwhere(x, deparse(substitute(subset)))
  }
  return(out)
}

#' Return a Subset of Rows and Columns from a CAS Table
#'
#' Return a subset of rows and columns from a \code{\link{CASTable}} that meet
#' subsetting criteria.
#'
#' @param x      \code{\link{CASTable}} object.
#' @param subset Logical expression indicating elements or rows to keep.
#' @param select Expression indicating columns to select from CAS table.
#' @param drop   Passed to the \code{[} indexing operator.
#' @param \ldots Further arguments to be passed to or from other methods.
#'
#' @return A CASTable object with the rows and columns that meet the subset criteria.
#'
#' @examples
#' \dontrun{
#' subset(ct, subset = ct[4] > 15, select = c("n1", "n4", "s"), drop = FALSE)
#' subset(ct, subset = ct$n4 > 15, select = c(1, 4, 5), drop = FALSE)
#' }
#'
#' @export
subset.casTable <- function(x, subset, select = NULL, drop = FALSE, ...) {
    message("This function has been deprecated. Use subset.CASTable instead.")
    return(subset.CASTable(x, subset, select = select, drop = drop, ...))
}

#' @inherit subset.CASTable
#' @export
setMethod("subset.data.frame", "CASTable", subset.CASTable)

#' @inherit subset.CASTable
#' @export
setMethod("subset", "CASTable", subset.CASTable)

#' Extract Unique Values from a CAS Table
#'
#' Extracts distinct values from columns in a \code{\link{CASTable}}.
#'
#' @param x             \code{\link{CASTable}} object.
#' @param incomparables A vector of values that cannot be compared.
#'                      See the help for base::unique.
#' @param \dots         Arguments that are passed to method arguments.
#'
#' @return A \code{\link{casDataFrame}} object.
#'
#' @examples
#' \dontrun{
#' unique(ct[4:5])
#' unique(ct$s)
#' unique(ct[4])
#' }
#'
#' @export
setMethod(
  "unique", signature(x = "CASTable"),
  function(x, incomparables = FALSE, ...) {
    if (class(x) != "CASTable") {
      stop("The first parameter must be a CASTable object")
    }

    if (sum(nchar(x@computedVars)) > 0) {
      stop("Not currently capable of finding unique rows when there are computedVars defined.")
      tp <- .gen_table_param(x)
      tp$computedOnDemand <- TRUE
      tmpname <- .unique_table_name("unique_tmp1")
      tmp1 <- cas.table.partition(x@conn, casout = tmpname, table = tp)
      tmp1 <- defCasTable(x@conn, tmpname)
      vars <- c(x@names, x@computedVars)
      vars <- vars[vars != ""]
      delete <- TRUE
    }
    else {
      vars <- x@names
      tmp1 <- x
      delete <- FALSE
    }

    cols <- paste('"', vars[1], '"', sep = "")
    if (length(vars) > 1) {
      for (i in 2:length(vars)) {
        cols <- paste(cols, ',"', vars[i], '"', sep = "")
      }
    }

    tname <- paste('"', tmp1@tname, '"', sep = "")
    q <- paste(" select distinct ", cols, " from ", tname, ";")
    res <- casRetrieve(x@conn, "fedSql.execDirect", query = q)

    if (delete) {
      dropTable(tmp1)
    }

    .check_for_cas_errors(res)
    if (ncol(res$results$"Result Set") == 1) {
      return(unlist(res$results$"Result Set", use.names = FALSE))
    }
    else {
      return(res$results$"Result Set")
    }
  }
)

#' Combine CAS Tables by Rows
#'
#' This is the implementation of \code{rbind} for in-memory tables.
#'
#' @param x     \code{\link{CASTable}}.
#' @param y     \code{\link{CASTable}}.
#' @param \dots Arguments that are passed to method arguments.
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' rbind(ct1, ct2)
#' rbind(ct1, ct2, ct3)
#' }
#'
#' @export
rbind2.casTable <- function(x, y, ...) {
  if (!class(x) == "CASTable") {
    stop("The first parameter must be a CASTable object")
  }
  if (!class(y) == "CASTable") {
    stop("The second parameter must be a CASTable object")
  }
  table_name <- .unique_table_name("_rbind")
  code <- paste("data ", table_name, "(caslib='", x@caslib, "') ", "; ",
                "set ", x@tname, "(caslib='", x@caslib, "') ",
                y@tname, "(caslib='", y@caslib, "'); run;", sep = "")
  .run_sas_code(x@conn, code = code)
  # return new CASTable
  return(defCasTable(x@conn, table_name, x@caslib))
}

#' Combine CAS Tables by Columns
#'
#' @rdname rbind2.casTable
#' @export
setMethod("rbind2", "CASTable", rbind2.casTable)

#' Combine CAS Tables by Columns
#'
#' @param \dots          Arguments that are passed to method arguments.
#' @param deparse.level  See the help for base::rbind.
#'
#' @export
rbind.casTable <- function(..., deparse.level = 1){
  stop("This function must take two or more casTables")
}

#' Combine CAS Tables by Rows
#'
#' This is the implementation of rbind for in-memory tables.
#'
#' @param \dots          Arguments that are passed to method arguments.
#' @param deparse.level  See the help for base::rbind.
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' rbind(ct1, ct2)
#' rbind(ct1, ct2, ct3)
#' }
#'
#' @export
setMethod("rbind", "CASTable", rbind.casTable)

#' Combine CAS Tables by Columns
#'
#' This is the implementation of \code{cbind} for in-memory tables.
#'
#' @param x     \code{\link{CASTable}}.
#' @param y     \code{\link{CASTable}}.
#' @param \dots Arguments that are passed to method arguments.
#'
#' @return \code{\link{CASTable}}
#'
#' @examples
#' \dontrun{
#' cbind2(ct1, ct2)
#' cbind2(ct1[1:3], ct2$X1)
#' }
#'
#' @export
cbind2.casTable <- function(x, y, ...) {
  if (!class(x) == "CASTable") {
    stop("The parameter must be a CASTable object")
  }
  if (!class(y) == "CASTable") {
    stop("The parameter must be a CASTable object")
  }

  x_info <- casRetrieve(x@conn, "table.columnInfo",
                        table = list(name = x@tname, caslib = x@caslib))
  .check_for_cas_errors(x_info)
  y_info <- casRetrieve(y@conn, "table.columnInfo",
                        table = list(name = y@tname, caslib = y@caslib))
  .check_for_cas_errors(y_info)

  x_cols <- tolower(x_info$results$ColumnInfo$Column)
  y_cols <- tolower(y_info$results$ColumnInfo$Column)

  if (length(intersect(x_cols, y_cols)) > 0) {
    stop("Column names in tables must have unique names")
  }

  table_name <- .unique_table_name("_cbind")

  code <- paste("data '", table_name, "'n(caslib='", x@caslib, "'); ",
    "  merge '", x@tname, "'n(caslib='", x@caslib, "') ",
    "        '", y@tname, "'n(caslib='", y@caslib, "'); ",
    "run;",
    sep = ""
  )

  .run_sas_code(x@conn, code = code)

  # return new CASTable
  return(defCasTable(x@conn, table_name, x@caslib))
}

#' Combine CAS Tables by Columns
#'
#' @inherit cbind2.casTable
#'
#' @export
setMethod("cbind2", "CASTable", cbind2.casTable)

#' Combine CAS Tables by Columns
#'
#' @param \dots          Arguments that are passed to method arguments.
#' @param deparse.level  See the help for base::rbind.
#'
#' @export
cbind.casTable <- function(..., deparse.level = 1){
  stop("This function must take two or more casTables")
}

#' Combine CAS Tables by Columns
#'
#' @inherit cbind.casTable
#'
#' @export
setMethod("cbind", "CASTable", cbind.casTable)
