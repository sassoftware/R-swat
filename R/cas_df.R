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


#' CAS Data Frame Class
#'
#' A casDataFrame is a superset of data.frame. The class is used primarily
#' by functions within the package to store tabular data from CAS action
#' results and to associate CAS metadata with the tabular data.
#'
#' A casDataFrame instance is a data object in \R (the client).
#'
#' @param \ldots      Arguments used to construct a data.frame.
#' @param name        Optional \code{character} string that specifies
#'   CAS metadata for a table name.
#' @param label       Optional \code{character} string that specifies
#'   CAS metadata for a table label.
#' @param title       Optional \code{character} string that specifies
#'   CAS metadata for a table title.
#' @param attrs       Optional \code{list} of key-value pairs the specify
#'   user-defined CAS metadata.
#' @param col.labels  Optional \code{character} string that specifies CAS
#'   metadata for column labels.
#' @param col.formats Optional \code{character} string that specifies CAS
#'   metadata for column formats.
#' @param col.attrs   Optional \code{list} that specifies CAS metadata for
#'   column attributes.
#' @param col.sizes   Optional \code{list} that specifies CAS metadata for
#'   the number of bytes in the widest row.
#' @param col.types   Optional \code{character} that specifies CAS metadata
#'   for the column data types.
#' @param col.widths  Optional \code{numeric} that specifies CAS metadata
#'   for column widths.
#'
#' @slot name        \code{character} string that contains the table name.
#' @slot label       \code{character} string that contains the table label.
#' @slot title       \code{character} string that contains the table title.
#' @slot attrs       \code{list} of key-value pairs the specify user-defined
#'   CAS metadata.
#' @slot col.labels  \code{character} vector that contains the column labels.
#' @slot col.formats \code{character} vector that contains the column formats.
#' @slot col.attrs   \code{list} that contains the column attributes.
#' @slot col.sizes   \code{list} that contains the byte-width of the columns.
#' @slot col.types   \code{character} that contains the column data types.
#' @slot col.widths  \code{numeric} that contains the column widths.
#' @slot df          The \code{data.frame} which contains the underlying data.
#'
#' @export
casDataFrame <- setClass(
  "casDataFrame",
  slots = list(
    name = "character",
    label = "character",
    title = "character",
    attrs = "list",
    col.labels = "character",
    col.formats = "character",
    col.attrs = "list",
    col.sizes = "list",
    col.types = "character",
    col.widths = "numeric",
    df = "data.frame"
  ),
  contains = "data.frame"
)

#' @export
setMethod("initialize", "casDataFrame",
          function(.Object, ..., name = "",
                   label = "", title = "", attrs = list(),
                   col.labels = "", col.formats = "",
                   col.attrs = list(), col.sizes = list(),
                   col.types = "", col.widths = 0) {
  .Object@name <- name
  .Object@label <- label
  .Object@title <- title
  .Object@attrs <- attrs
  .Object@col.labels <- col.labels
  .Object@col.formats <- col.formats
  .Object@col.attrs <- col.attrs
  .Object@col.sizes <- col.sizes
  .Object@col.types <- col.types
  .Object@col.widths <- col.widths

  args <- list(...)
  dframe <- ""
  if (length(args) > 0) {
    dframe <- args[[1]]
  }

  if (class(dframe) == "data.frame" | class(dframe) == "casDataFrame") {
    .Object@df <- dframe
  } else {
    .Object@df <- data.frame(...)
  }

  .Object@.Data <- .Object@df@.Data
  .Object@names <- names(.Object@df)
  .Object@row.names <- row.names(.Object@df)

  if (class(dframe) == "casDataFrame") {
    .Object@name <- dframe@name
    .Object@label <- dframe@label
    .Object@title <- dframe@title
    .Object@attrs <- dframe@attrs
    .Object@col.labels <- dframe@col.labels
    .Object@col.formats <- dframe@col.formats
    .Object@col.attrs <- dframe@col.attrs
    .Object@col.sizes <- dframe@col.sizes
    .Object@col.types <- dframe@col.types
    .Object@col.widths <- dframe@col.widths
  }

  .Object
})

#' CAS data frame show method
#'
#' @param object A \code{casDataFrame} object.
#'
#' @return data frame listing
#' @export
setMethod(
  "show",
  signature(object = "casDataFrame"),
  function(object) {
    show(object@df)
  }
)

#' Convert an \R Data Frame to a CAS Data Frame
#'
#' This function is rarely used for programming. It is
#' used by the package to associate CAS metadata with
#' tabular data that is returned by CAS actions.
#'
#' @param name  An optional \code{character} string that specifies CAS metadata
#'   for a table name.
#' @param label An optional \code{character} string that specifies CAS metadata
#'   for a table label.
#' @param title An optional \code{character} string that specifies CAS metadata
#'   for a table title.
#' @param attrs An optional \code{list} of key-value pairs the specify
#'   user-defined CAS metadata.
#' @param col.labels  An optional \code{character} string that specifies CAS
#'   metadata for column labels.
#' @param col.formats An optional \code{character} string that specifies CAS
#'   metadata for column formats.
#' @param col.attrs   An optional \code{list} that specifies CAS metadata for
#'   column attributes.
#' @param col.sizes   An optional \code{list} that specifies CAS metadata for
#'   the number of bytes in the widest row.
#' @param col.types   An optional \code{character} that specifies CAS metadata
#'   for the column data types.
#' @param col.widths An optional \code{numeric} that specifies CAS metadata
#'   for column widths.
#' @param df    The data.frame to encapsulate in the casDataFrame.
#'
#' @return casDataFrame
#'
#' @examples
#' \dontrun{
#' cdf2 <- as.CASDataFrame(df3[1:4])
#' cdf <- as.CASDataFrame(iris)
#' }
#'
#' @export
as.CASDataFrame <- function(df, name = "", label = "", title = "",
                            attrs = list(), col.labels = "", col.formats = "",
                            col.attrs = list(), col.sizes = list(), col.types = "",
                            col.widths = 0) {
  if (class(df) != "data.frame") {
    stop("The first parameter must be a data.frame object")
  }
  return(new("casDataFrame", df,
    name = name, label = label, title = title,
    attrs = attrs, col.labels = col.labels, col.formats = col.formats,
    col.attrs = col.attrs, col.sizes = col.sizes, col.types = col.types,
    col.widths = col.widths
  ))
}

#' Convert an \R Data Frame to a CAS Data Frame
#'
#' This function is rarely used for programming. It is
#' used by the package to associate CAS metadata with
#' tabular data that is returned by CAS actions.
#'
#' @param name  An optional \code{character} string that specifies CAS metadata
#'   for a table name.
#' @param label An optional \code{character} string that specifies CAS metadata
#'   for a table label.
#' @param title An optional \code{character} string that specifies CAS metadata
#'   for a table title.
#' @param attrs An optional \code{list} of key-value pairs the specify
#'   user-defined CAS metadata.
#' @param col.labels  An optional \code{character} string that specifies CAS
#'   metadata for column labels.
#' @param col.formats An optional \code{character} string that specifies CAS
#'   metadata for column formats.
#' @param col.attrs   An optional \code{list} that specifies CAS metadata for
#'   column attributes.
#' @param col.sizes   An optional \code{list} that specifies CAS metadata for
#'   the number of bytes in the widest row.
#' @param col.types   An optional \code{character} that specifies CAS metadata
#'   for the column data types.
#' @param col.widths An optional \code{numeric} that specifies CAS metadata
#'   for column widths.
#' @param df    The data.frame to encapsulate in the casDataFrame.
#'
#' @return casDataFrame
#'
#' @examples
#' \dontrun{
#' cdf2 <- as.CASDataFrame(df3[1:4])
#' cdf <- as.CASDataFrame(iris)
#' }
#'
#' @export
as.casDataFrame <- function(df, name = "", label = "", title = "",
                            attrs = list(), col.labels = "", col.formats = "",
                            col.attrs = list(), col.sizes = list(), col.types = "",
                            col.widths = 0) {
  message("This function has been deprecated. Use as.CASDataFrame instead.")
  return(as.CASDataFrame(df, name = name, label = label, title = title,
                         attrs = attrs, col.labels = col.labels,
                         col.formats = col.formats, col.attrs = col.attrs,
                         col.size = col.sizes, col.types = col.types,
                         col.widths = col.widths))
}

#' Convert a CAS Table to a CAS Data Frame (Download)
#'
#' Downloads the in-memory table that is referenced by
#' the CASTable object and stores it as a casDataFrame
#' in R. This function is used primarily by the package
#' to store the results of a CAS action.
#'
#' @param x   The CAS table data to download.
#' @param obs Optional integer indicating the maximum number of rows
#'   of data to download.
#'
#' @return Returns a casDataFrame object that contains
#'         a copy of the in-memory data.
#'
#' @examples
#' \dontrun{
#' cdf <- to.CASDataFrame(CASTable)
#' }
#'
#' @export
to.CASDataFrame <- function(x, obs = 32768) {
  if (class(x) != "CASTable") {
    stop("The first parameter must be a CASTable object")
  }

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
                       index = FALSE, from = 1, to = obs, maxRows = 10, sortby = tp$orderby)
  } else {
    res <- casRetrieve(x@conn, "table.fetch", table = tp, fetchVars = fv,
                       index = FALSE, from = 1, to = obs, maxRows = 10)
  }

  fetch <- res$results$Fetch

  name <- fetch@name
  label <- fetch@label
  title <- fetch@title
  attrs <- fetch@attrs
  col.labels <- fetch@col.labels
  col.formats <- fetch@col.formats
  col.attrs <- fetch@col.attrs
  col.sizes <- fetch@col.sizes
  col.types <- fetch@col.types
  col.widths <- fetch@col.widths

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
  rownames(out) <- NULL

  return(as.casDataFrame(out,
    name = name, label = label, title = title,
    attrs = attrs, col.labels = col.labels, col.formats = col.formats,
    col.attrs = col.attrs, col.sizes = col.sizes, col.types = col.types,
    col.widths = col.widths
  ))
}

#' Convert a CAS Table to a CAS Data Frame (Download)
#'
#' Downloads the in-memory table that is referenced by
#' the CASTable object and stores it as a casDataFrame
#' in R. This function is used primarily by the package
#' to store the results of a CAS action.
#'
#' @param x   The CAS table data to download.
#' @param obs Optional integer indicating the maximum number of rows
#'   of data to download.
#'
#' @return Returns a casDataFrame object that contains
#'         a copy of the in-memory data.
#'
#' @examples
#' \dontrun{
#' cdf <- to.CASDataFrame(CASTable)
#' }
#'
#' @export
to.casDataFrame <- function(x, obs = 32768) {
  message("This function has been deprecated. Use to.CASDataFrame instead.")
  return(to.CASDataFrame(x, obs = obs))
}

#' Convert a CAS data frame to an \R Data Frame
#'
#' This function returns the \R data.frame object that is encapsulated
#' in a casDataFrame. The CAS metadata is not available in the returned
#' data.frame.
#'
#' @param cdf The casDataFrame to convert
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' df3 <- to.data.frame(cdf)
#' }
#'
#' @export
to.data.frame <- function(cdf) {
  if (class(cdf) != "casDataFrame") {
    stop("The first parameter must be a casDataFrame object")
    return
  }
  cdf@df
}

#' Retrieve a vector of the By variables
#'
#' @param df \code{\link{casDataFrame}} object.
#'
#' @return character()
#'
#' @export
byvars <- function(df) {
  if (class(df) != "casDataFrame" || is.null(df@attrs$ByGroup)) {
    return(character())
  }

  attrs <- df@attrs
  vars <- character()

  i <- 1
  while (TRUE) {
    byvar <- paste("ByVar", i, sep = "")
    if (is.null(attrs[[byvar]])) {
      break
    }
    vars <- c(vars, attrs[[byvar]])
    i <- i + 1
  }

  return(vars)
}

#' Retrieve a vector of the By values
#'
#' @param df \code{\link{casDataFrame}} object.
#'
#' @return list()
#'
#' @export
byvals <- function(df) {
  if (class(df) != "casDataFrame" || is.null(df@attrs$ByGroup)) {
    return(list())
  }

  attrs <- df@attrs
  vals <- list()

  i <- 1
  while (TRUE) {
    byval <- paste("ByVar", i, "Value", sep = "")
    if (is.null(attrs[[byval]])) {
      break
    }
    vals[i] <- attrs[[byval]]
    i <- i + 1
  }

  return(vals)
}

#' Return data.frame with By group values prepended as columns
#'
#' @param df \code{\link{casDataFrame}} object.
#'
#' @return data.frame
#'
#' @export
bygroups.as.columns <- function(df) {
  if (class(df) != "casDataFrame" || is.null(df@attrs$ByGroup)) {
    return(df)
  }

  attrs <- df@attrs
  names <- colnames(df)
  bynames <- byvars(df)

  df <- df@df
  i <- 1
  while (TRUE) {
    byvar <- paste("ByVar", i, sep = "")
    byval <- paste("ByVar", i, "Value", sep = "")
    if (is.null(attrs[[byvar]])) {
      break
    }
    df[[attrs[[byvar]]]] <- attrs[[byval]]
    i <- i + 1
  }

  return(df[, c(bynames, names)])
}
