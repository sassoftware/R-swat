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
as.data.frame.CASTable <- function(x, obs = 32768) {
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
    res <- cas.retrieve(x@conn, "table.fetch", table = tp, fetchVars = fv,
                        index = FALSE, from = 1, to = obs, maxRows = 10, sortby = tp$orderby)
  } else {
    res <- cas.retrieve(x@conn, "table.fetch", table = tp, fetchVars = fv,
                        index = FALSE, from = 1, to = obs, maxRows = 10)
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

#' Retrieve a vector of the By variables
#'
#' @param df data.frame object.
#'
#' @return character()
#'
#' @export
byvars.data.frame <- function(df) {
  attrs <- attributes(df)$table.attrs
  if (is.null(attrs$ByGroup)) {
    return(character())
  }

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
#' @param df data.frame object.
#'
#' @return list()
#'
#' @export
byvals.data.frame <- function(df) {
  attrs <- attributes(df)$table.attrs
  if (is.null(attrs$ByGroup)) {
    return(list())
  }

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
#' @param df data.frame object.
#'
#' @return data.frame
#'
#' @export
bygroups.as.columns.data.frame <- function(df) {
  attrs <- attributes(df)$table.attrs
  if (is.null(attrs$ByGroup)) {
    return(df)
  }

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
