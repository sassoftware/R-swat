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

setGeneric("byvars",
  function (x) {
    standardGeneric("byvars")
  }
)

#' Retrieve a vector of the By values
#'
#' @param x data.frame object.
#'
#' @return character vector
#'
#' @export
setMethod(
  "byvars",
  signature(x = "data.frame"),
  function(x) {
    attrs <- attributes(x)$table.attrs
    if (is.null(attrs) || is.null(attrs$ByGroup)) {
      return(character())
    }

    vars <- character()
    table.attrs <- attributes(x)$table.attrs

    i <- 1
    while (TRUE) {
      byvar <- paste("ByVar", i, sep = "")
      if (is.null(table.attrs[[byvar]])) {
        break
      }
      vars[i] <- table.attrs[[byvar]]
      i <- i + 1
    }

    return(vars)
  }
)

setGeneric("byvals",
  function (x) {
    standardGeneric("byvals")
  }
)

#' Retrieve a vector of the By values
#'
#' @param x data.frame object.
#'
#' @return list()
#'
#' @export
setMethod(
  "byvals",
  signature(x = "data.frame"),
  function(x) {
    attrs <- attributes(x)$table.attrs
    if (is.null(attrs) || is.null(attrs$ByGroup)) {
      return(list())
    }

    vals <- list()
    table.attrs <- attributes(x)$table.attrs

    i <- 1
    while (TRUE) {
      byval <- paste("ByVar", i, "Value", sep = "")
      if (is.null(table.attrs[[byval]])) {
        break
      }
      vals[i] <- table.attrs[[byval]]
      i <- i + 1
    }

    return(vals)
  }
)

setGeneric("bygroups.as.columns",
  function (x) {
    standardGeneric("bygroups.as.columns")
  }
)

#' Return data.frame with By group values prepended as columns
#'
#' @param x data.frame object.
#'
#' @return data.frame
#'
#' @export
setMethod(
  "bygroups.as.columns",
  signature(x = "data.frame"),
  function(x) {
    attrs <- attributes(x)$table.attrs
    if (is.null(attrs) || is.null(attrs$ByGroup)) {
      return(x)
    }

    names <- colnames(x)
    bynames <- byvars(x)
    table.attrs <- attributes(x)$table.attrs

    i <- 1
    while (TRUE) {
      byvar <- paste("ByVar", i, sep = "")
      byval <- paste("ByVar", i, "Value", sep = "")
      if (is.null(table.attrs[[byvar]])) {
        break
      }
      x[[table.attrs[[byvar]]]] <- table.attrs[[byval]]
      i <- i + 1
    }

    return(x[, c(bynames, names)])
  }
)

setGeneric("rbind.bygroups",
  function (x) {
    standardGeneric("rbind.bygroups")
  }
)

#' Combine data.frames belonging to the same By Group set into one data.frame
#'
#' @param x List containing CAS action results.
#'
#' @return List of By Group sets with a single data.frame in each field.
#'
#' @export
setMethod(
  "rbind.bygroups",
  signature(x = "list"),
  function(x) {
    if (is.null(x[["ByGroupInfo"]]) && is.null(x[["ByGroupSet1.ByGroupInfo"]])) {
      return(x)
    }

    tables <- list()

    for (name in names(x)) {
      if (grepl("^(ByGroupSet\\d+\\.)?ByGroup\\d+\\.", name, perl = TRUE)) {
        tblname <- gsub("^(ByGroupSet\\d+\\.)?ByGroup\\d+\\.", "\\1", name, perl = TRUE)
        if (is.null(tables[[tblname]])) {
          tables[[tblname]] <- list()
        }
        tables[[tblname]][[length(tables[[tblname]]) + 1]] <- x[[name]]
      }
    }

    for (name in names(tables)) {
      tables[[name]] <- do.call(rbind, tables[[name]])
    }

    return(tables)
  }
)
