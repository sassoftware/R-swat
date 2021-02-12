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

    i <- 1
    while (TRUE) {
      byvar <- paste("ByVar", i, sep = "")
      byval <- paste("ByVar", i, "Value", sep = "")
      if (is.null(attrs[[byvar]])) {
        break
      }
      x[[attrs[[byvar]]]] <- attrs[[byval]]
      i <- i + 1
    }

    return(x[, c(bynames, names)])
  }
)
