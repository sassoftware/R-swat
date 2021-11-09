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
#' @slot name  An optional \code{character} string that specifies CAS metadata 
#'   for a table name.
#' @slot label An optional \code{character} string that specifies CAS metadata 
#'   for a table label.
#' @slot title An optional \code{character} string that specifies CAS metadata 
#'   for a table title.
#' @slot attrs An optional \code{list} of key-value pairs the specify 
#'   user-defined CAS metadata.
#' @slot col.labels  An optional \code{character} string that specifies CAS 
#'   metadata for column labels.
#' @slot col.formats An optional \code{character} string that specifies CAS 
#'   metadata for column formats.
#' @slot col.attrs   An optional \code{list} that specifies CAS metadata for
#'   column attributes.
#' @slot col.sizes   An optional \code{list} that specifies CAS metadata for
#'   the number of bytes in the widest row.
#' @slot col.types   An optional \code{character} that specifies CAS metadata
#'   for the column data types.
#' @slot col.widths An optional \code{numeric} that specifies CAS metadata
#'   for column widths.
#' @slot df    The data.frame to encapsulate in the casDataFrame.
#'
#' @export
#' @rawRd % Copyright SAS Institute
setClass(
  "casDataFrame",
  slots = list(
               name        = "character",
               label       = "character",
               title       = "character",
               attrs       = "list",
               col.labels  = "character",
               col.formats = "character",
               col.attrs   = "list",
               col.sizes   = "list",
               col.types   = "character",
               col.widths  = "numeric",
               df          = 'data.frame'),
  contains = "data.frame"
)

# CAS Data Frame Initialize Method
#' @keywords internal
#
#' @export
#' @rawRd % Copyright SAS Institute
setMethod("initialize", "casDataFrame", function(.Object,
                                                  dframe = '',
                                                  ...,
                                                  name = '',
                                                  label = '',
                                                  title = '',
                                                  attrs = list(),
                                                  col.labels = '',
                                                  col.formats = '',
                                                  col.attrs = list(),
                                                  col.sizes = list(),
                                                  col.types = '',
                                                  col.widths = 0
                                                  ) {
  .Object@name        <- name
  .Object@label       <- label
  .Object@title       <- title
  .Object@attrs       <- attrs
  .Object@col.labels  <- col.labels
  .Object@col.formats <- col.formats
  .Object@col.attrs   <- col.attrs
  .Object@col.sizes   <- col.sizes
  .Object@col.types   <- col.types
  .Object@col.widths  <- col.widths

  if (class(dframe) == 'data.frame' | class(dframe)=='casDataFrame')
    .Object@df        <- dframe
  else
    .Object@df        <- data.frame(...)

  .Object@.Data     <- .Object@df@.Data
  .Object@names     <- names(.Object@df)
  .Object@row.names <- row.names(.Object@df)

  if (class(dframe)=='casDataFrame') {
     .Object@name  <- dframe@name
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
#' @docType methods
#'
#' @param object A \code{casDataFrame} object.
#'
#' @return data frame listing
#' @export
#' @rawRd % Copyright SAS Institute
setMethod("show",
          signature(object = "casDataFrame"),
          function(object) {
            show(object@df)
          })

#' Convert an \R Data Frame to a CAS Data Frame
#'
#' This function is rarely used for programming. It is
#' used by the package to associate CAS metadata with 
#' tabular data that is returned by CAS actions.
#'
#' @inheritParams casDataFrame
#'
#' @return casDataFrame
#' @export
#' @rawRd % Copyright SAS Institute
#' @examples
#' \dontrun{
#' cdf2 = as.casDataFrame(df3[1:4])
#' cdf = as.casDataFrame(iris)
#' }
as.casDataFrame <-  function(df, name = '', label = '', title = '',
                                 attrs = list(), col.labels = '', col.formats = '',
                                 col.attrs = list(), col.sizes = list(), col.types = '',
                                 col.widths = 0) {
  if (class(df) != 'data.frame')
  {
    stop("The first parameter must be a data.frame object")
  }
  new("casDataFrame", df, name = name, label = label, title = title,
                           attrs = attrs, col.labels = col.labels, col.formats = col.formats,
                           col.attrs = col.attrs, col.sizes = col.sizes, col.types = col.types,
                           col.widths = col.widths)
}

#' @rdname casDataFrame-class
#'
#' @inheritParams casDataFrame
#'
#' @seealso \code{casDataFrame}
#'
#' @return casDataFrame
#' @export
casDataFrame <- function(..., name = '', label = '', title = '',
                               attrs = list(), col.labels = '', col.formats = '',
                               col.attrs = list(), col.sizes = list(), col.types = '',
                               col.widths = 0) {
  new("casDataFrame", ..., name = name, label = label, title = title,
                            attrs = attrs, col.labels = col.labels, col.formats = col.formats,
                            col.attrs = col.attrs, col.sizes = col.sizes, col.types = col.types,
                            col.widths = col.widths)
}

#' Convert a CAS Table to a CAS Data Frame (Download)
#'
#' Downloads the in-memory table that is referenced by
#' the CASTable object and stores it as a casDataFrame
#' in R. This function is used primarily by the package
#' to store the results of a CAS action.
#'
#' @param ct The CASTable object to download.
#'
#' @return Returns a casDataFrame object that contains
#'         a copy of the in-memory data.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cdf = to.casDataFrame(CASTable)
#' }
to.casDataFrame <-  function(ct, obs=32768) {
  if (class(ct) != 'CASTable') {
    stop("The first parameter must be a CASTable object")
  }

  tp = gen.table.parm(ct)
  fv = c(tp$vars, tp$computedVars)
  fv = fv[fv != ""]
  if (sum(nchar(ct@XcomputedVars)))
     for (Xcmp in ct@XcomputedVars)
        if (! (Xcmp %in% ct@computedVars))
           fv = fv[fv != Xcmp]

  if (length(tp$orderby))
     res <- casRetrieve(ct@conn, 'table.fetch', table=tp, fetchVars=fv, index=FALSE, from=1, to=obs, maxRows=1000, sortby=tp$orderby)
  else
     res <- casRetrieve(ct@conn, 'table.fetch', table=tp, fetchVars=fv, index=FALSE, from=1, to=obs, maxRows=1000)

  fetch <- res$results$Fetch

  name        = fetch@name
  label       = fetch@label
  title       = fetch@title
  attrs       = fetch@attrs
  col.labels  = fetch@col.labels
  col.formats = fetch@col.formats
  col.attrs   = fetch@col.attrs
  col.sizes   = fetch@col.sizes
  col.types   = fetch@col.types
  col.widths  = fetch@col.widths

  out <- list()
  for ( i in 1:length(res$results) ) {
     if ( i == 1 ) {
        keyname <- 'Fetch'
     } else {
        keyname <- paste('Fetch', i-1, sep='')
     }
     if ( is.null(res$results[keyname]) ) {
         break
     }
     out[[i]] <- res$results[[keyname]]
  }


  out <- do.call('rbind', out)
  rownames(out) <- NULL

  return( as.casDataFrame(out, name = name, label = label, title = title,
                          attrs = attrs, col.labels = col.labels, col.formats = col.formats,
                          col.attrs = col.attrs, col.sizes = col.sizes, col.types = col.types,
                          col.widths = col.widths) )
}

#' Convert a CAS data frame to an \R Data Frame
#'
#' This function returns the \R data.frame object that is encapsulated
#' in a casDataFrame. The CAS metadata is not available in the returned
#' data.frame.
#'
#' @param cdf The casDataFrame to convert
#'
#' @return Data Frame
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' df3 = to.data.frame(cdf)
#' }
to.data.frame <-  function(cdf) {
  if (class(cdf) != 'casDataFrame')
  {
    stop("The first parameter must be a casDataFrame object")
    return
  }
  cdf@df
}

# byvars
#
# Retrieve a vector of the By variables
#
# @param casDataFrame
#' @keywords internal
#
# @return character()
#
byvars <- function (df) {
   if ( class(df) != 'casDataFrame' || is.null(df@attrs$ByGroup) ) {
      return( character() )
   }

   attrs <- df@attrs
   names <- colnames(df)
   vars <- character()

   i <- 1
   while ( TRUE ) {
      byvar <- paste('ByVar', i, sep='')
      if ( is.null(attrs[[byvar]]) ) {
         break
      }
      vars <- c(vars, attrs[[byvar]])
      i <- i + 1
   }

   return( vars )
}

# byvals
#
# Retrieve a vector of the By values
#
# @param casDataFrame
#' @keywords internal
#
# @return list()
#
byvals <- function (df) {
   if ( class(df) != 'casDataFrame' || is.null(df@attrs$ByGroup) ) {
      return( list() )
   }

   attrs <- df@attrs
   names <- colnames(df)
   vals <- list()

   i <- 1
   while ( TRUE ) {
      byval <- paste('ByVar', i, 'Value', sep='')
      if ( is.null(attrs[[byval]]) ) {
         break
      }
      vals[i] <- attrs[[byval]]
      i <- i + 1
   }

   return( vals )
}

# Return bygroupsAsColumns
#
# @param casDataFrame
#' @keywords internal
#
# @return data.frame
#
bygroupsAsColumns <- function (df) {
   if ( class(df) != 'casDataFrame' || is.null(df@attrs$ByGroup) ) {
      return( df )
   }

   attrs <- df@attrs
   names <- colnames(df)
   bynames <- byvars(df)

   df <- df@df
   i <- 1
   while ( TRUE ) {
      byvar <- paste('ByVar', i, sep='')
      byval <- paste('ByVar', i, 'Value', sep='')
      if ( is.null(attrs[[byvar]]) ) {
         break
      }
      df[[attrs[[byvar]]]] <- attrs[[byval]]
      i <- i + 1
   }

   return( df[, c(bynames, names)] )
}

#' Convert a CAS Table to a R Data Frame (Download)
#'
#' Downloads the in-memory table that is referenced by
#' the CASTable object and stores it as a casDataFrame
#' in R. This function is used to download datasets from CAS.
#'
#' @param ct The CASTable object to download.
#' @param obs Number of rows to download, by default 32768
#' 
#' @return Returns a casDataFrame object that contains
#'         a copy of the in-memory data.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' rdf = to.r.data.frame(CASTable)
#' }
#' 

to.r.data.frame <-  function(ct, obs=32768) {
  if (class(ct) != 'CASTable') {
    stop("The first parameter must be a CASTable object")
  }
  
  tp = gen.table.parm(ct)
  fv = c(tp$vars, tp$computedVars)
  fv = fv[fv != ""]
  if (sum(nchar(ct@XcomputedVars)))
    for (Xcmp in ct@XcomputedVars)
      if (! (Xcmp %in% ct@computedVars))
        fv = fv[fv != Xcmp]
  
  if (length(tp$orderby))
    res <- casRetrieve(ct@conn, 'table.fetch', table=tp, fetchVars=fv, index=FALSE, from=1, to=obs, maxRows=1000, sortby=tp$orderby)
  else
    res <- casRetrieve(ct@conn, 'table.fetch', table=tp, fetchVars=fv, index=FALSE, from=1, to=obs, maxRows=1000)
  
  fetch <- res$results$Fetch
  
  name        = fetch@name
  label       = fetch@label
  title       = fetch@title
  attrs       = fetch@attrs
  col.labels  = fetch@col.labels
  col.formats = fetch@col.formats
  col.attrs   = fetch@col.attrs
  col.sizes   = fetch@col.sizes
  col.types   = fetch@col.types
  col.widths  = fetch@col.widths
  
  out <- list()
  for ( i in 1:length(res$results) ) {
    if ( i == 1 ) {
      keyname <- 'Fetch'
    } else {
      keyname <- paste('Fetch', i-1, sep='')
    }
    if ( is.null(res$results[keyname]) ) {
      break
    }
    out[[i]] <- res$results[[keyname]]
  }
  
  
  out <- do.call('rbind', out)
  rownames(out) <- NULL
  
  return( out )
}

