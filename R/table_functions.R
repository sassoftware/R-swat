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
#' 
#' The \code{head} function is not deterministic between reloads of data
#' or if nodes or added or removed from a distributed server.
#'
#' @param x A CASTable object.
#'
#' @param n An optional positive integer that specifies the number of 
#'   rows to return.
#'
#' @return A \code{\link{casDataFrame}} object with the first n rows.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' head(ct1)
#' head(ct[1:4], 10)
#' }
setMethod("head",
          signature(x = "CASTable"),
          function(x, n = 6L) {
            tp = gen.table.parm(x)
            fv = c(tp$vars, tp$computedVars)
            fv = fv[fv != ""]
            if (sum(nchar(x@XcomputedVars)))
               for (Xcmp in x@XcomputedVars)
                  if (! (Xcmp %in% x@computedVars))
                     fv = fv[fv != Xcmp]

            if (length(x@orderby))
               {
               tp$orderby = NULL
               tp = tp[tp !=""]
               res <- casRetrieve(x@conn, 'table.fetch', table=tp, fetchVars=fv, index = FALSE, to=n, from=1, sortby=x@orderby)
               }
            else
               res <- casRetrieve(x@conn, 'table.fetch', table=tp, fetchVars=fv, index = FALSE, to=n, from=1 )
            check_for_cas_errors(res)
            rownames(res$results$Fetch) <- 1:min(nrow(res$results$Fetch), n)
            return (res$results$Fetch)
          })

#' Return the Last Part of a CAS Table
#'
#' Returns the last part of an in-memory table that is referenced
#' by a \code{\link{CASTable}} object.
#'
#' @section Note:
#' 
#' The \code{tail} function is not deterministic between reloads of data
#' or if nodes or added or removed from a distributed server.
#'
#' @param x A CASTable object.
#'
#' @return A \code{\link{casDataFrame}} object with the last n rows.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' tail(ct1)
#' tail(ct[1:4], 10)
#' }
setMethod("tail",
          signature(x = "CASTable"),
          function(x, n = 6L) {
            # get number of rows
            r <- nrow(x)
            tp = gen.table.parm(x)
            tp = gen.table.parm(x)
            fv = c(tp$vars, tp$computedVars)
            fv = fv[fv != ""]
            if (sum(nchar(x@XcomputedVars)))
               for (Xcmp in x@XcomputedVars)
                  if (! (Xcmp %in% x@computedVars))
                     fv = fv[fv != Xcmp]

            if (length(x@orderby))
               {
               tp$orderby = NULL
               tp = tp[tp !=""]
               res <- casRetrieve(x@conn, 'table.fetch', table=tp, fetchVars=fv, index = FALSE, to=r, from=r-n+1, sortby=x@orderby)
               }
            else
               res <- casRetrieve(x@conn, 'table.fetch', table=tp, fetchVars=fv, index = FALSE, to=r, from=r-n+1)

            n = min(n, dim(res$results$Fetch)[1])
            rownames(res$results$Fetch) <- (r-n+1):r
            return (res$results$Fetch)
          })

#' Return a Subset of Rows and Columns from a CAS Table
#'
#' Return a subset of rows and columns from a \code{\link{CASTable}} that meet
#' subsetting criteria.
#'
#' @param x A CASTable object.
#'
#' @return A CASTable object with the rows and columns that meet the subset criteria.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' subset(ct, subset = ct[4] > 15, select = c("n1", "n4", "s"), drop = FALSE)
#' subset(ct, subset = ct$n4 > 15, select = c(1, 4, 5), drop = FALSE)
#' }
subset.casTable <- function(x, subset, select=NULL, drop = FALSE, ...) {
            if (is.null(select)){
              vars = c(x@names, x@computedVars)
              vars = vars[vars != ""]
              select = vars
            }
            out = x[select, drop = drop, ...]
            if (! missing(subset)) {
              out@where=CASwhere(x, deparse(substitute(subset)))
            }
            return(out)
          }

#' @export
setMethod("subset.data.frame", "CASTable", subset.casTable)

#' @export
setMethod("subset", "CASTable", subset.casTable)

#' Extract Unique Values from a CAS Table
#'
#' Extracts distinct values from columns in a \code{\link{CASTable}}.
#'
#' @param x              A CASTable object.
#' @param incomparables  A vector of values that cannot be compared.
#'                       See the help for base::unique.
#' @param \dots          Arguments that are passed to method arguments.
#'
#' @return A \code{\link{casDataFrame}} object.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' unique(ct[4:5])
#' unique(ct$s)
#' unique(ct[4])
#' }
setMethod("unique", signature(x = "CASTable"), 
          function(x, incomparables = FALSE, ...){
  if (class(x) != 'CASTable') {
    stop("The first parameter must be a CASTable object")
  }
  
  if (sum(nchar(x@computedVars)) > 0)
  {
    stop("Not currently capable of finding unique rows when there are computedVars defined.")
    tp = gen.table.parm(x)
    tp$computedOnDemand = TRUE 
    tmpname = paste('unique_tmp1', random::randomStrings(n = 1, len = 9, unique = TRUE), sep='')
    tmp1 = cas.table.partition(x@conn, casout=tmpname, table =tp)
    tmp1 = defCasTable(x@conn, tmpname)
    vars = c(x@names, x@computedVars)
    vars = vars[vars != ""]
    delete = TRUE
  }
  else
  {
    vars = x@names
    tmp1 = x
    delete = FALSE
  }
  
  cols = paste('"', vars[1], '"', sep='')
  if (length(vars) > 1)
    for (i in 2:length(vars)) 
      cols = paste(cols, ',"', vars[i], '"', sep='')

  tname = paste('"',tmp1@tname,'"', sep='')
  q  <-paste(' select distinct ', cols, ' from ', tname, ';')
  res <- casRetrieve(x@conn, 'fedSql.execDirect', query=q)
  
  if (delete)
    dropTable(tmp1)
  
  check_for_cas_errors(res)
  if (ncol(res$results$'Result Set')==1) {
    return (unlist(res$results$'Result Set', use.names = FALSE))
  }
  else {
    return (res$results$'Result Set')
  }
  
})

#' Combine CAS Tables by Rows
#' 
#' This is the implementation of \code{rbind} for in-memory tables.
#'
#' @param \dots          Arguments that are passed to method arguments.
#' @param deparse.level  See the help for base::rbind.
#'
#' @return \code{\link{CASTable}}
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' rbind(ct1,ct2)
#' rbind(ct1,ct2, ct3)
#' }
rbind2.casTable <- function (x, y, ...) {
            if (! class(y) == 'CASTable') {
              stop("The parameter must be a CAS object")
            }
            if (!requireNamespace("random", quietly = TRUE)) {
              stop(paste0("The 'random' package is required for this function ",
                          "Use install.packages('random') to install the package."),
                   call. = FALSE)
            }
            tableName <- paste("_rbind", random::randomStrings(n = 1, len = 9, unique = TRUE), sep='')
            code <- paste("data", tableName,";", "set", x@tname, y@tname, "; run;")
            runSasCode(x@conn, code=code)
            # return new CASTable
            return (defCasTable(x@conn, tableName))
          }

#' Combine CAS Tables by Columns
#' 
#' @export
#' @rdname rbind2.casTable
setMethod("rbind2", "CASTable", rbind2.casTable)

#' Combine CAS Tables by Columns
#' 
#' @rdname rbind.casTable
#' @export
#' @rawRd % Copyright SAS Institute
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
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' rbind(ct1,ct2)
#' rbind(ct1,ct2, ct3)
#' }
setMethod("rbind", "CASTable", rbind.casTable)

#' Combine CAS Tables by Columns
#'
#' This is the implementation of \code{cbind} for in-memory tables.
#'
#' @param \dots          Arguments that are passed to method arguments.
#' @param deparse.level  See the help for base::cbind.
#'
#' @return \code{\link{CASTable}}
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cbind(ct1, ct2)
#' cbind(ct1[1:3], ct2$X1)
#' }
cbind2.casTable <- function (x, y, ...) {
            if (! class(y) == 'CASTable') {
              stop("The parameter must be a CAS object")
            }
            if (!requireNamespace("random", quietly = TRUE)) {
              stop(paste0("The 'random' package is required for this function ",
                          "Use install.packages('random') to install the package."),
                   call. = FALSE)
            }
            tableName <- paste("_cbind", random::randomStrings(n = 1, len = 9, unique = TRUE), sep='')
            code <- paste("data", tableName,";", "merge", x@tname, y@tname, "; run;")
            runSasCode(x@conn, code=code)
            # return new CASTable
            return (defCasTable(x@conn, tableName))
          }


#' Combine CAS Tables by Columns
#' 
#' @export
#' @rdname cbind2.casTable
setMethod("cbind2", "CASTable", cbind2.casTable)

#' Combine CAS Tables by Columns
#' 
#' @rdname cbind.casTable
#' @export
#' @rawRd % Copyright SAS Institute
cbind.casTable <- function(..., deparse.level = 1){
  stop("This function must take two or more casTables")
}
