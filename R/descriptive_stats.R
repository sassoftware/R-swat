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


# CAS descriptive statistics

#' Maximum Values
#'
#' @docType methods
#' 
#' @param x CASTable.
#'
#' @seealso \code{cas.max}
#'
#' @return scalar
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' max(ct1$X1)
#' max(ct1[1:2])
#' }
setMethod("max",
          signature(x = "CASTable"),
          function(x) {
            vars = ""
            tp = swat::gen.table.parm(x)
            vars = c(x@names, x@computedVars)
            vars = vars[vars != ""]
            res <- casRetrieve(x@conn, 'simple.topK', table=tp, inputs=vars, bottomk=0)
            return (max(as.numeric(res$results$Topk$FmtVar)))
          })

#' Minimum Value
#'
#' @docType methods
#' 
#' @param x CASTable.
#'
#' @seealso \code{cas.min}
#'
#' @return scalar
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' min(ct1$X1)
#' min(ct1[1:2])
#' }
setMethod("min",
          signature(x = "CASTable"),
          function(x) {
            vars = ""
            tp = swat::gen.table.parm(x)
            nvars <- swat::numericVarList(x)
            res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('MIN'))
            return (min(res$results$Summary[2]))
          })



#' Mean Value for a Single Column
#'
#' Returns the mean value for the specified column in 
#' the input table.
#'
#' @docType methods
#' 
#' @param x CASTable.
#'
#' @seealso \code{cas.mean}
#'
#' @return scalar
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' mean(ct1$x1)
#' mean(ct1[2])
#' }
setMethod("mean",
          signature(x = "CASTable"),
          function (x, ...) 
          {
            if (ncol(x)==1){
              return(as.numeric(cas.mean(x)[2]))
            }
            else{
              stop("The mean function accepts one column only. Use cas.mean for multple columns.")
            }
          }
)

#' Median Values
#'
#' @docType methods
#' 
#' @param x CASTable.
#'
#' @seealso \code{cas.median}
#'
#' @return scalar
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' median(ct1$x1)
#' median(ct1[1:4])
#' }
setMethod("median",
          signature(x = "CASTable"),
          function(x)
          {
            tp = swat::gen.table.parm(x)
            nvars <- swat::numericVarList(x)
            res <- casRetrieve(x@conn, 'percentile.percentile', table=tp[!names(tp) == "vars"], inputs=nvars, values='50')
            return (as.numeric(res$results$Percentile$Value))
          })

#' Column Sums
#'
#' @docType methods
#' 
#' @param x CASTable.
#'
#' @seealso \code{cas.sum}
#'
#' @return vector
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' colSums(ct[1:4])
#' colSums(ct$X1)
#' }
setMethod("colSums",
          signature(x = "CASTable" ),
          function (x, na.rm = FALSE, dims = 1, ...) {
            tp = swat::gen.table.parm(x)
            nvars = swat::numericVarList(x)
            res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('SUM'))
            res2 <- swat::translate(res$results$Summary)
            # check for missing values and set value to NA
            if (! na.rm){
              res3 <- replace(res2, cas.nmiss(x) > 0, NA)  
            }
            else{
              res3 <- res2
            }
            
            return(res3)
          })

#' Correlation
#'
#' Unlike the \code{cor} function for data frames, 
#' this function does not support specifying the
#' method as "kendall" or "spearman." The results
#' for method "pearson" are returned.
#'
#' @docType methods
#' 
#' 
#' @param x CASTable.
#'
#' @return matrix
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cor(ct1)
#' cor(i2[1:4])
#' cor(i2[c('col1', 'col2', 'col3')])
#' cor(i2$col1, i2$col2)
#' }
setMethod("cor",
          signature(x = "CASTable"),
          function (x, y = NULL, use = "everything", 
                    method = c("pearson")) {
            if (!is.null(y)) {
              if (y@tname != x@tname) {
                stop("x and y must come from the same table")
              }
              vars = c(x@names, y@names)
              vars = vars[vars != ""]
              if (length(vars) == 0)
                vars = ""
              cvars = c(x@computedVars, y@computedVars)
              cvars = cvars[cvars != ""]
              if (length(cvars) == 0)
              {
                cvars = ""
                cpgm  = ""
              }
              else
              {
                cpgm = c(x@computedVarsProgram, y@computedVarsProgram)
                cpgm = cpgm[cpgm != ""]
              }
              v2 <- x
              v2@names               = vars
              v2@computedVars        = cvars
              v2@computedVarsProgram = cpgm
              tp = swat::gen.table.parm(v2)
              vars = c(v2@names, v2@computedVars)
              vars = vars[vars != ""]
              
              # check if use != complete and there are any missing values then return NA
              if ( ! startsWith(use, 'c')){
                if (sum(cas.nmiss(v2)) > 0){
                  return (as.numeric(NA))
                }
              }
              if ( startsWith(use, 'c')) {
                complete=''
                for (i in 1:length(names(v2))) {
                  complete <- paste(complete, "missing(", names(v2)[i], ")", sep='')
                  if (i < (length(names(v2))) ) {complete <- paste(complete, "and")}
                  else {complete <- paste(complete, ";")}
                }
                if (nchar(v2@where) ==0) {
                  v2@where <- complete
                }
                else {
                  w <- v2@where
                  v2@where <- paste("(", w, ") and (", complete, ")")
                }
              }
              res <- casRetrieve(x@conn, 'simple.correlation', table=tp, 
                                 simple=FALSE, inputs=as.list(vars))
              cormat <- unique(res$results$Correlation)
              rownames(cormat) <- as.list(unlist(t(cormat[1])))
              cormat2 <- cormat[-1]
              cormat2 <- cormat2[c(vars)]
              
              cormat3 <- cormat2[1:length(x),(nrow(cormat2)-length(y)+1):nrow(cormat2)]
              if (is.null(dim(cormat3))) {
                return (as.numeric(cormat3))
              }
              else {
                return(as.matrix(cormat3))
              }
            }
            else {
              vars = c(x@names, x@computedVars)
              vars = vars[vars != ""]
              
              if ( startsWith(use, 'c')) {
                complete=''
                for (i in 1:length(names(x))) {
                  complete <- paste(complete, " ", '"', names(x)[i], '"n', "^=.", sep='')
                  if (i < (length(names(x))) ) {complete <- paste(complete, "and", sep=' ')}
                  else {complete <- paste(complete, ";")}
                }
                if (nchar(x@where) ==0) {
                  x@where <- complete
                }
                else {
                  w <- x@where
                  x@where <- paste("(", w, ") and (", complete, ")")
                }
              }
              
              tp = swat::gen.table.parm(x)
              res <- casRetrieve(x@conn, 'simple.correlation', table=tp, simple=FALSE, inputs=as.list(vars))
              cormat <- unique(res$results$Correlation)
              rownames(cormat) <- as.list(unlist(t(cormat[1])))
              cormat2 <- cormat[-1]
              cormat2 <- cormat2[c(vars)]
              
              # check if use != complete and there are any missing values then return NA
              if ( ! startsWith(use, 'c')){
                nm <- cas.nmiss(x[vars])
                missvar <- nm[nm>0]
                nm3 <- as.character(names(missvar))
                cormat2[colnames(cormat2) %in% nm3 ] <- NA
                cormat2[rownames(cormat2) %in% nm3,] <- NA
              }
              
              mat1 <- as.matrix(cormat2)
              diag(mat1) <-1
              return(mat1)
            }
            
          })

#' Covariance
#'
#' Unlike the \code{cov} function for data frames, 
#' this function does not support specifying the
#' method as "kendall" or "spearman." The results
#' for method "pearson" are returned.
#'
#' @docType methods
#' 
#' @param x CASTable.
#'
#' @return matrix
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cov(ct1)
#' cov(i2[1:4])
#' cov(i2[c('col1', 'col2', 'col3')])
#' cov(i2$col1, i2$col2)
#' }
setMethod("cov",
          signature(x = "CASTable"),
          function (x, y = NULL, use = "everything", 
                    method = c("pearson")) 
          {
            cor2cov <- function(table){
              tp = swat::gen.table.parm(table)
              vars = c(table@names, table@computedVars)
              vars = vars[vars != ""]
              
              # check if use != complete and there are any missing values then return NA
              if ( startsWith(use, 'c')){
                cor <- cor(table[vars], use='complete')
                complete=''
                for (i in 1:length(names(table))) {
                  complete <- paste(complete, " ", '"', names(table)[i], '"n', "^=.", sep='')
                  if (i < (length(names(table))) ) {complete <- paste(complete, "and", sep=' ')}
                  else {complete <- paste(complete, ";")}
                }
                if (nchar(table@where) ==0) {
                  table@where <- complete
                }
                else {
                  w <- table@where
                  table@where <- paste("(", w, ") and (", complete, ")")
                }
              }
              else {
                cor <- cor(table[vars])
              }
              if ( startsWith(use, 'c')){ stdev <-as.numeric(unlist(t(cas.sd(table, na.rm=TRUE)[2])))}
              else                      { stdev <-as.numeric(unlist(t(cas.sd(table, na.rm=FALSE)[2])))}
              b <- stdev %*% t(stdev)
              cov <- b * cor
              rownames(cov) <- rownames(cor)
              return (cov)
            }
            if (!is.null(y)) {
              if (y@tname != x@tname) {
                stop("x and y must come from the same table")
              }
              vars = c(x@names, y@names)
              vars = vars[vars != ""]
              if (length(vars) == 0)
                vars = ""
              cvars = c(x@computedVars, y@computedVars)
              cvars = cvars[cvars != ""]
              if (length(cvars) == 0)
              {
                cvars = ""
                cpgm  = ""
              }
              else
              {
                cpgm = c(x@computedVarsProgram, y@computedVarsProgram)
                cpgm = cpgm[cpgm != ""]
              }
              v2 <- x
              v2@names               = vars
              v2@computedVars        = cvars
              v2@computedVarsProgram = cpgm
              cov <- cor2cov(v2)
              cormat3 <- cov[1:length(x),(nrow(cov)-length(y)+1):nrow(cov)]
              if (is.null(dim(cormat3))) {
                return (as.numeric(cormat3))
              }
              else {
                return(as.matrix(cormat3))
              }
            }
            else {
              cov <- cor2cov(x)
              return(as.matrix(cov))
            }
          })

#' Column Means
#'
#' @docType methods
#' 
#' 
#' @param x CASTable.
#'
#' @seealso \code{cas.mean}
#'
#' @return vector
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' colMeans(ct[1:4])
#' colMeans(ct$X1)
#' }
setMethod("colMeans",
          signature(x = "CASTable"),
          function(x) {
            tp = swat::gen.table.parm(x)
            nvars <- swat::numericVarList(x)
            res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('MEAN'))
            return(swat::translate(res$results$Summary))
          })

#' Count of Nonmissing Values
#'
#' Returns the number of nonmissing values in the input
#' table by column.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @seealso \code{cas.nmiss} to count missing values.
#'
#' @return casDataFrame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named N for the nonmissing count.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.count(ct[1:4])
#' cas.count(ct$n2)
#' }
cas.count       <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('N'))
  check_for_cas_errors(res)
  return(res$results$Summary)
}

#' Maximum Values
#'
#' Returns the maximum value for each column in 
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @seealso \code{max,CASTable-method}
#'
#' @return casDataFrame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named Max for the maximum value.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.max(ct[1:4])
#' cas.max(ct$n2)
#' }
cas.max <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('MAX'))
  check_for_cas_errors(res)
  return(res$results$Summary)
}


#' Average Values
#'
#' Returns the mean value for each column in 
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @seealso \code{mean,CASTable-method}
#'
#' @return casDataFrame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named Mean for the mean value.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.mean(ct[1:4])
#' cas.mean(ct$n2)
#' }
cas.mean <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('MEAN'))
  check_for_cas_errors(res)
  return(res$results$Summary)
}


#' Median Values
#'
#' Returns the median value for each column in 
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @seealso \code{median,CASTable-method}
#'
#' @return data.frame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named Median for the median value.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.median(ct[1:4])
#' cas.median(ct$n2)
#' }
cas.median <- function(CASTable, q){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'percentile.percentile', table=tp[!names(tp) == "vars"], inputs=nvars, values=list('50'))
  check_for_cas_errors(res)
  m <- res$results$Percentile
  colnames(m)[3] <- "Median"
  return(m[c(1,3)])
}

#' Minimum Values
#'
#' Returns the minimum value for each column in 
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @seealso \code{min,CASTable-method}
#'
#' @return casDataFrame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named Minimum for the minimum value.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.min(ct[1:4])
#' cas.min(ct$n2)
#' }
cas.min <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('MIN'))
  check_for_cas_errors(res)
  return(res$results$Summary)
}



#' Mode Value
#'
#' Returns the value that occurs most often for each column in 
#' the input table and the count for the value.
#'
#' This function operates on numeric and character columns.
#'
#' @param x CASTable.
#'
#' @return data.frame
#' 
#' The result includes one row for each variable.
#' One column is named Mode for the most common value. 
#' Another column is named Count to show the number of rows
#' with the most common value.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.mode(ct[1:4])
#' cas.mode(ct$n2)
#' }
cas.mode <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  vars = c(x@names, x@computedVars)
  vars = vars[vars != ""]
  res <- casRetrieve(x@conn, 'Freq', table=tp, inputs=as.list(vars))
  check_for_cas_errors(res)
  dt <- res$results$Frequency[,c("Column","FmtVar", "Frequency")]
  r2 <-do.call(rbind, by(dt, dt$Column, function(x) x[which.max(x$Frequency), ] ))
  m <- r2[match(as.character(unlist(unique(dt[1]))), r2$Column),]
  colnames(m)[2] <- "Mode"
  colnames(m)[3] <- "Count"
  rownames(m) <- NULL
  return(m[c(1:3)])
}

#' Quantile and Percentile Values
#'
#' Returns the requested percentiles for each column in 
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#' @param q A list of numeric values.
#'
#' @return data.frame
#' 
#' The result includes one row for the variable, the requested
#' percentile, and the value.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.quantile(ct[1:4], q=50)
#' cas.quantile(ct$n2, q=c(10, 25, 50, 75, 90))
#' }
cas.quantile <- function(CASTable, q){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'percentile.percentile', table=tp[!names(tp) == "vars"], inputs=nvars, values=as.list(q))
  check_for_cas_errors(res)
  return(res$results$Percentile[1:3])
}

#' Column Sums
#'
#' Returns the sum of the values for each column in 
#' the input table.
#'
#' This function operates on numeric columns only.
#' @docType methods
#'  
#' @param x CASTable.
#'
#' @seealso \code{colSums,CASTable-method}
#'
#' @return casDataFrame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named Sum for the summed value.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.sum(ct[1:4])
#' cas.sum(ct$n2)
#' }
cas.sum <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('SUM'))
  check_for_cas_errors(res)
  return(res$results$Summary)
}

#' Standard Deviation
#'
#' Returns the standard deviation of the values for each 
#' column in  the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @param na.rm An optional \code{logical}. When set to FALSE,
#'   missing values (NA) are not removed from the analysis.
#'   By default, missing values are ignored.
#'
#' @return casDataFrame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named Std for the standard deviation.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.sd(ct[1:4])
#' cas.sd(ct$n2)
#' }
cas.sd  <- function(CASTable, na.rm = TRUE){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('STD'))
  check_for_cas_errors(res)
  sd_res <- res$results$Summary
  if (! na.rm){
    nm <- cas.nmiss(x[nvars])
    missvar <- nm[nm>0]
    nm3 <- as.character(names(missvar))
    t1 <- sd_res
    t1["miss"] <- ! t1$Column %in% nm3
    
    sd_res <- transform(t1, Std = ifelse(t1$miss, Std <- t1$Std, Std <- NA))
  }
  return(sd_res[1:2])
}

#' Variance
#'
#' Returns the variance of the values for each 
#' column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @return casDataFrame
#' 
#' The result includes one row for each numeric variable
#' and a column that is named Var for the variance.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' cas.var(ct[1:4])
#' cas.var(ct$n2)
#' }
cas.var <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('VAR'))
  return(res$results$Summary)
}

#' Number of Missing Values
#'
#' Returns the number of missing values in the input
#' table by column.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @seealso \code{cas.count} to count nonmissing values.
#'
#' @return vector
#'
#' The result is a named numeric vector. You can access
#' the count of missing values by column name or index.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' x <- cas.nmiss(irisct)
#' x['Sepal.Length']
#' x[1:2]
#' }
cas.nmiss       <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'summary', table=tp, inputs=nvars, subSet=list('NMISS'))
  swat::translate(res$results$Summary)
}

#' Standard Error
#'
#' Returns the standard error of the values for each 
#' column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @return vector
#'
#' The result is a named numeric vector. You can access
#' the standard error by column name or index.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' x <- cas.stderr(irisct)
#' x['Sepal.Length']
#' x[1:2]
#' }
cas.stderr      <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('STD'))
  swat::translate(res$results$Summary)
}

#' Uncorrected Sum of Squares
#'
#' Returns the uncorrected sum of squares of the 
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @return vector
#'
#' The result is a named numeric vector. You can access
#' the uncorrected sum of squares by column name or index.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' x <- cas.uss(irisct)
#' x['Sepal.Length']
#' x[1:2]
#' }
cas.uss <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('USS'))
  swat::translate(res$results$Summary)
}

#' Corrected Sum of Squares
#'
#' Returns the corrected sum of squares of the 
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @return vector
#'
#' The result is a named numeric vector. You can access
#' the corrected sum of squares by column name or index.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' x <- cas.css(irisct)
#' x['Sepal.Length']
#' x[1:2]
#' }
cas.css <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('CSS'))
  swat::translate(res$results$Summary)
}

#' Coefficient of Variation
#'
#' Returns the coefficient of variation of the 
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @return vector
#'
#' The result is a named numeric vector. You can access
#' the coefficient of variation by column name or index.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' x <- cas.cv(irisct)
#' x['Sepal.Length']
#' x[1:2]
#' }
cas.cv  <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('CV'))
  swat::translate(res$results$Summary)
}

#' T-Statistics for Hypothesis Testing
#'
#' Returns the t-statistic for the
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @return vector
#'
#' The result is a named numeric vector. You can access
#' the t-statistic by column name or index.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' x <- cas.tvalue(irisct)
#' x['Sepal.Length']
#' x[1:2]
#' }
cas.tvalue      <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('TSTAT'))
  swat::translate(res$results$Summary)
}

#' P-Value of the T-Statistics
#'
#' Returns the p-values for the
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x CASTable.
#'
#' @return vector
#'
#' The result is a named numeric vector. You can access
#' the p-value by column name or index.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' x <- cas.probt(irisct)
#' x['Sepal.Length']
#' x[1:2]
#' }
cas.probt <- function(CASTable){
  if (class(CASTable) != 'CASTable'){ stop("Method only valid on a CASTable")}
  x <- CASTable
  tp = swat::gen.table.parm(x)
  nvars <- swat::numericVarList(x)
  res <- casRetrieve(x@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list('PROBT'))
  swat::translate(res$results$Summary)
}

#' Summary Statistics
#'
#' Returns simple descriptive statistics.
#' @docType methods
#' 
#' @param object CASTable.
#'
#' @return table
#' 
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' summary(ct1)
#' summary(ct1[1:4])
#' }
setMethod("summary",
          signature(object = "CASTable"),
          function(object, maxsum = 7,
                   digits = max(3, getOption("digits")-3), ...) {
            tp = gen.table.parm(object)
            if (sum(nchar(tp$computedVars)))
            {
              tp$vars = c(tp$vars, tp$computedVars)
              tp$vars = tp$vars[tp$vars != ""] 
            }
            res <- casRetrieve(object@conn, 'table.columninfo', table=tp)
            d = dim(res$results$ColumnInfo)
            # initialize variable lists
            ret   = list()
            nvars = list()
            cvars = list()
            for (i in 1:d[1])
            {
              if (res$results$ColumnInfo$Column[i] %in% c(object@names, object@computedVars))
              {
                if (res$results$ColumnInfo$Type[i] %in% c('double', 'int32', 'int64'))
                  nvars = c(nvars, res$results$ColumnInfo$Column[i])
                else
                  cvars = c(cvars, res$results$ColumnInfo$Column[i])
              }
            }
            # get distinct counts for NA's (missing values)
            distinct_res <- casRetrieve(object@conn, 'simple.distinct', table=tp[!names(tp) == "vars"])
            if (length(nvars) > 0)
            {
              # get statistics for numeric variables
              nres <- casRetrieve(object@conn, 'simple.summary', table=tp, inputs=nvars, subSet=list("NMISS", "MIN", "MEAN", "MAX"))
              ret = nres$results$Summary
              pctres <- casRetrieve(object@conn, 'percentile.percentile', table=tp[!names(tp) == "vars"], inputs=nvars, values=list('25', '50', '75'))
              pet <- pctres$results$Percentile
            }
            
            # format items to look like the summary function
            # create empty list
            z <- vector("list", length = nrow(res$results$ColumnInfo[1]))
            names(z) <-t(res$results$ColumnInfo[1])
            
            ncw <- function(x) {
              z <- nchar(x, type = "w")
              if (any(na <- is.na(z))) {
                z[na] <- nchar(encodeString(z[na]), "b")
              }
              z
            }
            if (length(cvars) > 0) {
              # get statistics for character variables
              freqres <- casRetrieve(object@conn, 'simple.topk', table=tp, inputs=cvars,
                                     topk=6, bottomk=0, order='freq', includeMisc=FALSE,
                                     includeMissing=FALSE)
              fres <- freqres$results$Topk@df[,c('Column', 'FmtVar', 'Score')]
              fres <- fres[order(fres$Column, fres$FmtVar, -fres$Score),]
            } 
            
            sumpop <- function(v, n=maxsum){
              # create numeric statistics
              if (v %in% nvars){
                s1 <-unlist(pet[pet$Variable==v,3])
                names(s1) <- c('1st Qu.','Median','3rd Qu.')
                s2 <- unlist(ret[ret$Column==v, c(2,5,3)])
                names(s2) <- c('Min.','Mean','Max.')
                s3 <- distinct_res$results$Distinct[distinct_res$results$Distinct$Column==v,3]
                if (s3 > 0){
                  names(s3) <- "NA's"
                  s3[1] <- as.character(s3[1])
                  return (c(s2[1],s1[1],s1[2],s2[2],s1[3],s2[3], s3[1]))
                }
                else {
                  return (c(s2[1],s1[1],s1[2],s2[2],s1[3],s2[3]))
                }
              }
              else {
                myDF <- as.data.frame(fres[fres$Column==v,2:3])
                nmiss <- distinct_res$results$Distinct[distinct_res$results$Distinct$Column==v,3]
                if ( nmiss > 0) {
                  myDF <- rbind(myDF, c("NA's", nmiss))
                }
                f2 <- unlist(myDF[2])
                names(f2) <- unlist(myDF[1])
                f3 <- f2
                if (names(f2[1]) =="NA's"){
                  na <- f2[1]
                  f2 <- f2[-1]
                  f3 <- c(f2, na)
                }
                return (f3)
              }
              
            }
            nm <- tp$vars
            nv <- length(nvars)+length(cvars)
            for (i in seq_len(nv)) {
              z[[i]] <- sumpop(names(z[i]))
            }
            
            lw <- numeric(nv)
            nr <- if (nv)
              max(vapply(z, function(x) NROW(x) + (!is.null(attr(x, "NAs"))), integer(1)))
            else 0
            for (i in seq_len(nv)) {
              z[[i]] <- sumpop(names(z[i]))
              
              sms <- z[[i]]
              lbs <- format(trimws(names(sms)))
              sms2 <- paste0(lbs, ":", format(as.numeric(sms), digits = digits), "  ")
              lw[i] <- ncw(lbs[1L])
              length(sms2) <- nr
              z[[i]] <- sms2
            }
            if (nv) {
              z <- unlist(z, use.names = TRUE)
              dim(z) <- c(nr, nv)
              if (anyNA(lw))
                warning("probably wrong encoding in names(.) of column ",
                        paste(which(is.na(lw)), collapse = ", "))
              blanks <- paste(character(max(lw, na.rm = TRUE) + 2L),
                              collapse = " ")
              pad <- floor(lw - ncw(nm)/2)
              nm <- paste0(substring(blanks, 1, pad), nm)
              dimnames(z) <- list(rep.int("", nr), nm)
            }
            attr(z, "class") <- c("table")
            return (z)
          })
