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


# Translate CAS result data frame to list
#
# This is an internal function to aid in formating results as \R users expect.
#' @keywords internal
#' @export
#' @rawRd % Copyright SAS Institute
#
# results <- cas.simple.summary(irisct)
# translate(results$Summary)
# translate(results$Summary, col='Mean')
translate <- function(df, col = 2L) {
  if (is.data.frame(df))
    x <- as.vector(t(df[, col]))
  names(x) <- as.list(df[, 1])
  return (x)
}

#' @keywords internal
#' @export
#' @rawRd % Copyright SAS Institute
uniqueTableName <- function(prefix = "") {
  a <- tempfile(prefix)
  b <- strsplit(a, split = '/')
  return (as.character(lapply(b, tail, n = 1L)))
}


# Generate Table Parameters
#
#' @keywords internal
#' @export
#' @rawRd % Copyright SAS Institute
#
# tp = swat::gen.table.parm(x)
gen.table.parm <- function(ct) {
  if (class(ct)  == 'CASTable') tp <- c(    name        = ct@tname)
  if (ct@caslib  != ''        ) tp <- c(tp, caslib      = ct@caslib)
  if (ct@where   != ''        ) tp <- c(tp, where       = ct@where)
  if (length(ct@orderby)      ) tp <- c(tp, orderby     = list(ct@orderby))
  if (length(ct@groupby)      ) tp <- c(tp, groupby     = list(ct@groupby))
  if (ct@gbmode  != ''        ) tp <- c(tp, groupbymode = ct@gbmode)

  if (length(ct@computedVars) > 1 || ct@computedVars != "")
     {
                     if (sum(nchar(ct@XcomputedVars)))
                        {
                        cmpvars = ct@computedVars
                        for(Xcmp in ct@XcomputedVars)
                           if (!(Xcmp %in% ct@computedVars))
                              cmpvars = c(cmpvars, Xcmp)  
                        tp <- c(tp, computedVars        = list(c(cmpvars)))
                        }
                     else                           
                        tp <- c(tp, computedVars        = list(c(ct@computedVars)))

                        tp <- c(tp, computedOnDemand    = ct@computedOnDemand)
                        tp <- c(tp, computedVarsProgram = paste(paste(ct@computedVarsProgram,collapse=';'),';',sep=''))

                     if (length(ct@names) > 1 || nchar(ct@names))
                        tp <- c(tp, vars                = list(c(ct@names)))
     }
  else
     {
                     if (length(ct@names) > 1 || nchar(ct@names))
                        tp <- c(tp, vars                = list(c(ct@names)))
                     if (sum(nchar(ct@XcomputedVars)))
                        {
                        tp <- c(tp, computedVars        = list(c(ct@XcomputedVars)))
                        tp <- c(tp, computedVarsProgram = paste(paste(ct@computedVarsProgram,collapse=';'),';',sep=''))
                     }
     }
  return (tp)
}


# Check if CAS Submission had Errors
#
# The function scans the result messages from CAS to see if they contain and ERROR string.
# There is an optional argument stop.on.error which defaults to TRUE. which will stop processing,
# if set to FALSE a warning will be displayed and processing will continue
#
# @param result the result to check
# @param stop.on.error should an error in the CAS log raise and exception
#
# @return Boolean
#' @keywords internal
#' @export
#' @rawRd % Copyright SAS Institute
# check_for_cas_errors(res)
check_for_cas_errors <- function(result, stop.on.error = TRUE) {
  if (length(result$messages) > 0L) {
    if (length(grep('ERROR:', result$messages, value = TRUE)) > 0 &&
        stop.on.error) {
      msgs = ''
      for (msg in result$messages)
        msgs = paste(msgs, msg[], '\n', sep='')
      stop(
        paste(
          "\nError message(s) found in CAS action results:",
          deparse(sys.call(-1)),
          msgs, sep = "\n"
        )
      )
    }
    else{
      if (length(grep('ERROR:', result$messages, value = TRUE)) > 0 &&
          stop.on.error == FALSE) {
        msgs = ''
        for (msg in result$messages)
          msgs = paste(msgs, msg[], '\n', sep='')
        warning(
          paste(
            "\nError message(s) found in CAS action results:",
            deparse(sys.call(-1)),
            msgs, sep = "\n"
          )
        )
      }
    }
    
  }
}

#https://ww2.coastal.edu/kingw/statistics/R-tutorials/formulae.html
#' CAS Function to extract information from an \R formula object
#'
#' @param f1 a formula object
#'
#' @return list with target and inputs as members
#' @export
#' @rawRd % Copyright SAS Institute
#' @keywords internal
#'
casFormula <- function(f, ct) {
  if (class(f) != "formula") {
    stop("must be a formula")
  }
  else{
    # is there a target or just inputs
    if (length(f) == 3) {
      target <- f[[2]]
      i_index = 3
    }
    else{
      target <- NULL
      i_index = 2
    }
    s1 <- deparse(f[[i_index]])
    if (s1 == ".") {
      # remove target from list
      ci <- grep(as.character(target), names(ct))
      ct <- ct[-ci]
      # format names list to string for later processing
      s0 <- as.character(names(ct)) 
      s1 <- paste(s0, collapse=' + ')
    }
    
    # check for non-supported formula operators
    s2 <- grep("[*:]", s1, ignore.case = TRUE)
    if (length(s2) == 0) {
      inputs <- lapply(strsplit(s1, '\\+'), trimws)
    }
    else {
      stop("unsupported formula operators found")
    }
  }
  return (list(target, inputs[[1]]))
}

#' @export
#' @rawRd % Copyright SAS Institute
#' @keywords internal
CASwhere <- function(ct, s) {
  # s <- "ct$n4 > 15 & df$n1 < 6"
  # string $ operator and prior string
  a <- gsub("\\b\\w+\\$", " ", s, perl = TRUE)
  
  # convert !, &, ||
  b <- gsub("&", "and", a)
  c <- gsub("!", "not", b)
  d <- gsub("\\|", "or", c)
  e <- gsub("==", "=", d)
  
  # pad the string with one space to test last token
  e <- paste(e, ' ', sep='')
  
  vars = c(ct@names, ct@computedVars)
  vars = vars[vars != ""]
  
  for (name in vars) {
    pat <- paste("\\s", name, '\\s', sep='')
    e = gsub(pat, paste('"', name, '"n', sep=''), e, ignore.case =TRUE, perl=TRUE)
  }
  return(e)
}

#' @keywords internal
#' @export
#' @rawRd % Copyright SAS Institute
numericVarList <- function(object) {
  tp = gen.table.parm(object)
  if (sum(nchar(tp$computedVars)))
  {
    tp$vars = c(tp$vars, tp$computedVars)
    tp$vars = tp$vars[tp$vars != ""] 
  }
  res <- casRetrieve(object@conn, 'columninfo', table = tp)
  d = dim(res$results$ColumnInfo)
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
  return (nvars)
}
