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
  a <- gsub('\\', '/', tempfile(prefix), fixed=TRUE)
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
  tp <- list()
  if (class(ct)  == 'CASTable') tp[['name']] <- ct@tname
  if (!is.null(ct@caslib) && ct@caslib  != ''   ) tp[['caslib']] <- ct@caslib
  if (!is.null(ct@where) && ct@where   != ''    ) tp[['where']] <- ct@where
  if (!is.null(ct@orderby) && length(ct@orderby)) tp[['orderby']] <- ct@orderby
  if (!is.null(ct@groupby) && length(ct@groupby)) tp[['groupby']] <- ct@groupby
  if (!is.null(ct@gbmode) && ct@gbmode  != ''   ) tp[['groupbymode']] <- ct@gbmode

# if (length(ct@computedVars) > 1 || ct@computedVars != "")
  if (sum(nchar(ct@computedVars)))
     {
                     if (sum(nchar(ct@XcomputedVars)))
                        {
                        cmpvars <- ct@computedVars
                        for(Xcmp in ct@XcomputedVars)
                           if (!(Xcmp %in% ct@computedVars))
                              cmpvars = c(cmpvars, Xcmp)  
                        tp[['computedVars']] <- c(cmpvars)
                        }
                     else                           
                        tp[['computedVars']] <- c(ct@computedVars)

                     tp[['computedOnDemand']] <- ct@computedOnDemand
                     tp[['computedVarsProgram']] <- paste(paste(ct@computedVarsProgram,collapse=';'),';',sep='')

                     if (length(ct@names) > 1 || nchar(ct@names))
                        tp[['vars']] <- c(ct@names)
     }
  else
     {
                     if (length(ct@names) > 1 || nchar(ct@names))
                        tp[['vars']] <- c(ct@names)
                     if (sum(nchar(ct@XcomputedVars)))
                        {
                        tp[['computedVars']] <- c(ct@XcomputedVars)
                        tp[['computedVarsProgram']] <- paste(paste(ct@computedVarsProgram,collapse=';'),';',sep='')
                        }
     }

  if ((!sum(nchar(ct@XcomputedVars))) & sum(nchar(ct@XcomputedVarsProgram)))
     {
     cw = paste(ct@XcomputedVarsProgram, sep='', collapse=' AND ')
     if (ct@where != '')
           tp[['where']] <- paste('(', ct@where, ' AND ', cw, ')', sep='')
        else
           tp[['where']] <- cw
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
  if ( result$disposition$severity > 1 ) {
    if (stop.on.error) {
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


#' Download SAS SWAT binary libraries for the current platform
#'
#' @description
#' Downloads and extracts platform-specific SWAT (SAS Scripting Wrapper for Analytics Transfer)
#' binary libraries into a swat/ subdirectory of the first library path returned by `.libPaths()`.
#'
#' @param libpath Character vector of library paths. The first element is used as the installation root. Defaults to `.libPaths()`.
#' @param pkg_url Optional explicit URL to a release tar.gz. If `NULL`, a URL is constructed from the installed swat package version.
#' @param vb_version Character scalar for the Viya build/version suffix embedded in the archive name (default: `"vb24110"`).
#'
#' @return Invisibly returns the destination path of the extracted `libs` directory.
#'
#' @note
#' If `pkg_url` is not `NULL`, the current code does not set `url` (will error). You may want to add `url <- pkg_url` in the `else` branch.
#'
#' @examples
#' \dontrun{
#' download_sas_binaries()
#' }
#'
#' @export

download_sas_binaries <- function(libpath = .libPaths(), pkg_url = NULL, vb_version = "vb24110" ){
  libpath <- file.path(libpath[1], "swat")
  temp <- tempdir()
  tempfile <- file.path(temp, "r-swat.tar.gz")
  pkg_version <- packageVersion("swat")
  pkg_ver <- sub("^([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", pkg_version)
  
  platform <- switch(.Platform$OS.type,
                     windows = "win",
                     unix    = "linux",
                     stop("Platform not supported for binary connection")
  )

  if (is.null(pkg_url)) {
    pkg_url <- paste0("https://github.com/sassoftware/R-swat/releases/download/v", 
                  pkg_ver,"/R-swat-", pkg_ver ,"+",vb_version,"-", platform, "-64.tar.gz")
  }
  
  download.file(pkg_url, tempfile)
  
  installed_lib_paths = paste0("R-swat-", pkg_ver, "/inst/libs")
  
  untar(tempfile, files = installed_lib_paths, exdir = temp)
  file.rename(file.path(temp, installed_lib_paths), file.path(libpath, "libs"))
  
  message("Restart your R session and reload swat to enable binary connection")
}
