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


#' Translate CAS result data frame to list
#'
#' This is an internal function to aid in formating results as \R users expect.
#'
#' @keywords internal
#'
translate <- function(df, col = 2L) {
  if (is.data.frame(df)) {
    x <- as.vector(t(df[, col]))
  }
  names(x) <- as.list(df[, 1])
  return(x)
}

#' Generate a unique table name
#'
#' @param prefix Optional prefix to add to generated name.
#'
#' @return String
#'
#' @keywords internal
#'
.unique_table_name <- function(prefix = "") {
  a <- gsub("\\", "/", tempfile(prefix), fixed = TRUE)
  b <- strsplit(a, split = "/")
  return(as.character(lapply(b, tail, n = 1L)))
}

#' Generate Table Parameters
#'
#' @param ct CASTable object.
#'
#' @return List containing CASTable parameters
#'
#' @keywords internal
#'
.gen_table_param <- function(ct) {
  tp <- list()
  if (class(ct) == "CASTable") tp[["name"]] <- ct@tname
  if (!is.null(ct@caslib) && ct@caslib != "") tp[["caslib"]] <- ct@caslib
  if (!is.null(ct@where) && ct@where != "") tp[["where"]] <- ct@where
  if (!is.null(ct@orderby) && length(ct@orderby)) tp[["orderby"]] <- ct@orderby
  if (!is.null(ct@groupby) && length(ct@groupby)) tp[["groupby"]] <- ct@groupby
  if (!is.null(ct@gbmode) && ct@gbmode != "") tp[["groupbymode"]] <- ct@gbmode

  # if (length(ct@computedVars) > 1 || ct@computedVars != "")
  if (sum(nchar(ct@computedVars))) {
    if (sum(nchar(ct@XcomputedVars))) {
      cmpvars <- ct@computedVars
      for (Xcmp in ct@XcomputedVars) {
        if (!(Xcmp %in% ct@computedVars)) {
          cmpvars <- c(cmpvars, Xcmp)
        }
      }
      tp[["computedVars"]] <- c(cmpvars)
    }
    else {
      tp[["computedVars"]] <- c(ct@computedVars)
    }

    tp[["computedOnDemand"]] <- ct@computedOnDemand
    tp[["computedVarsProgram"]] <- paste(paste(ct@computedVarsProgram, collapse = ";"), ";", sep = "")

    if (length(ct@names) > 1 || nchar(ct@names)) {
      tp[["vars"]] <- c(ct@names)
    }
  }
  else {
    if (length(ct@names) > 1 || nchar(ct@names)) {
      tp[["vars"]] <- c(ct@names)
    }
    if (sum(nchar(ct@XcomputedVars))) {
      tp[["computedVars"]] <- c(ct@XcomputedVars)
      tp[["computedVarsProgram"]] <- paste(paste(ct@computedVarsProgram, collapse = ";"), ";", sep = "")
    }
  }

  if ((!sum(nchar(ct@XcomputedVars))) & sum(nchar(ct@XcomputedVarsProgram))) {
    cw <- paste(ct@XcomputedVarsProgram, sep = "", collapse = " AND ")
    if (ct@where != "") {
      tp[["where"]] <- paste("(", ct@where, " AND ", cw, ")", sep = "")
    } else {
      tp[["where"]] <- cw
    }
  }

  return(tp)
}


#' Check if CAS submission had errors
#'
#' The function scans the result messages from CAS to see if they contain
#' and ERROR string. There is an optional argument stop.on.error which
#' defaults to TRUE. which will stop processing, if set to FALSE a warning
#' will be displayed and processing will continue.
#'
#' @param result The result to check
#' @param stop.on.error Should an error in the CAS log raise and exception?
#'
#' @return logical
#'
#' @keywords internal
#'
.check_for_cas_errors <- function(result, stop.on.error = TRUE) {
  if (result$disposition$severity > 1) {
    if (stop.on.error) {
      msgs <- ""
      for (msg in result$messages) {
        msgs <- paste(msgs, msg[], "\n", sep = "")
      }
      stop(
        paste(
          "\nError message(s) found in CAS action results:",
          deparse(sys.call(-1)),
          msgs,
          sep = "\n"
        )
      )
    }
    else {
      msgs <- ""
      for (msg in result$messages) {
        msgs <- paste(msgs, msg[], "\n", sep = "")
      }
      warning(
        paste(
          "\nError message(s) found in CAS action results:",
          deparse(sys.call(-1)),
          msgs,
          sep = "\n"
        )
      )
    }
  }
}

# https://ww2.coastal.edu/kingw/statistics/R-tutorials/formulae.html
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
  else {
    # Is there a target or just inputs
    if (length(f) == 3) {
      target <- f[[2]]
      i_index <- 3
    }
    else {
      target <- NULL
      i_index <- 2
    }
    s1 <- deparse(f[[i_index]])
    if (s1 == ".") {
      # Remove target from list
      ci <- grep(as.character(target), names(ct))
      ct <- ct[-ci]
      # Format names list to string for later processing
      s0 <- as.character(names(ct))
      s1 <- paste(s0, collapse = " + ")
    }

    # Check for non-supported formula operators
    s2 <- grep("[*:]", s1, ignore.case = TRUE)
    if (length(s2) == 0) {
      inputs <- lapply(strsplit(s1, "\\+"), trimws)
    }
    else {
      stop("Unsupported formula operators found")
    }
  }
  return(list(target, inputs[[1]]))
}

#' @export
#' @rawRd % Copyright SAS Institute
#' @keywords internal
CASwhere <- function(ct, s) {
  # String $ operator and prior string
  a <- gsub("\\b\\w+\\$", " ", s, perl = TRUE)

  # Convert !, &, ||
  b <- gsub("&", "and", a)
  c <- gsub("!", "not", b)
  d <- gsub("\\|", "or", c)
  e <- gsub("==", "=", d)

  # Pad the string with one space to test last token
  e <- paste(e, " ", sep = "")

  vars <- c(ct@names, ct@computedVars)
  vars <- vars[vars != ""]

  for (name in vars) {
    pat <- paste("\\s", name, "\\s", sep = "")
    e <- gsub(pat, paste('"', name, '"n', sep = ""), e, ignore.case = TRUE, perl = TRUE)
  }
  return(e)
}

#' @keywords internal
#' @export
#' @rawRd % Copyright SAS Institute
numericVarList <- function(object) {
  tp <- .gen_table_param(object)
  if (sum(nchar(tp$computedVars))) {
    tp$vars <- c(tp$vars, tp$computedVars)
    tp$vars <- tp$vars[tp$vars != ""]
  }
  res <- casRetrieve(object@conn, "columninfo", table = tp)
  d <- dim(res$results$ColumnInfo)
  nvars <- list()
  cvars <- list()
  for (i in 1:d[1]) {
    if (res$results$ColumnInfo$Column[i] %in% c(object@names, object@computedVars)) {
      if (res$results$ColumnInfo$Type[i] %in% c("double", "int32", "int64")) {
        nvars <- c(nvars, res$results$ColumnInfo$Column[i])
      } else {
        cvars <- c(cvars, res$results$ColumnInfo$Column[i])
      }
    }
  }
  return(nvars)
}
