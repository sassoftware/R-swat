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


#
# CASTable arithmetic operators
#

.cas_arith <- function(e1, op, e2) {
  if (class(e1) == "CASTable") {
    rct <- new("CASTable", e1@conn, e1@tname, e1@caslib, e1@names,
      where = e1@where, orderby = e1@orderby, groupby = e1@groupby
    )
    rct@.compcomp <- e1@.compcomp
    if (nchar(e1@.computedVarsProgram)) {
      e1p <- e1@.computedVarsProgram
      rct@.computedVars <- e1@.computedVars
      rct@computedVars <- e1@computedVars
      rct@computedVarsProgram <- e1@computedVarsProgram
    }
    else {
      e1p <- c(e1@names, e1@computedVars)
      e1p <- e1p[e1p != ""]
      e1p <- paste('"', e1p, '"n', sep = "")
      if (sum(nchar(e1@computedVars))) {
        rct@.compcomp <- TRUE
        rct@.computedVars <- c(e1@.computedVars, e1@computedVars)
        rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
        rct@computedVars <- e1@computedVars
        rct@computedVarsProgram <- e1@computedVarsProgram
      }
    }
  }
  else {
    rct <- new("CASTable", e2@conn, e2@tname, e2@caslib, e2@names,
      where = e2@where, orderby = e2@orderby, groupby = e2@groupby
    )
    rct@.compcomp <- e2@.compcomp
    e1p <- e1
  }

  if (class(e2) == "CASTable") {
    if (nchar(e2@.computedVarsProgram)) {
      e2p <- e2@.computedVarsProgram
      rct@.computedVars <- c(rct@.computedVars, e2@.computedVars)
      rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
      rct@computedVars <- c(rct@computedVars, e2@computedVars)
      rct@computedVars <- rct@computedVars[rct@computedVars != ""]
      rct@computedVarsProgram <- c(rct@computedVarsProgram, e2@computedVarsProgram)
      rct@computedVarsProgram <- rct@computedVarsProgram[rct@computedVarsProgram != ""]
    }
    else {
      e2p <- c(e2@names, e2@computedVars)
      e2p <- e2p[e2p != ""]
      e2p <- paste('"', e2p, '"n', sep = "")
      if (sum(nchar(e2@computedVars))) {
        rct@.compcomp <- TRUE
        rct@.computedVars <- c(rct@.computedVars, e2@.computedVars, e2@computedVars)
        rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
        rct@computedVars <- c(rct@computedVars, e2@computedVars)
        rct@computedVars <- rct@computedVars[rct@computedVars != ""]
        rct@computedVarsProgram <- c(rct@computedVarsProgram, e2@computedVarsProgram)
        rct@computedVarsProgram <- rct@computedVarsProgram[rct@computedVarsProgram != ""]
      }
    }
  }
  else {
    e2p <- e2
  }

  if (op == " %% ") {
    rct@.computedVarsProgram <- paste("mod(", e1p, ", ", e2p, ")", sep = "")
  } else
  if (op == " %/% ") {
    rct@.computedVarsProgram <- paste("floor(", e1p, " / ", e2p, ")", sep = "")
  } else {
    rct@.computedVarsProgram <- paste("(", e1p, op, e2p, ")", sep = "")
  }
  return(rct)
}

# Subtraction ---------------------------

#' @export
setMethod(
  "-",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " - ", e2))
  }
)

#' @export
setMethod(
  "-",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_arith(e1, " - ", e2))
  }
)

#' @export
setMethod(
  "-",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " - ", e2))
  }
)

# Addition ---------------------------

#' @export
setMethod(
  "+",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " + ", e2))
  }
)

#' @export
setMethod(
  "+",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_arith(e1, " + ", e2))
  }
)

#' @export
setMethod(
  "+",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " + ", e2))
  }
)

# Division ---------------------------

#' @export
setMethod(
  "/",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " / ", e2))
  }
)

#' @export
setMethod(
  "/",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_arith(e1, " / ", e2))
  }
)

#' @export
setMethod(
  "/",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " / ", e2))
  }
)

# Multiplication ---------------------------

#' @export
setMethod(
  "*",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " * ", e2))
  }
)

#' @export
setMethod(
  "*",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_arith(e1, " * ", e2))
  }
)

#' @export
setMethod(
  "*",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " * ", e2))
  }
)

# Exponentiation ---------------------------

#' @export
setMethod(
  "^",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " ** ", e2))
  }
)

#' @export
setMethod(
  "^",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_arith(e1, " ** ", e2))
  }
)

#' @export
setMethod(
  "^",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " ** ", e2))
  }
)

# Modulus ---------------------------

#' @export
setMethod(
  "%%",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " %% ", e2))
  }
)

#' @export
setMethod(
  "%%",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_arith(e1, " %% ", e2))
  }
)

#' @export
setMethod(
  "%%",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " %% ", e2))
  }
)

# Integer Division ---------------------------

#' @export
setMethod(
  "%/%",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " %/% ", e2))
  }
)

#' @export
setMethod(
  "%/%",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_arith(e1, " %/% ", e2))
  }
)

#' @export
setMethod(
  "%/%",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_arith(e1, " %/% ", e2))
  }
)

#
# CASTable comparison operators
#

.cas_compare <- function(e1, op, e2) {
  if (class(e1) == "CASTable") {
    rct <- new("CASTable", e1@conn, e1@tname, e1@caslib, e1@names,
      where = e1@where, orderby = e1@orderby, groupby = e1@groupby
    )
    rct@.compcomp <- e1@.compcomp
    if (nchar(e1@.computedVarsProgram)) {
      e1p <- e1@.computedVarsProgram
      rct@.computedVars <- e1@.computedVars
      rct@computedVars <- e1@computedVars
      rct@computedVarsProgram <- e1@computedVarsProgram
    }
    else {
      e1p <- c(e1@names, e1@computedVars)
      e1p <- e1p[e1p != ""]
      e1p <- paste('"', e1p, '"n', sep = "")
      if (sum(nchar(e1@computedVars))) {
        rct@.compcomp <- TRUE
        rct@.computedVars <- c(e1@.computedVars, e1@computedVars)
        rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
        rct@computedVars <- e1@computedVars
        rct@computedVarsProgram <- e1@computedVarsProgram
      }
    }
  }
  else {
    rct <- new("CASTable", e2@conn, e2@tname, e2@caslib, e2@names,
      where = e2@where, orderby = e2@orderby, groupby = e2@groupby
    )
    rct@.compcomp <- e2@.compcomp
    if (class(e1) == "character") {
      e1p <- paste("'", e1, "'", sep = "")
    } else {
      e1p <- e1
    }
  }

  if (class(e2) == "CASTable") {
    if (nchar(e2@.computedVarsProgram)) {
      e2p <- e2@.computedVarsProgram
      rct@.computedVars <- c(rct@.computedVars, e2@.computedVars)
      rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
      rct@computedVars <- c(rct@computedVars, e2@computedVars)
      rct@computedVars <- rct@computedVars[rct@computedVars != ""]
      rct@computedVarsProgram <- c(rct@computedVarsProgram, e2@computedVarsProgram)
      rct@computedVarsProgram <- rct@computedVarsProgram[rct@computedVarsProgram != ""]
    }
    else {
      e2p <- c(e2@names, e2@computedVars)
      e2p <- e2p[e2p != ""]
      e2p <- paste('"', e2p, '"n', sep = "")
      if (sum(nchar(e2@computedVars))) {
        rct@.compcomp <- TRUE
        rct@.computedVars <- c(rct@.computedVars, e2@.computedVars, e2@computedVars)
        rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
        rct@computedVars <- c(rct@computedVars, e2@computedVars)
        rct@computedVars <- rct@computedVars[rct@computedVars != ""]
        rct@computedVarsProgram <- c(rct@computedVarsProgram, e2@computedVarsProgram)
        rct@computedVarsProgram <- rct@computedVarsProgram[rct@computedVarsProgram != ""]
      }
    }
  }
  else {
    if (class(e2) == "character") {
      e2p <- paste("'", e2, "'", sep = "")
    } else {
      e2p <- e2
    }
  }

  rct@.computedVarsProgram <- paste("(", e1p, op, e2p, ")", sep = "")
  return(rct)
}

# Greater Than ---------------------------

#' @export
setMethod(
  ">",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " > ", e2))
  }
)

#' @export
setMethod(
  ">",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_compare(e1, " > ", e2))
  }
)

#' @export
setMethod(
  ">",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " > ", e2))
  }
)

# Less Than ---------------------------

#' @export
setMethod(
  "<",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " < ", e2))
  }
)

#' @export
setMethod(
  "<",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_compare(e1, " < ", e2))
  }
)

#' @export
setMethod(
  "<",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " < ", e2))
  }
)

# Greater Than or Equal ---------------------------

#' @export
setMethod(
  ">=",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " >= ", e2))
  }
)

#' @export
setMethod(
  ">=",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_compare(e1, " >= ", e2))
  }
)

#' @export
setMethod(
  ">=",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " >= ", e2))
  }
)

# Less Than or Equal ---------------------------

#' @export
setMethod(
  "<=",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " <= ", e2))
  }
)

#' @export
setMethod(
  "<=",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_compare(e1, " <= ", e2))
  }
)

#' @export
setMethod(
  "<=",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " <= ", e2))
  }
)

# Equal ---------------------------

#' @export
setMethod(
  "==",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " = ", e2))
  }
)

#' @export
setMethod(
  "==",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_compare(e1, " = ", e2))
  }
)

#' @export
setMethod(
  "==",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " = ", e2))
  }
)

# Not Equal ---------------------------

#' @export
setMethod(
  "!=",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " ^= ", e2))
    return(rct)
  }
)

#' @export
setMethod(
  "!=",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_compare(e1, " ^= ", e2))
  }
)

#' @export
setMethod(
  "!=",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_compare(e1, " ^= ", e2))
  }
)

#
# CASTable logical operators
#

.cas_logic <- function(e1, op, e2) {
  if (class(e1) == "CASTable") {
    rct <- new("CASTable", e1@conn, e1@tname, e1@caslib, e1@names,
      where = e1@where, orderby = e1@orderby, groupby = e1@groupby
    )
    rct@.compcomp <- e1@.compcomp
    if (nchar(e1@.computedVarsProgram)) {
      e1p <- e1@.computedVarsProgram
      rct@.computedVars <- e1@.computedVars
      rct@computedVars <- e1@computedVars
      rct@computedVarsProgram <- e1@computedVarsProgram
    }
    else {
      e1p <- c(e1@names, e1@computedVars)
      e1p <- e1p[e1p != ""]
      e1p <- paste('"', e1p, '"n', sep = "")
      if (sum(nchar(e1@computedVars))) {
        rct@.compcomp <- TRUE
        rct@.computedVars <- c(e1@.computedVars, e1@computedVars)
        rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
        rct@computedVars <- e1@computedVars
        rct@computedVarsProgram <- e1@computedVarsProgram
      }
    }
  }
  else {
    rct <- new("CASTable", e2@conn, e2@tname, e2@caslib, e2@names,
      where = e2@where, orderby = e2@orderby, groupby = e2@groupby
    )
    rct@.compcomp <- e2@.compcomp
    e1p <- e1
  }

  if (class(e2) == "CASTable") {
    if (nchar(e2@.computedVarsProgram)) {
      e2p <- e2@.computedVarsProgram
      rct@.computedVars <- c(rct@.computedVars, e2@.computedVars)
      rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
      rct@computedVars <- c(rct@computedVars, e2@computedVars)
      rct@computedVars <- rct@computedVars[rct@computedVars != ""]
      rct@computedVarsProgram <- c(rct@computedVarsProgram, e2@computedVarsProgram)
      rct@computedVarsProgram <- rct@computedVarsProgram[rct@computedVarsProgram != ""]
    }
    else {
      e2p <- c(e2@names, e2@computedVars)
      e2p <- e2p[e2p != ""]
      e2p <- paste('"', e2p, '"n', sep = "")
      if (sum(nchar(e2@computedVars))) {
        rct@.compcomp <- TRUE
        rct@.computedVars <- c(rct@.computedVars, e2@.computedVars, e2@computedVars)
        rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
        rct@computedVars <- c(rct@computedVars, e2@computedVars)
        rct@computedVars <- rct@computedVars[rct@computedVars != ""]
        rct@computedVarsProgram <- c(rct@computedVarsProgram, e2@computedVarsProgram)
        rct@computedVarsProgram <- rct@computedVarsProgram[rct@computedVarsProgram != ""]
      }
    }
  }
  else {
    e2p <- e2
  }

  rct@.computedVarsProgram <- paste("(", e1p, op, e2p, ")", sep = "")
  return(rct)
}

# And ---------------------------

#' @export
setMethod(
  "&",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_logic(e1, " AND ", e2))
  }
)

#' @export
setMethod(
  "&",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_logic(e1, " AND ", e2))
  }
)

#' @export
setMethod(
  "&",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_logic(e1, " AND ", e2))
  }
)

# Or ---------------------------

#' @export
setMethod(
  "|",
  signature(e1 = "CASTable", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_logic(e1, " OR ", e2))
  }
)

#' @export
setMethod(
  "|",
  signature(e1 = "CASTable", e2 = "ANY"),
  function(e1, e2) {
    return(.cas_logic(e1, " OR ", e2))
  }
)

#' @export
setMethod(
  "|",
  signature(e1 = "ANY", e2 = "CASTable"),
  function(e1, e2) {
    return(.cas_logic(e1, " OR ", e2))
  }
)

# Not ---------------------------

#' @export
setMethod(
  "!",
  signature(x = "CASTable"),
  function(x) {
    rct <- new("CASTable", x@conn, x@tname, x@caslib, x@names,
      where = x@where, orderby = x@orderby, groupby = x@groupby
    )
    rct@.compcomp <- x@.compcomp
    if (sum(nchar(x@.computedVarsProgram))) {
      e1p <- x@.computedVarsProgram
      rct@.computedVars <- x@.computedVars
      rct@computedVars <- x@computedVars
      rct@computedVarsProgram <- x@computedVarsProgram
    }
    else {
      e1p <- c(x@names, x@computedVars)
      e1p <- e1p[e1p != ""]
      if (sum(nchar(x@computedVars))) {
        rct@.compcomp <- TRUE
        rct@.computedVars <- c(x@.computedVars, x@computedVars)
        rct@.computedVars <- rct@.computedVars[rct@.computedVars != ""]
        rct@computedVars <- x@computedVars
        rct@computedVarsProgram <- x@computedVarsProgram
      }
    }
    rct@.computedVarsProgram <- paste("NOT ", e1p, sep = "")
    return(rct)
  }
)
