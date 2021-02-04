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


# CAS descriptive statistics ------------------------

#' @keywords internal
.summary_stat <- function(object, stat, na.rm = FALSE, numeric.only = FALSE) {
  UseMethod(".summary_stat")
}

#' Function for retrieving a single summary statistic
#'
#' @param x CASTable
#' @param stat Name of statistic to compute
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @keywords internal
.summary_stat.CASTable <- function(x, stat, na.rm = FALSE, numeric.only = FALSE) {
  stat <- tolower(stat)

  if (stat %in% c("n", "nmiss")) {
    res <- cas.retrieve(x@conn, "simple.summary", stop.on.error = TRUE,
                        table = x, subset = stat)

    res <- res$results$Summary

    if (numeric.only && length(names(x)) > nrow(res)) {
      stop("only defined on a CASTable with all numeric variables")
    }

    row.names(res) <- res$Column
    defval <- if (stat == "n") nrow(x) else 0
    statcol <- if (stat == "n") "N" else "NMiss"
    res <- sapply(names(x), function(col) {
      val <- res[col, statcol][[1]]
      return(if (!is.na(val)) val else defval)
    })
  }

  else if (stat %in% c("min", "max")) {
    # Try summary first and hope there aren't any character variables.
    res <- cas.retrieve(x@conn, "simple.summary", stop.on.error = TRUE,
                        table = x, subset = c(stat, "nmiss"))

    nmiss <- res$results$Summary$NMiss
    res <- .translate(res$results$Summary[, !names(res$results$Summary) %in% c('NMiss')])

    if (numeric.only && length(names(x)) > length(res)) {
      stop("only defined on a CASTable with all numeric variables")
    }

    # check for missing values and set value to NA
    if (!na.rm) {
      res <- replace(res, nmiss > 0, NA)
    }

    # If our result has fewer items than names in the table, we have character variables.
    # We don't do all variables here because topk treats empty strings as missing
    # values, but that's not consistent with R's min()/max() functions.
    if (length(res) < length(names(x))) {
      resnames <- names(res)
      xnames <- names(x)
      chars <- xnames[sapply(xnames, function(y) !(y %in% resnames))]
      topk <- if (stat == "min") 0 else 1
      bottomk <- if (stat == "min") 1 else 0
      out <- cas.retrieve(x@conn, "simple.topk", stop.on.error = TRUE,
                          table = x, topk = topk, bottomk = bottomk,
                          includemisc = FALSE, raw = TRUE,
                          includemissing = TRUE, inputs = chars)
      out <- out$results$Topk
      row.names(out) <- out$Column
      res <- sapply(names(x), function(col) {
        if (col %in% chars) {
          return(out[col, "CharVar"][[1]])
        }
        return(res[col][[1]])
      })
    }
  }

  else {
    res <- cas.retrieve(x@conn, "simple.summary", stop.on.error = TRUE,
                        table = x, subset = c(stat, "nmiss"))

    nmiss <- res$results$Summary$NMiss
    res <- .translate(res$results$Summary[, !names(res$results$Summary) %in% c('NMiss')])

    if (numeric.only && length(names(x)) > length(res)) {
      stop("only defined on a CASTable with all numeric variables")
    }

    # check for missing values and set value to NA
    if (!na.rm) {
      res <- replace(res, nmiss > 0, NA)
    }
  }

  return(res)
}

#' Maximum Values (numeric)
#'
#' This method only computes the maximum of numeric values.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Numeric
#'
#' @seealso \code{cas.max}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' max(ct1$X1)
#' max(ct1[1:2])
#' }
setMethod(
  "max",
  signature(x = "CASTable"),
  function(x, na.rm = FALSE) {
    return(max(.summary_stat(x, "max", na.rm = na.rm, numeric.only = TRUE), na.rm = na.rm))
  }
)

#' Minimum Value
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Numeric
#'
#' @seealso \code{cas.min}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' min(ct1$X1)
#' min(ct1[1:2])
#' }
setMethod(
  "min",
  signature(x = "CASTable"),
  function(x, na.rm = FALSE) {
    return(min(.summary_stat(x, "min", na.rm = na.rm, numeric.only = TRUE), na.rm = na.rm))
  }
)

#' Mean Value
#'
#' @param x      \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#' @param \ldots Additional arguments. Currently ignored.
#'
#' @return Numeric
#'
#' @seealso \code{cas.mean}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mean(ct1$x1)
#' mean(ct1[2])
#' }
setMethod(
  "mean",
  signature(x = "CASTable"),
  function(x, na.rm = FALSE, ...) {
    total <- .summary_stat(x, "sum", na.rm = na.rm, numeric.only = TRUE)
    count <- .summary_stat(x, "n", na.rm = na.rm, numeric.only = TRUE)
    return(sum(total, na.rm = na.rm) / sum(count, na.rm = na.rm))
  }
)

#' Median Values
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @seealso \code{cas.median}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' median(ct1$x1)
#' median(ct1[1:4])
#' }
setMethod(
  "median",
  signature(x = "CASTable"),
  function(x, na.rm = FALSE) {
    if (length(names(x)) > 1) {
      stop("median can only be computed on a single table column")
    }
    return(cas.median(x, na.rm = na.rm)[[1]])
  }
)

#' Column Sums
#'
#' @param x     \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#' @param dims  Ignored. Parameter is here for compatibility with \code{colSums}.
#' @param \ldots Additional parameters. Currently ignored.
#'
#' @seealso \code{cas.sum}
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' colSums(ct[1:4])
#' colSums(ct$X1)
#' }
setMethod(
  "colSums",
  signature(x = "CASTable"),
  function(x, na.rm = FALSE, dims = 1, ...) {
    return(.summary_stat(x, "sum", na.rm = na.rm, numeric.only = TRUE))
  }
)

#' Correlation
#'
#' Unlike the \code{cor} function for data frames,
#' this function does not support specifying the
#' method as "kendall" or "spearman." The results
#' for method "pearson" are returned.
#'
#' @param x     \code{\link{CASTable}} object.
#' @param y     \code{\link{CASTable}} object. This must be the same table
#'   as in \code{x}, but may have different column names selected.
#' @param use    An optional character string giving a method for computing
#'   covariances in the presence of missing values. This must be one of
#'   the following strings \code{"everything"}, \code{"all.obs"},
#'   \code{"complete.obs"}, \code{"na.or.complete"}, or
#'   \code{"pairwise.complete.obs"}.
#' @param method  Ignored. This parameter is here for compatibility with
#'   the \code{cor()} function.
#'
#' @return Matrix
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cor(ct1)
#' cor(i2[1:4])
#' cor(i2[c("col1", "col2", "col3")])
#' cor(i2$col1, i2$col2)
#' }
setMethod(
  "cor",
  signature(x = "CASTable"),
  function(x, y = NULL, use = "everything", method = c("pearson")) {
    if (!is.null(y)) {
      if (y@tname != x@tname || y@caslib != x@caslib) {
        stop("x and y must come from the same table")
      }
      vars <- c(x@names, y@names)
      vars <- vars[vars != ""]
      if (length(vars) == 0) {
        vars <- ""
      }
      cvars <- c(x@computedVars, y@computedVars)
      cvars <- cvars[cvars != ""]
      if (length(cvars) == 0) {
        cvars <- ""
        cpgm <- ""
      }
      else {
        cpgm <- c(x@computedVarsProgram, y@computedVarsProgram)
        cpgm <- cpgm[cpgm != ""]
      }
      v2 <- x
      v2@names <- vars
      v2@computedVars <- cvars
      v2@computedVarsProgram <- cpgm
      tp <- .gen_table_param(v2)
      vars <- c(v2@names, v2@computedVars)
      vars <- vars[vars != ""]

      # Check if use != complete and there are any missing values then return NA
      if (!startsWith(use, "c")) {
        if (sum(cas.nmiss(v2)) > 0) {
          return(as.numeric(NA))
        }
      }
      if (startsWith(use, "c")) {
        complete <- ""
        for (i in seq_len(length(names(v2)))) {
          complete <- paste(complete, "missing(", names(v2)[i], ")", sep = "")
          if (i < (length(names(v2)))) {
            complete <- paste(complete, "and")
          }
          else {
            complete <- paste(complete, ";")
          }
        }
        if (nchar(v2@where) == 0) {
          v2@where <- complete
        }
        else {
          w <- v2@where
          v2@where <- paste("(", w, ") and (", complete, ")")
        }
      }
      res <- cas.retrieve(x@conn, "simple.correlation",
        table = tp,
        simple = FALSE, inputs = as.list(vars)
      )
      cormat <- unique(res$results$Correlation)
      rownames(cormat) <- as.list(unlist(t(cormat[1])))
      cormat2 <- cormat[-1]
      cormat2 <- cormat2[c(vars)]

      cormat3 <- cormat2[seq_len(length(x)), (nrow(cormat2) - length(y) + 1):nrow(cormat2)]
      if (is.null(dim(cormat3))) {
        return(as.numeric(cormat3))
      }
      else {
        return(as.matrix(cormat3))
      }
    }
    else {
      vars <- c(x@names, x@computedVars)
      vars <- vars[vars != ""]

      if (startsWith(use, "c")) {
        complete <- ""
        for (i in seq_len(length(names(x)))) {
          complete <- paste(complete, " ", '"', names(x)[i], '"n', "^=.", sep = "")
          if (i < (length(names(x)))) {
            complete <- paste(complete, "and", sep = " ")
          }
          else {
            complete <- paste(complete, ";")
          }
        }
        if (nchar(x@where) == 0) {
          x@where <- complete
        }
        else {
          w <- x@where
          x@where <- paste("(", w, ") and (", complete, ")")
        }
      }

      tp <- .gen_table_param(x)
      res <- cas.retrieve(x@conn, "simple.correlation", table = tp,
                         simple = FALSE, inputs = as.list(vars))
      cormat <- unique(res$results$Correlation)
      rownames(cormat) <- as.list(unlist(t(cormat[1])))
      cormat2 <- cormat[-1]
      cormat2 <- cormat2[c(vars)]

      # Check if use != complete and there are any missing values then return NA
      if (!startsWith(use, "c")) {
        nm <- cas.nmiss(x[vars])
        missvar <- nm[nm > 0]
        nm3 <- as.character(names(missvar))
        cormat2[colnames(cormat2) %in% nm3] <- NA
        cormat2[rownames(cormat2) %in% nm3, ] <- NA
      }

      mat1 <- as.matrix(cormat2)
      diag(mat1) <- 1
      return(mat1)
    }
  }
)

#' Covariance
#'
#' Unlike the \code{cov} function for data frames,
#' this function does not support specifying the
#' method as "kendall" or "spearman." The results
#' for method "pearson" are returned.
#'
#' @param x     \code{\link{CASTable}} object.
#' @param y     \code{\link{CASTable}} object. This must be the same table
#'   as in \code{x}, but may have different column names selected.
#' @param use    An optional character string giving a method for computing
#'   covariances in the presence of missing values. This must be one of
#'   the following strings \code{"everything"}, \code{"all.obs"},
#'   \code{"complete.obs"}, \code{"na.or.complete"}, or
#'   \code{"pairwise.complete.obs"}.
#' @param method  Ignored. This parameter is here for compatibility with
#'   the \code{cov()} function.
#'
#' @return Matrix
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cov(ct1)
#' cov(i2[1:4])
#' cov(i2[c("col1", "col2", "col3")])
#' cov(i2$col1, i2$col2)
#' }
setMethod(
  "cov",
  signature(x = "CASTable"),
  function(x, y = NULL, use = "everything", method = c("pearson")) {
    cor2cov <- function(table) {
      tp <- .gen_table_param(table)
      vars <- c(table@names, table@computedVars)
      vars <- vars[vars != ""]

      # check if use != complete and there are any missing values then return NA
      if (startsWith(use, "c")) {
        cor <- cor(table[vars], use = "complete")
        complete <- ""
        for (i in seq_len(length(names(table)))) {
          complete <- paste(complete, " ", '"', names(table)[i], '"n', "^=.", sep = "")
          if (i < (length(names(table)))) {
            complete <- paste(complete, "and", sep = " ")
          }
          else {
            complete <- paste(complete, ";")
          }
        }
        if (nchar(table@where) == 0) {
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
      if (startsWith(use, "c")) {
        stdev <- as.numeric(unlist(t(cas.sd(table, na.rm = TRUE)[2])))
      }
      else {
        stdev <- as.numeric(unlist(t(cas.sd(table, na.rm = FALSE)[2])))
      }
      b <- stdev %*% t(stdev)
      cov <- b * cor
      rownames(cov) <- rownames(cor)
      return(cov)
    }
    if (!is.null(y)) {
      if (y@tname != x@tname) {
        stop("x and y must come from the same table")
      }
      vars <- c(x@names, y@names)
      vars <- vars[vars != ""]
      if (length(vars) == 0) {
        vars <- ""
      }
      cvars <- c(x@computedVars, y@computedVars)
      cvars <- cvars[cvars != ""]
      if (length(cvars) == 0) {
        cvars <- ""
        cpgm <- ""
      }
      else {
        cpgm <- c(x@computedVarsProgram, y@computedVarsProgram)
        cpgm <- cpgm[cpgm != ""]
      }
      v2 <- x
      v2@names <- vars
      v2@computedVars <- cvars
      v2@computedVarsProgram <- cpgm
      cov <- cor2cov(v2)
      cormat3 <- cov[seq_len(length(x)), (nrow(cov) - length(y) + 1):nrow(cov)]
      if (is.null(dim(cormat3))) {
        return(as.numeric(cormat3))
      }
      else {
        return(as.matrix(cormat3))
      }
    }
    else {
      cov <- cor2cov(x)
      return(as.matrix(cov))
    }
  }
)

#' Column Means
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @seealso \code{cas.mean}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' colMeans(ct[1:4])
#' colMeans(ct$X1)
#' }
setMethod(
  "colMeans",
  signature(x = "CASTable"),
  function(x, na.rm = FALSE) {
    return(.summary_stat(x, "mean", na.rm = na.rm, numeric.only = TRUE))
  }
)

#' Count of Nonmissing Values
#'
#' Returns the number of nonmissing values in the input
#' table by column.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#'
#' @return Named numeric vector
#'
#' @seealso \code{cas.nmiss} to count missing values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.count(ct[1:4])
#' cas.count(ct$n2)
#' }
cas.count <- function(object, na.rm = FALSE) {
  UseMethod("cas.count")
}

#' @export
cas.count.CASTable <- function(x) {
  return(.summary_stat(x, "n"))
}

#' Maximum Values
#'
#' Returns the maximum value for each column in
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @seealso \code{max,CASTable-method}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.max(ct[1:4])
#' cas.max(ct$n2)
#' }
cas.max <- function(object, na.rm = FALSE) {
  UseMethod("cas.max")
}

#' @export
cas.max.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "max", na.rm = na.rm))
}

#' Average Values
#'
#' Returns the mean value for each column in
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @seealso \code{mean,CASTable-method}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.mean(ct[1:4])
#' cas.mean(ct$n2)
#' }
cas.mean <- function(object, na.rm = FALSE) {
  UseMethod("cas.mean")
}

#' @export
cas.mean.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "mean", na.rm = na.rm))
}

#' Median Values
#'
#' Returns the median value for each column in
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @seealso \code{median,CASTable-method}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.median(ct[1:4])
#' cas.median(ct$n2)
#' }
cas.median <- function(object, na.rm = FALSE) {
  UseMethod("cas.median")
}

#' @export
cas.median.CASTable <- function(x, na.rm = FALSE) {
  return(unlist(cas.quantile(x, q = 50, na.rm = na.rm)[1, ]))
}

#' Minimum Values
#'
#' Returns the minimum value for each column in
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @seealso \code{min,CASTable-method}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.min(ct[1:4])
#' cas.min(ct$n2)
#' }
cas.min <- function(object, na.rm = FALSE) {
  UseMethod("cas.min")
}

#' @export
cas.min.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "min", na.rm = na.rm))
}

#' Mode Value
#'
#' Returns the value that occurs most often for each column in
#' the input table and the count for the value.
#'
#' @param x \code{\link{CASTable}} object.
#' @param max.tie Maximum number of duplicate values to return if values have the same rank.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.mode(ct[1:4])
#' cas.mode(ct$n2)
#' }
cas.mode <- function(object, max.tie = 25, na.rm = FALSE) {
  UseMethod("cas.mode")
}

#' @export
cas.mode.CASTable <- function(x, max.tie = 25, na.rm = FALSE) {
  nvars <- .numeric_var_list(x)
  res <- cas.retrieve(x@conn, "simple.topk", stop.on.error = TRUE,
                      order = "freq", includeMisc = FALSE,
                      table = x, topK = 1, bottomK = 0, raw = TRUE,
                      maxtie = max.tie - 1, includemissing = !na.rm)

  if (!na.rm) {
    nmiss <- cas.nmiss(x)
    nmiss <- names(nmiss[nmiss > 0])
  }

  res <- res$results$Topk
  cols <- lapply(unique(res$Column), function(x) {
    col <- subset(res, Column == x)
    if (!na.rm && x %in% nmiss) {
      col <- NA
    }
    else {
      col <- col[[ifelse(x %in% nvars, "NumVar", "CharVar")]]
    }
    return(col)
  })

  maxl <- max(sapply(cols, length))
  df <- data.frame(lapply(cols, function(x) {
    c(x, rep(ifelse(class(x) == "character", "", NA), maxl - length(x)))
  }))
  names(df) <- unique(res$Column)
  return(df)
}

#' Quantile and Percentile Values
#'
#' Returns the requested percentiles for each column in
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param q A list of numeric values.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return data.frame with row labels as variable names and column
#'         labels as percentage labels (e.g., '25%', '50%', etc.).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.quantile(ct[1:4], q = 50)
#' cas.quantile(ct$n2, q = c(10, 25, 50, 75, 90))
#' }
cas.quantile <- function(object, q = c(0, 25, 50, 70, 100), na.rm = FALSE) {
  UseMethod("cas.quantile")
}

#' @export
cas.quantile.CASTable <- function(x, q = c(0, 25, 50, 70, 100), na.rm = FALSE) {
  res <- cas.retrieve(x@conn, "percentile.percentile", stop.on.error = TRUE,
                      table = x, values = as.list(q))
  res <- res$results$Percentile
  cols <- list()
  colnames <- c()
  nmiss <- NULL
  if (!na.rm) {
    nmiss <- cas.nmiss(x)
    nmiss <- nmiss[names(nmiss) %in% res$Variable]
  }
  for (item in q) {
    values <- res[res$Pctl == item, ]$Value
    # check for missing values and set value to NA
    if (!na.rm) {
      values <- replace(values, nmiss > 0, NA)
    }
    cols[[paste(item)]] <- values
    colnames <- c(colnames, paste(item, "%", sep = ""))
  }
  cols[["row.names"]] <- res[res$Pctl == q[1], ]$Variable
  res <- do.call("data.frame", cols)
  names(res) <- colnames
  return(t(res))
}

#' @export
quantile.CASTable <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
                              names = TRUE, type = 7, ...) {
  if (length(names(x)) > 1) {
    stop("quantile can only be computed on a single table column")
  }
  return(unlist(t(cas.quantile(x, q = probs * 100, na.rm = na.rm))[1, ]))
}

#' Column Sums
#'
#' Returns the sum of the values for each column in
#' the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @seealso \code{colSums,CASTable-method}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.sum(ct[1:4])
#' cas.sum(ct$n2)
#' }
cas.sum <- function(object, na.rm = FALSE) {
  UseMethod("cas.sum")
}

#' @export
cas.sum.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "sum", na.rm = na.rm))
}

#' @export
sum.CASTable <- function(x, na.rm = FALSE) {
  return(sum(cas.sum(x, na.rm = na.rm)))
}

#' Standard Deviation
#'
#' Returns the standard deviation of the values for each
#' column in  the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x     \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.sd(ct[1:4])
#' cas.sd(ct$n2)
#' }
cas.sd <- function(object, na.rm = FALSE) {
  UseMethod("cas.sd")
}

#' @export
cas.sd.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "std", na.rm = na.rm))
}

#' Variance
#'
#' Returns the variance of the values for each
#' column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cas.var(ct[1:4])
#' cas.var(ct$n2)
#' }
cas.var <- function(object, na.rm = FALSE) {
  UseMethod("cas.var")
}

#' @export
cas.var.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "var", na.rm = na.rm))
}

#' Number of Missing Values
#'
#' Returns the number of missing values in the input
#' table by column.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#'
#' @return Named numeric vector
#'
#' @seealso \code{cas.count} to count nonmissing values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cas.nmiss(irisct)
#' x["Sepal.Length"]
#' x[1:2]
#' }
cas.nmiss <- function(object, na.rm = FALSE) {
  UseMethod("cas.nmiss")
}

#' @export
cas.nmiss.CASTable <- function(x) {
  return(.summary_stat(x, "nmiss"))
}

#' Standard Error
#'
#' Returns the standard error of the values for each
#' column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cas.stderr(irisct)
#' x["Sepal.Length"]
#' x[1:2]
#' }
cas.stderr <- function(object, na.rm = FALSE) {
  UseMethod("cas.stderr")
}

#' @export
cas.stderr.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "std", na.rm = na.rm))
}

#' Uncorrected Sum of Squares
#'
#' Returns the uncorrected sum of squares of the
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cas.uss(irisct)
#' x["Sepal.Length"]
#' x[1:2]
#' }
cas.uss <- function(object, na.rm = FALSE) {
  UseMethod("cas.uss")
}

#' @export
cas.uss.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "uss", na.rm = na.rm))
}

#' Corrected Sum of Squares
#'
#' Returns the corrected sum of squares of the
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cas.css(irisct)
#' x["Sepal.Length"]
#' x[1:2]
#' }
cas.css <- function(object, na.rm = FALSE) {
  UseMethod("cas.css")
}

#' @export
cas.css.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "css", na.rm = na.rm))
}

#' Coefficient of Variation
#'
#' Returns the coefficient of variation of the
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cas.cv(irisct)
#' x["Sepal.Length"]
#' x[1:2]
#' }
cas.cv <- function(object, na.rm = FALSE) {
  UseMethod("cas.cv")
}

#' @export
cas.cv.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "cv", na.rm = na.rm))
}

#' T-Statistics for Hypothesis Testing
#'
#' Returns the t-statistic for the
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cas.tvalue(irisct)
#' x["Sepal.Length"]
#' x[1:2]
#' }
cas.tvalue <- function(object, na.rm = FALSE) {
  UseMethod("cas.tvalue")
}

#' @export
cas.tvalue.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "t", na.rm = na.rm))
}

#' P-Value of the T-Statistics
#'
#' Returns the p-values for the
#' values for each column in the input table.
#'
#' This function operates on numeric columns only.
#'
#' @param x \code{\link{CASTable}} object.
#' @param na.rm Should missing values be omitted from the calculations?
#'
#' @return Named numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cas.probt(irisct)
#' x["Sepal.Length"]
#' x[1:2]
#' }
cas.probt <- function(object, na.rm = FALSE) {
  UseMethod("cas.probt")
}

#' @export
cas.probt.CASTable <- function(x, na.rm = FALSE) {
  return(.summary_stat(x, "probt", na.rm = na.rm))
}

#' Summary Statistics
#'
#' Returns simple descriptive statistics.
#'
#' @param object \code{\link{CASTable}} object.
#' @param maxsum Integer indicating how many levels should be shown
#'   for factors.
#' @param digits Integer used for number formatting.
#' @param \ldots Additional arguments. Currently ignored.
#'
#' @return table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summary(ct1)
#' summary(ct1[1:4])
#' }
setMethod(
  "summary",
  signature(object = "CASTable"),
  function(object, maxsum = 7, digits = max(3, getOption("digits") - 3), ...) {
    tp <- .gen_table_param(object)

    # initialize variable lists
    vars <- .column_types(object)
    ctypes <- c("char", "varchar", "binary", "varbinary")
    nvars <- vars[!(vars %in% ctypes)]
    cvars <- vars[vars %in% ctypes]

    # get distinct counts for NA's (missing values)
    distinct_res <- cas.retrieve(object, "simple.distinct", table = tp,
                                 stop.on.error = TRUE)

    if (length(nvars) > 0) {
      # get statistics for numeric variables
      sumres <- cas.retrieve(object, "simple.summary", table = tp,
                             inputs = nvars, subset = c("min", "mean", "max", "nmiss"),
                             stop.on.error = TRUE)
      sum <- nres$results$Summary

      pctres <- cas.retrieve(object, "percentile.percentile", table = tp,
                             inputs = nvars, values = c(25, 50, 75),
                             stop.on.error = TRUE)
      pct <- pctres$results$Percentile
    }

    # format items to look like the summary function

    # create empty list
    z <- vector("list", length = length(vars))
    names(z) <- names(vars)

    ncw <- function(x) {
      z <- nchar(x, type = "w")
      if (any(na <- is.na(z))) {
        z[na] <- nchar(encodeString(z[na]), "b")
      }
      z
    }

    sumpop <- function(v, n = maxsum) {
      # create numeric statistics
      if (v %in% nvars) {
        s1 <- unlist(pct[pct$Variable == v, 3])
        names(s1) <- c("1st Qu.", "Median", "3rd Qu.")
        s2 <- unlist(ret[ret$Column == v, c(2, 5, 3)])
        names(s2) <- c("Min.", "Mean", "Max.")
        s3 <- distinct_res$results$Distinct[distinct_res$results$Distinct$Column == v, 3]
        if (s3 > 0) {
          names(s3) <- "NA's"
          s3[1] <- as.character(s3[1])
          return(c(s2[1], s1[1], s1[2], s2[2], s1[3], s2[3], s3[1]))
        }
        else {
          return(c(s2[1], s1[1], s1[2], s2[2], s1[3], s2[3]))
        }
      }
      else {
        my_df <- as.data.frame(fres[fres$Column == v, 2:3])
        nmiss <- distinct_res$results$Distinct[distinct_res$results$Distinct$Column == v, 3]
        if (nmiss > 0) {
          my_df <- rbind(my_df, c("NA's", nmiss))
        }
        f2 <- unlist(my_df[2])
        names(f2) <- unlist(my_df[1])
        f3 <- f2
        if (names(f2[1]) == "NA's") {
          na <- f2[1]
          f2 <- f2[-1]
          f3 <- c(f2, na)
        }
        return(f3)
      }
    }

    nm <- tp$vars
    nv <- length(nvars) + length(cvars)
    for (i in seq_len(nv)) {
      z[[i]] <- sumpop(names(z[i]))
    }

    lw <- numeric(nv)
    nr <- if (nv) {
      max(vapply(z, function(x) NROW(x) + (!is.null(attr(x, "NAs"))), integer(1)))
    } else {
      0
    }

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
      if (anyNA(lw)) {
        warning(
          "probably wrong encoding in names(.) of column ",
          paste(which(is.na(lw)), collapse = ", ")
        )
      }
      blanks <- paste(character(max(lw, na.rm = TRUE) + 2L),
        collapse = " "
      )
      pad <- floor(lw - ncw(nm) / 2)
      nm <- paste0(substring(blanks, 1, pad), nm)
      dimnames(z) <- list(rep.int("", nr), nm)
    }

    attr(z, "class") <- c("table")

    return(z)
  }
)
