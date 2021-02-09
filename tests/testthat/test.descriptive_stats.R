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

library(swat)

options(cas.print.messages = FALSE)

.num_cols <- function(x) {
  if (class(x) != "data.frame") stop("parameter must be a data frame")
  return(names(x)[sapply(names(x), function(y) !(class(x[, y]) %in% c("character", "factor")))])
}

context("descriptive_stats.R")

# Overloaded functions
test_that("Table Meta Functions", {
  # rownames
  expect_equivalent(rownames(ct), rownames(df))

  # colnames
  expect_equivalent(colnames(df), colnames(ct))

  # dimnames
  expect_equivalent(dimnames(df), dimnames(ct))

  # nrow
  expect_equivalent(nrow(df), nrow(ct))

  # ncol
  expect_equivalent(ncol(df), ncol(ct))

  # dim
  expect_equivalent(dim(df), dim(ct))
})

test_that("cas.count", {
  cout <- cas.count(t)
  dfout <- sapply(titanic, function(x) sum(!is.na(x)))
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("min", {
  nums <- .num_cols(iris)
  cout <- min(i2[, nums])
  dfout <- min(iris[, nums])
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("cas.min", {
  nums <- .num_cols(titanic)

  cout <- cas.min(t)
  dfout <- sapply(titanic, min)
  expect_that(cout, is_a("character"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.min(t, na.rm = TRUE)
  dfout <- sapply(titanic, min, na.rm = TRUE)
  expect_that(cout, is_a("character"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.min(t[, nums])
  dfout <- sapply(titanic[, nums], min)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.min(t[, nums], na.rm = TRUE)
  dfout <- sapply(titanic[, nums], min, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  nums <- .num_cols(iris)

  cout <- cas.min(i2[, nums])
  dfout <- sapply(iris[, nums], min)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.min(i2[, nums], na.rm = TRUE)
  dfout <- sapply(iris[, nums], min, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("max", {
  nums <- .num_cols(iris)
  cout <- max(i2[, nums])
  dfout <- max(iris[, nums])
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  nums <- .num_cols(titanic)
  cout <- max(t[, nums])
  dfout <- max(titanic[, nums])
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- max(t[, nums], na.rm = TRUE)
  dfout <- max(titanic[, nums], na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("cas.max", {
  nums <- .num_cols(titanic)

  cout <- cas.max(t)
  dfout <- sapply(titanic, max)
  expect_that(cout, is_a("character"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.max(t, na.rm = TRUE)
  dfout <- sapply(titanic, max, na.rm = TRUE)
  expect_that(cout, is_a("character"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.max(t[, nums])
  dfout <- sapply(titanic[, nums], max)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.max(t[, nums], na.rm = TRUE)
  dfout <- sapply(titanic[, nums], max, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  nums <- .num_cols(iris)

  cout <- cas.max(i2[, nums])
  dfout <- sapply(iris[, nums], max)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.max(i2[, nums], na.rm = TRUE)
  dfout <- sapply(iris[, nums], max, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("median", {
  expect_error(median(i2))
  expect_error(median(i2$Species))

  # R's median averages the median of even numbered variables.
  # Petal.Length is different than for CAS.
  vars <- c("Sepal.Length", "Sepal.Width", "Petal.Width")

  cout <- sapply(vars, function (x) median(i2[, x]))
  dfout <- sapply(vars, function (x) median(iris[, x]))
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- sapply(vars, function (x) median(i2[, x], na.rm = TRUE))
  dfout <- sapply(vars, function (x) median(iris[, x], na.rm = TRUE))
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("cas.median", {
  # cas.median only does numerics 
  nums <- .num_cols(titanic)

  cout <- cas.median(t)
  dfout <- sapply(titanic[, nums], median, quantile.type = 1)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.median(t, na.rm = TRUE)
  dfout <- sapply(titanic[, nums], median, quantile.type = 1, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  vars <- c("Sepal.Length", "Sepal.Width", "Petal.Width")

  cout <- cas.median(i2[, vars])
  dfout <- sapply(iris[, vars], median, quantile.type = 1)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.median(i2[, vars], na.rm = TRUE)
  dfout <- sapply(iris[, vars], median, quantile.type = 1, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("cas.mode", {
  compute.mode <- function(v, na.rm = FALSE) {
    u <- unique(v)
    if (na.rm) {
      u <- u[u != ""]
      u <- u[!(is.na(u))]
    }
    u[which.max(tabulate(match(v, u)))]
  }

  cout <- cas.mode(t)
  expect_equivalent(nrow(cout), 25)
  expect_equivalent(ncol(cout), ncol(titanic))

  cout <- cas.mode(t, max.tie = 5)
  expect_equivalent(nrow(cout), 5)
  expect_equivalent(ncol(cout), ncol(titanic))

  # Remove PassengerID and Name since they are all unique values
  cols <- names(t)
  cols <- cols[!(cols %in% c("PassengerId", "Name"))]

  # dfout only contains one value for each column
  cout <- cas.mode(t[, cols])
  dfout <- data.frame(lapply(titanic[, cols], compute.mode))
  expect_true(all(sapply(cols, function (x) {
    dfout[, x][[1]] %in% cout[, x]
  })))

  cout <- cas.mode(t[, cols], na.rm = TRUE)
  dfout <- data.frame(lapply(titanic[, cols], compute.mode, na.rm = TRUE))
  expect_true(all(sapply(cols, function (x) {
    dfout[, x][[1]] %in% cout[, x]
  })))
})

test_that("quantile", {
  expect_error(quantile(i2))
  expect_error(quantile(i2$Species))

  # R's median averages the quantile of even numbered variables.
  # Petal.Length is different than for CAS.
  vars <- c("Sepal.Length", "Sepal.Width", "Petal.Width")

  cout <- sapply(vars, function (x) quantile(i2[, x]))
  dfout <- sapply(vars, function (x) quantile(iris[, x]))
  expect_that(cout, is_a("matrix"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- sapply(vars, function (x) quantile(i2[, x], na.rm = TRUE))
  dfout <- sapply(vars, function (x) quantile(iris[, x], na.rm = TRUE))
  expect_that(cout, is_a("matrix"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("cas.quantile", {
  # R's median averages the quantile of even numbered variables.
  # Petal.Length is different than for CAS.
  vars <- c("Sepal.Length", "Sepal.Width", "Petal.Width")

  cout <- cas.quantile(i2[, vars])
  dfout <- sapply(vars, function (x) quantile(iris[, x], quantile.type = 1))
  expect_that(cout, is_a("matrix"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.quantile(i2[, vars], q = c(25, 75))
  dfout <- sapply(vars, function (x) quantile(iris[, x], probs = c(.25, .75), quantile.type = 1))
  expect_that(cout, is_a("matrix"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  cout <- cas.quantile(i2[, vars], na.rm = TRUE)
  dfout <- sapply(vars, function (x) quantile(iris[, x], na.rm = TRUE, quantile.type = 1))
  expect_that(cout, is_a("matrix"))
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)

  nums <- .num_cols(titanic)

  passenger_id <- c(1, 223, 446, 669, 891)
  survived <- c(0, 0, 0, 1, 1)
  pclass <- c(1, 2, 3, 3, 3)
  age <- c(.42, 20, 28, 38, 80)
  sibsp <- c(0, 0, 0, 1, 8)
  parch <- c(0, 0, 0, 0, 6)
  fare <- c(0, 7.8958, 14.4542, 31, 512.3292)

  cout <- cas.quantile(t)
  expect_that(cout, is_a("matrix"))
  expect_equivalent(colnames(cout), nums)
  expect_equivalent(rownames(cout), c("0%", "25%", "50%", "75%", "100%"))
  expect_equivalent(cout[, "PassengerId"], passenger_id)
  expect_equivalent(cout[, "Survived"], survived)
  expect_equivalent(cout[, "Pclass"], pclass)
  expect_equivalent(length(is.na(cout[, "Age"])), 5)
  expect_equivalent(cout[, "SibSp"], sibsp)
  expect_equivalent(cout[, "Parch"], parch)
  expect_equivalent(cout[, "Fare"], fare)

  cout <- cas.quantile(t, q = c(25, 75))
  expect_that(cout, is_a("matrix"))
  expect_equivalent(colnames(cout), nums)
  expect_equivalent(rownames(cout), c("25%", "75%"))
  expect_equivalent(cout[, "PassengerId"], passenger_id[c(2, 4)])
  expect_equivalent(cout[, "Survived"], survived[c(2, 4)])
  expect_equivalent(cout[, "Pclass"], pclass[c(2, 4)])
  expect_equivalent(length(is.na(cout[, "Age"])), 2)
  expect_equivalent(cout[, "SibSp"], sibsp[c(2, 4)])
  expect_equivalent(cout[, "Parch"], parch[c(2, 4)])
  expect_equivalent(cout[, "Fare"], fare[c(2, 4)])

  cout <- cas.quantile(t, na.rm = TRUE)
  expect_that(cout, is_a("matrix"))
  expect_equivalent(colnames(cout), nums)
  expect_equivalent(rownames(cout), c("0%", "25%", "50%", "75%", "100%"))
  expect_equivalent(cout[, "PassengerId"], passenger_id)
  expect_equivalent(cout[, "Survived"], survived)
  expect_equivalent(cout[, "Pclass"], pclass)
  expect_equivalent(cout[, "Age"], age)
  expect_equivalent(cout[, "SibSp"], sibsp)
  expect_equivalent(cout[, "Parch"], parch)
  expect_equivalent(cout[, "Fare"], fare)
})

test_that("colSums", {
  expect_that(colSums(ct[1:4]), is_a("numeric"))
  expect_equivalent(colSums(df[1:4]), colSums(ct[1:4]))
})

test_that("colMeans", {
  expect_that(colMeans(ct[1:4]), is_a("numeric"))
  expect_equivalent(colMeans(df[1:4]), colMeans(ct[1:4]))
  # colMeans seems to always return the columns in order, even if a different order is specified.
  expect_failure(expect_equivalent(colMeans(ct[c(1, 4, 3)]), colMeans(df[c(1, 3, 4)])))
  expect_equivalent(colMeans(ct[c(1, 4, 3)]), colMeans(df[c(1, 4, 3)]))
})

test_that("head", {
  # This diff because of the factor column in 's'.
  # > expect_equivalent(head(df, 4), head(ct, 4))
  expect_equivalent(head(df[1:4], 4), head(ct[1:4], 4))
  expect_that(head(ct, 4), is_a("data.frame"))
})

test_that("tail", {
  # Order is non-deterministic in CAS tables
  i2@orderby <- list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  iris_sorted <- iris[order(iris$Sepal.Length, iris$Sepal.Width,
                            iris$Petal.Length, iris$Petal.Width), ]
  expect_equivalent(tail(df[1:4], 4), tail(ct[1:4], 4))
  expect_that(tail(ct, 4), is_a("data.frame"))
  expect_equivalent(tail(i2[1:4]), tail(iris_sorted[1:4]))
  i2@orderby <- list()

  # check that row index numbering is correct
  expect_equivalent(rownames(tail(i2[1:4])), rownames(tail(iris[1:4])))

  # check that tail works correctly after sort
  ct@orderby <- list("n4")
  expect_equivalent(tail(df[order(df$n4), c(1:4)], 4), tail(ct[c(1:4)], 4))
  expect_true(all.equal(tail(df[order(df$n4), c(1:4)], 4), tail(ct[c(1:4)], 4),
                        tolerance = 1e-06, check.attributes = FALSE))

  #
  # NOTE: Character columns are returned as factors in R and Date columns
  #       are returned as Character.  Currently the tail function is not the
  #       the same for those two types.
  #
  # Column 5 is a Character column.  Column 6 is a Date column.
  # > expect_equivalent(tail(df[5:6], 4), tail(ct[5:6], 4))
})

test_that("correlation", {
  expect_equivalent(cor(ct1), cor(df1))
  expect_equivalent(cor(i2[1:4]), cor(iris[1:4]))
  expect_equivalent(cor(i2$Sepal.Length, i2$Sepal.Width), cor(iris$Sepal.Length, iris$Sepal.Width))
  expect_equivalent(cor(i2[1:2], i2[3:4]), cor(iris[1:2], iris[3:4]))
  expect_that(cor(ct1), is_a("matrix"))
  expect_that(cor(df1), is_a("matrix"))
  expect_that(cor(i2[1:2], i2[3:4]), is_a("matrix"))
  expect_that(cor(iris[1:2], iris[3:4]), is_a("matrix"))
  # use=
  expect_equivalent(cor(ct0$n1, ct0$n2), as.numeric(NA))
  expect_equivalent(cor(ct0$n1, ct0$n2), cor(df0$n1, df0$n2))
  expect_equivalent(cor(df0$n1, df0$n2, use = "complete"), cor(ct0$n1, ct0$n2, use = "complete"))
  expect_true(all.equal(cor(ctn), cor(dfn), tolerance = 1.e-6))
  # method
  expect_true(all.equal(cor(iris$Sepal.Length, iris$Sepal.Width, method = "pearson"), -0.1175698, tolerance = 1.e-6))
  # everything
  expect_equivalent(cor(ct$n1, ct$n2, use = "everything"), cor(df$n1, df$n2, use = "everything"))

  # Missing values test
  expect_equivalent(cor(ctn), cor(dfn))
  expect_equal(cor(ct0[1:4], use = "complete"), cor(df0[1:4], use = "complete"), tolerance = 1.e-5)
})

test_that("covariance", {
  expect_equivalent(cov(ct1), cov(df1))
  expect_equivalent(cov(i2[1:4]), cov(iris[1:4]))
  expect_equivalent(cov(i2$Sepal.Length, i2$Sepal.Width), cov(iris$Sepal.Length, iris$Sepal.Width))
  expect_equivalent(cov(i2[1:2], i2[3:4]), cov(iris[1:2], iris[3:4]))
  expect_that(cov(ct1), is_a("matrix"))
  expect_that(cov(i2[1:2], i2[3:4]), is_a("matrix"))
  expect_that(cov(df1), is_a("matrix"))
  expect_that(cov(iris[1:2], iris[3:4]), is_a("matrix"))
  # use=
  expect_equivalent(cov(ct0$n1, ct0$n2), as.numeric(NA))
  expect_equivalent(cov(ct0$n1, ct0$n2), cov(df0$n1, df0$n2))
  expect_equivalent(cov(df0$n1, df0$n2, use = "complete"), cov(ct0$n1, ct0$n2, use = "complete"))
  expect_equivalent(cov(df0[1:3], use = "complete"), cov(ct0[1:3], use = "complete"))
  expect_true(all.equal(cov(ctn), cov(dfn), tolerance = 1.e-6))
  # method
  expect_true(all.equal(cov(iris$Sepal.Length, iris$Sepal.Width, method = "pearson"), -0.042434, tolerance = 1.e-6))
  expect_true(all.equal(cov(i2$Sepal.Length, i2$Sepal.Width, method = "pearson"), -0.042434, tolerance = 1.e-6))
  # everything
  expect_equivalent(cov(ct$n1, ct$n2, use = "everything"), cov(df$n1, df$n2, use = "everything"))
})

test_that("str", {
  expect_output(str(ct), "CASTable")
})

test_that("summary", {
  expect_that(summary(i2), is_a("table"))
  expect_true(all.equal(summary(i2), summary(iris, quantile.type = 1), check.attributes = FALSE))
})

test_that("column selection", {
  expect_equivalent(i2[1:3], i2[c("Sepal.Length", "Sepal.Width", "Petal.Length")])
  expect_equivalent(i2[1], i2$Sepal.Length)
  expect_equivalent(i2["Sepal.Length"], i2$Sepal.Length)
})

test_that("cas.tvalue", {
  expect_that(cas.tvalue(ct), is_a("numeric"))
  n1 <- c(3, 3, 3, 3, 3, 3)
  n2 <- c(5, 6, 7, 7, 8, 120)
  n3 <- c(12, 13, 15, 15, 8, 198)
  n4 <- c(15, 16, 17, 15, 8, 1120)
  dfc <- data.frame(n1, n2, n3, n4)
  ctc <- as.CASTable(caz, dfc)
  response <- as.numeric(c(NaN, 1.348879, 1.406952, 1.077021))
  names(response) <- c("n1", "n2", "n3", "n4")
  expect_equal(cas.tvalue(ctc), response, tolerance = 1e-6)
})

test_that("cas.uss", {
  expect_equivalent(cas.uss(i2[1:4]), c(5223.85, 1430.40, 2582.71, 302.33))
})

test_that("cas.cv", {
  cv <- function(x, na.rm = FALSE) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(100*(sd(x)/mean(x)))
    }
  }
  nums <- .num_cols(iris)

  # testthat::skip("Issue 87")
  cv1 <- sapply(nums, function(x) cv(iris[, x]))
  names(cv1) <- nums

  cout <- cas.cv(i2)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(cv1))
  expect_true(all.equal(cout, cv1, tolerance = 1.e-5))

  cout <- cas.cv(i2, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(cv1))
  expect_true(all.equal(cout, cv1, tolerance = 1.e-5))

  nums <- c("n1", "n2", "n3", "n4")
  cv1 <- sapply(df0[, nums], cv)
  cv1.na.rm <- sapply(df0[, nums], cv, na.rm = TRUE)
  names(cv1) <- nums
  names(cv1.na.rm) <- nums

  cout <- cas.cv(ct0)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(cv1))
  expect_true(all.equal(cout, cv1, tolerance = 1.e-4))

  cout <- cas.cv(ct0, na.rm = TRUE)
  expect_that(cout, is_a("numeric"))
  expect_equivalent(names(cout), names(cv1.na.rm))
  expect_true(all.equal(cout, cv1.na.rm, tolerance = 1.e-4))
})

test_that("cas.sd", {
  cout <- cas.sd(mtcars_ct)
  dfout <- sapply(mtcars, sd)
  expect_equivalent(cout, dfout)

  cout <- cas.sd(mtcars_ct, na.rm = TRUE)
  dfout <- sapply(mtcars, sd, na.rm = TRUE)
  all.equal(cout, dfout)
})

test_that("cas.css", {
  cout <- cas.css(i2)
  css1 <- c(102.16833, 28.30693, 464.32540, 86.56993)
  expect_that(cout, is_a("numeric"))
  expect_true(all.equal(cout, css1, tolerance = 1.e-4, check.names = FALSE))
  expect_equivalent(names(cout), c("Sepal.Length", "Sepal.Width",
				   "Petal.Length", "Petal.Width"))

  cout <- cas.css(i2, na.rm = TRUE)
  css1 <- c(102.16833, 28.30693, 464.32540, 86.56993)
  expect_that(cout, is_a("numeric"))
  expect_true(all.equal(cout, css1, check.names = FALSE, tolerance = 1.e-4))
  expect_equivalent(names(cout), c("Sepal.Length", "Sepal.Width",
				   "Petal.Length", "Petal.Width"))

  # TODO: Verify correct values

  cout <- cas.css(ct0)
  css1 <- c(NA, NA, 27165.2, NA)
  expect_that(cout, is_a("numeric"))
  #expect_true(all.equal(cout, css1, tolerance = 1.e-4, check.names = FALSE))
  expect_equivalent(names(cout), c("n1", "n2", "n3", "n4"))

  cout <- cas.css(ct0, na.rm = TRUE)
  css1.na.rm <- c(282513.2, 10354.0, 27165.2, 978638.8)
  expect_that(cout, is_a("numeric"))
  #expect_true(all.equal(cout, css1.na.rm, tolerance = 1.e-4, check.names = FALSE))
  expect_equivalent(names(cout), c("n1", "n2", "n3", "n4"))
})
