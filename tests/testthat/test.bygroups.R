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


context("test.bygroups.R")

test_that("bygroups", {
  options("cas.bygroup.mode" = "raw", "cas.bygroup.dup.suffix" = "_f")

  stats <- c("Min", "Max", "Mean")

  cols <- c("Column", "Min", "Max", "Mean")
  tsumm <- cas.simple.summary(caz, table = t, subset = stats)
  expect_equivalent(length(tsumm), 1)
  expect_true(!is.null(tsumm$Summary))
  expect_equivalent(names(tsumm$Summary), cols)

  cols <- c("Sex", "Column", "Min", "Max", "Mean")
  t2 <- cas.copy(t)
  t2@groupby <- "Sex"
  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 3)
  expect_true(!is.null(t2summ$ByGroupInfo))
  expect_true(!is.null(t2summ$ByGroup1.Summary))
  expect_true(!is.null(t2summ$ByGroup2.Summary))
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)
  expect_equivalent(names(t2summ$ByGroup2.Summary), cols)
  expect_equivalent(t2summ$ByGroupInfo$Sex, c("female", "male"))
})

test_that("byvars and byvars", {
  options("cas.bygroup.mode" = "raw", "cas.bygroup.dup.suffix" = "_f")

  stats <- c("Min", "Max", "Mean")
  cols <- c("Sex", "Column", "Min", "Max", "Mean")
  t2 <- cas.copy(t)

  t2@groupby <- "Sex"
  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 3)
  expect_equivalent(byvars(t2summ$ByGroup1.Summary), "Sex")
  expect_equivalent(byvars(t2summ$ByGroup2.Summary), "Sex")
  expect_equivalent(byvals(t2summ$ByGroup1.Summary), "female")
  expect_equivalent(byvals(t2summ$ByGroup2.Summary), "male")

  t2@groupby <- c("Pclass", "Sex")
  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 7)
  expect_equivalent(byvars(t2summ$ByGroup1.Summary), c("Pclass", "Sex"))
  expect_equivalent(byvals(t2summ$ByGroup1.Summary), list(1, "female"))
  expect_equivalent(byvars(t2summ$ByGroup2.Summary), c("Pclass", "Sex"))
  expect_equivalent(byvals(t2summ$ByGroup2.Summary), list(1, "male"))
  expect_equivalent(byvars(t2summ$ByGroup3.Summary), c("Pclass", "Sex"))
  expect_equivalent(byvals(t2summ$ByGroup3.Summary), list(2, "female"))
  expect_equivalent(byvars(t2summ$ByGroup4.Summary), c("Pclass", "Sex"))
  expect_equivalent(byvals(t2summ$ByGroup4.Summary), list(2, "male"))
  expect_equivalent(byvars(t2summ$ByGroup5.Summary), c("Pclass", "Sex"))
  expect_equivalent(byvals(t2summ$ByGroup5.Summary), list(3, "female"))
  expect_equivalent(byvars(t2summ$ByGroup6.Summary), c("Pclass", "Sex"))
  expect_equivalent(byvals(t2summ$ByGroup6.Summary), list(3, "male"))

  # No bygroups
  t2@groupby <- NULL
  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 1)
  expect_true(!is.null(t2summ$Summary))
  expect_true(is.null(t2summ$ByGroup1.Summary))
  expect_equivalent(byvars(t2summ$Summary), character())
  expect_equivalent(byvals(t2summ$Summary), character())
})

test_that("cas.bygroup.mode", {
  stats <- c("Min", "Max", "Mean")
  cols <- c("Pclass", "Sex", "Column", "Min", "Max", "Mean")
  t2 <- cas.copy(t)
  t2@groupby <- c("Pclass", "Sex")

  options("cas.bygroup.mode" = "raw", "cas.bygroup.dup.suffix" = "_f")

  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 7)
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)
  expect_that(t2summ$ByGroup1.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup1.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup2.Summary), cols)
  expect_that(t2summ$ByGroup2.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup2.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup3.Summary), cols)
  expect_that(t2summ$ByGroup3.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup3.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup4.Summary), cols)
  expect_that(t2summ$ByGroup4.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup4.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup5.Summary), cols)
  expect_that(t2summ$ByGroup5.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup5.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup6.Summary), cols)
  expect_that(t2summ$ByGroup6.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup6.Summary$Sex, is_a("character"))

  options("cas.bygroup.mode" = "formatted")

  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 7)
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)
  expect_that(t2summ$ByGroup1.Summary$Pclass, is_a("character"))
  expect_that(t2summ$ByGroup1.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup2.Summary), cols)
  expect_that(t2summ$ByGroup2.Summary$Pclass, is_a("character"))
  expect_that(t2summ$ByGroup2.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup3.Summary), cols)
  expect_that(t2summ$ByGroup3.Summary$Pclass, is_a("character"))
  expect_that(t2summ$ByGroup3.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup4.Summary), cols)
  expect_that(t2summ$ByGroup4.Summary$Pclass, is_a("character"))
  expect_that(t2summ$ByGroup4.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup5.Summary), cols)
  expect_that(t2summ$ByGroup5.Summary$Pclass, is_a("character"))
  expect_that(t2summ$ByGroup5.Summary$Sex, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup6.Summary), cols)
  expect_that(t2summ$ByGroup6.Summary$Pclass, is_a("character"))
  expect_that(t2summ$ByGroup6.Summary$Sex, is_a("character"))

  options("cas.bygroup.mode" = "both")

  cols <- c("Pclass", "Pclass_f", "Sex", "Sex_f", "Column", "Min", "Max", "Mean")

  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 7)
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)
  expect_that(t2summ$ByGroup1.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup1.Summary$Pclass_f, is_a("character"))
  expect_that(t2summ$ByGroup1.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup1.Summary$Sex_f, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup2.Summary), cols)
  expect_that(t2summ$ByGroup2.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup2.Summary$Pclass_f, is_a("character"))
  expect_that(t2summ$ByGroup2.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup2.Summary$Sex_f, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup3.Summary), cols)
  expect_that(t2summ$ByGroup3.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup3.Summary$Pclass_f, is_a("character"))
  expect_that(t2summ$ByGroup3.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup3.Summary$Sex_f, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup4.Summary), cols)
  expect_that(t2summ$ByGroup4.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup4.Summary$Pclass_f, is_a("character"))
  expect_that(t2summ$ByGroup4.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup4.Summary$Sex_f, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup5.Summary), cols)
  expect_that(t2summ$ByGroup5.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup5.Summary$Pclass_f, is_a("character"))
  expect_that(t2summ$ByGroup5.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup5.Summary$Sex_f, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup6.Summary), cols)
  expect_that(t2summ$ByGroup6.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup6.Summary$Pclass_f, is_a("character"))
  expect_that(t2summ$ByGroup6.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup6.Summary$Sex_f, is_a("character"))

  options("cas.bygroup.mode" = "both", "cas.bygroup.dup.suffix" = "2")

  cols <- c("Pclass", "Pclass2", "Sex", "Sex2", "Column", "Min", "Max", "Mean")

  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 7)
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)
  expect_that(t2summ$ByGroup1.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup1.Summary$Pclass2, is_a("character"))
  expect_that(t2summ$ByGroup1.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup1.Summary$Sex2, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup2.Summary), cols)
  expect_that(t2summ$ByGroup2.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup2.Summary$Pclass2, is_a("character"))
  expect_that(t2summ$ByGroup2.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup2.Summary$Sex2, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup3.Summary), cols)
  expect_that(t2summ$ByGroup3.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup3.Summary$Pclass2, is_a("character"))
  expect_that(t2summ$ByGroup3.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup3.Summary$Sex2, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup4.Summary), cols)
  expect_that(t2summ$ByGroup4.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup4.Summary$Pclass2, is_a("character"))
  expect_that(t2summ$ByGroup4.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup4.Summary$Sex2, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup5.Summary), cols)
  expect_that(t2summ$ByGroup5.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup5.Summary$Pclass2, is_a("character"))
  expect_that(t2summ$ByGroup5.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup5.Summary$Sex2, is_a("character"))
  expect_equivalent(names(t2summ$ByGroup6.Summary), cols)
  expect_that(t2summ$ByGroup6.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup6.Summary$Pclass2, is_a("character"))
  expect_that(t2summ$ByGroup6.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup6.Summary$Sex2, is_a("character"))

  options("cas.bygroup.mode" = "none")

  cols <- c("Column", "Min", "Max", "Mean")

  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 7)
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)
  expect_true(is.null(t2summ$ByGroup1.Summary$Pclass))
  expect_true(is.null(t2summ$ByGroup1.Summary$Sex))
  expect_equivalent(names(t2summ$ByGroup2.Summary), cols)
  expect_true(is.null(t2summ$ByGroup2.Summary$Pclass))
  expect_true(is.null(t2summ$ByGroup2.Summary$Sex))
  expect_equivalent(names(t2summ$ByGroup3.Summary), cols)
  expect_true(is.null(t2summ$ByGroup2.Summary$Pclass))
  expect_true(is.null(t2summ$ByGroup2.Summary$Sex))
  expect_equivalent(names(t2summ$ByGroup5.Summary), cols)
  expect_true(is.null(t2summ$ByGroup2.Summary$Pclass))
  expect_true(is.null(t2summ$ByGroup2.Summary$Sex))
  expect_equivalent(names(t2summ$ByGroup6.Summary), cols)
  expect_true(is.null(t2summ$ByGroup2.Summary$Pclass))
  expect_true(is.null(t2summ$ByGroup2.Summary$Sex))

  options("cas.bygroup.mode" = "raw", "cas.bygroup.dup.suffix" = "_f")
})

test_that("bygroups.as.columns", {
  stats <- c("Min", "Max", "Mean")
  t2 <- cas.copy(t)
  t2@groupby <- c("Pclass", "Sex")

  options("cas.bygroup.mode" = "none", "cas.bygroup.dup.suffix" = "_f")

  cols <- c("Column", "Min", "Max", "Mean")
  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)

  cols <- c("Pclass", "Sex", "Column", "Min", "Max", "Mean")
  t3 <- bygroups.as.columns(t2summ$ByGroup1.Summary)
  expect_equivalent(names(t3), cols)

  options("cas.bygroup.mode" = "raw", "cas.bygroup.dup.suffix" = "_f")

  # No bygroups
  t2@groupby <- NULL
  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_true(!is.null(t2summ$Summary))
  expect_true(is.null(t2summ$ByGroup1.Summary))
  expect_equivalent(t2summ$Summary, bygroups.as.columns(t2summ$Summary))
})


test_that("rbind.bygroups", {
  stats <- c("Min", "Max", "Mean")
  t2 <- cas.copy(t)
  t2@groupby <- c("Pclass", "Sex")

  options("cas.bygroup.mode" = "raw", "cas.bygroup.dup.suffix" = "_f")

  cols <- c("Pclass", "Sex", "Column", "Min", "Max", "Mean")

  t2summ <- cas.simple.summary(caz, table = t2, subset = stats)
  expect_equivalent(length(t2summ), 7)
  expect_equivalent(names(t2summ$ByGroup1.Summary), cols)
  expect_true(is.null(t2summ$Summary))
  expect_that(t2summ$ByGroup1.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup1.Summary$Sex, is_a("character"))
  expect_that(t2summ$ByGroup2.Summary$Pclass, is_a("numeric"))
  expect_that(t2summ$ByGroup2.Summary$Sex, is_a("character"))

  nrows <- sum(sapply(t2summ, nrow)) - nrow(t2summ$ByGroupInfo)
  expect_true(nrows > 0)

  t2rbind <- rbind.bygroups(t2summ)
  expect_equivalent(length(t2rbind), 1)
  expect_false(is.null(t2rbind$Summary))
  expect_equivalent(names(t2rbind$Summary), cols)
  expect_true(is.null(t2rbind$ByGroup1.Summary))
  expect_true(is.null(t2rbind$ByGroup2.Summary))
  expect_equivalent(nrows, nrow(t2rbind$Summary))

  # Not a CAS result set
  x <- list(a = "foo", b = "bar")
  y <- rbind.bygroups(x)
  expect_equivalent(y, x)
})
