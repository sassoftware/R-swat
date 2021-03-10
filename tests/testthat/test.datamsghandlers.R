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


context("test.addtable.R")

test_that("cas.addtable", {
  if (PROTOCOL == "http" || PROTOCOL == "https") {
    skip("Not implemented in REST interface yet.")
  }

  dmh <- CASDataMsgHandler(iris)
  tbl <- cas.addtable(caz, dmh)
  expect_that(tbl, is_a("CASTable"))

  iris2 <- data.frame(iris)
  iris2$Species <- as.character(iris2$Species)
  expect_equivalent(.sorted_df(tbl, c("Sepal.Length", "Sepal.Width")),
                    .sorted_df(iris2, c("Sepal.Length", "Sepal.Width")))
})

test_that("test.addtable all types", {
  if (PROTOCOL == "http" || PROTOCOL == "https") {
    skip("Not implemented in REST interface yet.")
  }

  data <- data.frame(
    Char = as.character(c("abc", "def")),
    Double = as.numeric(c(123.45, 67890.12345)),
    Int32 = as.integer(c(12345, 67890123)),
#   Int64 = as.character(c("12345", "678901234567")),
    Date = as.Date(c("1960-01-01", "1972-12-25")),
    DatetimeCT = as.POSIXct(c("1960-01-01 05:06:07", "1972-12-25 12:00:00")),
    DatetimeLT = as.POSIXlt(c("1960-01-01 05:06:07", "1972-12-25 12:00:00"))
#   Binary = c(charToRaw("abc"), charToRaw("def"))
  )

  dmh <- CASDataMsgHandler(data)
  tbl <- cas.addtable(caz, dmh)
  expect_that(tbl, is_a("CASTable"))

  info <- cas.table.columnInfo(tbl)
  expect_equivalent(info$ColumnInfo$Column, 
                    c("Char", "Double", "Int32", "Date", "DatetimeCT", "DatetimeLT"))
  expect_equivalent(info$ColumnInfo$Type, 
                    c("varchar", "double", "int64", "date", "datetime", "datetime"))

  alldf <- as.data.frame(tbl)
  expect_that(alldf$Char, is_a("character"))
  expect_equivalent(alldf$Char, c("abc", "def"))
  expect_that(alldf$Double, is_a("numeric"))
  expect_equivalent(alldf$Double, as.numeric(c(123.45, 67890.12345)))
  expect_that(alldf$Int32, is_a("numeric"))
  expect_equivalent(alldf$Int32, as.integer(c(12345, 67890123)))
  expect_that(alldf$Date, is_a("Date"))
  expect_equivalent(alldf$Date, as.Date(c("1960-01-01", "1972-12-25")))
  expect_that(alldf$DatetimeCT, is_a("POSIXct"))
  expect_equivalent(alldf$DatetimeCT, as.POSIXct(c("1960-01-01 05:06:07", "1972-12-25 12:00:00")))
  expect_that(alldf$DatetimeLT, is_a("POSIXct"))
  expect_equivalent(alldf$DatetimeLT, as.POSIXlt(c("1960-01-01 05:06:07", "1972-12-25 12:00:00")))
})
