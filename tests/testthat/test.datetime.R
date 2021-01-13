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


context("test.datetime.R")

test_that("test.cas.datetime", {
  expect_equal(class(CASdt.as.POSIXct("315662400000000")), c("POSIXct", "POSIXt"))
  expect_equal(
    CASdt.as.POSIXct("315662400000000"),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    CASdt.as.POSIXct(315662400000000),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(class(CASdt.as.POSIXlt("315662400000000")), c("POSIXlt", "POSIXt"))
  expect_equal(
    CASdt.as.POSIXlt("315662400000000"),
    as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    CASdt.as.POSIXlt(315662400000000),
    as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(CASdt.as.SASdt("315662400000000"), 315662400)
  expect_equal(CASdt.as.SASdt(315662400000000), 315662400)

  expect_equal(class(CASd.as.Date(3653)), "Date")
  expect_equal(
    CASd.as.Date(3653),
    as.Date(as.POSIXlt(origin = "1970-01-01", tz = "UTC", 0))
  )
  expect_equal(CASd.as.SASd(3653), 3653)

  expect_equal(class(CASt.as.POSIXct("43200000000")), c("POSIXct", "POSIXt"))
  expect_equal(
    CASt.as.POSIXct("43200000000"),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    CASt.as.POSIXct(43200000000),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(class(CASt.as.POSIXlt("43200000000")), c("POSIXlt", "POSIXt"))
  expect_equal(
    CASt.as.POSIXlt("43200000000"),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    CASt.as.POSIXlt(43200000000),
    as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(CASt.as.SASt("43200000000"), 43200)
  expect_equal(CASt.as.SASt(43200000000), 43200)
})

test_that("test.r2cas", {
  expect_equal(
    POSIXlt.as.CASdt(as.POSIXlt(strptime("1970-01-01 12:00:00",
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ))),
    315662400000000
  )
  expect_equal(
    POSIXct.as.CASdt(as.POSIXct(strptime("1970-01-01 12:00:00",
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ))),
    315662400000000
  )

  expect_equal(Date.as.CASd(as.Date(as.POSIXlt(origin = "1970-01-01", tz = "UTC", 0))), 3653)
})

test_that("test.sas.datetime", {
  expect_equal(class(SASdt.as.POSIXct("315662400")), c("POSIXct", "POSIXt"))
  expect_equal(
    SASdt.as.POSIXct("315662400"),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    SASdt.as.POSIXct(315662400),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(class(SASdt.as.POSIXlt("315662400")), c("POSIXlt", "POSIXt"))
  expect_equal(
    SASdt.as.POSIXlt("315662400"),
    as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    SASdt.as.POSIXlt(315662400),
    as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(SASdt.as.CASdt("315662400"), 315662400000000)
  expect_equal(SASdt.as.CASdt(315662400), 315662400000000)

  expect_equal(class(SASd.as.Date(3653)), "Date")
  expect_equal(
    SASd.as.Date(3653),
    as.Date(as.POSIXlt(origin = "1970-01-01", tz = "UTC", 0))
  )
  expect_equal(SASd.as.CASd(3653), 3653)

  expect_equal(class(SASt.as.POSIXct("43200")), c("POSIXct", "POSIXt"))
  expect_equal(
    SASt.as.POSIXct("43200"),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    SASt.as.POSIXct(43200),
    as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(class(SASt.as.POSIXlt("43200")), c("POSIXlt", "POSIXt"))
  expect_equal(
    SASt.as.POSIXlt("43200"),
    as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )
  expect_equal(
    SASt.as.POSIXlt(43200),
    as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

  expect_equal(SASt.as.CASt("43200"), 43200000000)
  expect_equal(SASt.as.CASt(43200), 43200000000)
})

test_that("test.r2sas", {
  expect_equal(
    POSIXlt.as.SASdt(as.POSIXlt(strptime("1970-01-01 12:00:00",
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ))),
    315662400
  )
  expect_equal(
    POSIXct.as.SASdt(as.POSIXct(strptime("1970-01-01 12:00:00",
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ))),
    315662400
  )

  expect_equal(Date.as.SASd(as.Date(as.POSIXlt(origin = "1970-01-01", tz = "UTC", 0))), 3653)
})
