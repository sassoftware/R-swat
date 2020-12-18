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


context("test.rbind.R")

test_that("test that rbind basic functionality works", {
  expect_is(ct1, "CASTable")
  expect_is(ct2, "CASTable")

  expect_that(rbind(ct1, ct2), is_a("CASTable"))
})

test_that("test that rbind  work for R data frames.", {
  expect_is(df1, "data.frame")
  expect_is(df2, "data.frame")

  expect_that(rbind(df1, df2), is_a("data.frame"))

  expect_is(ct1, "CASTable")
  expect_is(ct2, "CASTable")

  expect_equivalent(colSums(rbind(ct1, ct2)), colSums(rbind(df1, df2)))
  expect_error(rbind(df1, ct2))
})

test_that("test that rbind works with assignment", {
  nct <- rbind(ct1, ct2)
  expect_that(nct, is_a("CASTable"))
  expect_equivalent(rbind(ct1, ct2), nct)
})
