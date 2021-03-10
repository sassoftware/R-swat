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
library(testthat)

options(cas.print.messages = FALSE)


context("test.action.R")

test_that("cas.gen.function.sig", {
  options(cas.gen.function.sig = TRUE)
  swat:::.gen_functions(caz, "simple")
  options(cas.gen.function.sig = FALSE)

  out <- paste(capture.output(args(cas.simple.summary)), collapse = "")
  expect_match(out, " table, ")
  expect_match(out, " inputs = NULL, ")
  expect_match(out, " orderBy = NULL, ")
  expect_match(out, " ciAlpha = 0\\.05, ")
  expect_match(out, " subSet = NULL, ")
  expect_match(out, " \\.\\.\\.)")
})
