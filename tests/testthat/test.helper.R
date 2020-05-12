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

options(cas.print.messages=FALSE)


context("test.helper.R")

test_that("casFormula Functions", {
  t1 <- y ~ x1
  casFormula(t1)
  expect_that(casFormula(t1), is_a("list"))
  
  t2 <- y ~ x1 + x2
  expect_that(casFormula(t2), is_a("list"))
  
  t3 <- y ~ x1 + x2 + x3
  foo <- casFormula(t3)
  valid <- list(as.name('y'),c('x1','x2','x3'))
  expect_equivalent(foo[[1]], as.name('y'))
  expect_equivalent(foo, valid)
  
  
  t4 <- y ~ x1 + x2 * x3
  expect_error(casFormula(t4))
  
  t5 <- ~ x1 + x2 * x3
  expect_error(casFormula(t5))
})

test_that("help function",{
  #expect_message(help(cas.aStore.describe),regexp = "Using")
  #expect_message(help(cas.network.readGraph),regexp = "Using")
  expect_silent(help(cas.foo.bar))
})
