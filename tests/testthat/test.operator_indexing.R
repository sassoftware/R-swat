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


context("test.operatorindexing.R")

test_that("test that multi-column referencing with : works", {
  expect_equivalent(colSums(ct[1:4]), colSums(df[1:4]))
  expect_equivalent(median(ct[2]), median(data.matrix(df[2])))
  expect_equivalent(sum(ct[2:3]), sum(sapply(df[2:3], sum)))
})

test_that("test that multi-comlumn referencing with vector works", {
  expect_equivalent(colMeans(ct[c(1, 3, 4)]), colMeans(df[c(1, 3, 4)]))
  expect_equivalent(colSums(ct[c(2, 3)]), colSums(df[c(2, 3)]))
})

test_that("test that referencing with $ operator works", {
  expect_equivalent(colSums(ct$n3), colSums(data.matrix(df$n3)))
})

test_that("test that referencing with column names works", {
  ct13 <- ct[c("n1", "n3")]
  expect_equivalent(ct[c("n1", "n3")], ct13)

  colSums(ct13)
  colMeans(ct13)

  colSums(ct13[1])
  colMeans(ct13[2])

  expect_equivalent(max(ct[[1]]), max(df[[1]]))
  expect_equivalent(max(ct[["n4"]]), max(df[["n4"]]))
})

# Row indexing is not currenly supported
