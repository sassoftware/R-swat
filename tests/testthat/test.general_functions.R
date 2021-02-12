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


context("general_functions")

test_that("Load ActionSet and List ActionSet Functions", {
  cas.retrieve(caz, "builtins.loadactionset", actionset = "simple")
  expect_true("simple" %in% swat:::.list_action_sets(caz)$actionset)

  p <- swat:::.list_action_params(caz, action = "summary", display = FALSE)
  expect_true(length(p) > 15)
  expect_equivalent(p[[1]]$name, "table")
  expect_equivalent(p[[1]]$parmType, "value_list")
})

test_that("Test that the class for R-SWAT objects is returned correctly", {
  expect_is(caz, "CAS")
  expect_is(ct, "CASTable")
  expect_is(as.data.frame(ct), "data.frame")
})
