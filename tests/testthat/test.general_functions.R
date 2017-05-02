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



context("general_functions")

orig_options <- options()
options(cas.print.messages=TRUE)

test_that("Load ActionSet and List ActionSet Functions", {
  expect_that(swat::loadActionSet(caz,"foobar"), testthat::throws_error())
  expect_null(swat::loadActionSet(caz,"builtins"))
  #testthat::expect_message(swat::load.actionset(s,"foobar"), "ERROR:")
  # Notes have been removed by default
  #testthat::expect_message(swat::load.actionset(s,"network"), "NOTE: Added action set 'network'.")
  
  expect_message(loadActionSet(caz, actionSet = "simple"), "Added action set 'simple")
  expect_true("simple" %in% listActionSets(caz)$actionset)
  expect_output(listActionParms(caz, actn="summary"), 'cas.summary')
})
options(orig_options)

test_that("Test that the class for R-SWAT objects is returned correctly", {
  expect_is(caz, "CAS")
  expect_is(ct, "CASTable")
  expect_is(to.casDataFrame(ct), "casDataFrame")
})
