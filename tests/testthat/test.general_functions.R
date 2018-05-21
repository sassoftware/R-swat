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


expect_cas_message <- function(caz, regexp) {
  if ( length(caz$messages) == 0 ) {
     return(sprintf("CAS did not produce any messages."))
  }

  for ( i in 1:length(caz$messages) ) {
     if ( grepl(regexp, caz$messages[i], perl = TRUE) ) {
        expect(TRUE, '')
        return(invisible(caz))
     }
  }

  expect(FALSE, sprintf('%s did not match any CAS message.\n%s',
                        encodeString(regexp),
                        paste0('Actual values: ', paste0('', caz$messages, collapse = '\n'))))
  invisible(caz)
}


context("general_functions")

orig_options <- options()
options(cas.print.messages=TRUE)

test_that("Load ActionSet and List ActionSet Functions", {
  expect_that(swat::loadActionSet(caz,"foobar"), testthat::throws_error())
  expect_null(swat::loadActionSet(caz,"builtins"))
  #testthat::expect_cas_message(swat::load.actionset(s,"foobar"), "ERROR:")
  # Notes have been removed by default
  #testthat::expect_cas_message(swat::load.actionset(s,"network"), "NOTE: Added action set 'network'.")
  
  loadActionSet(caz, actionSet = "simple")
  expect_cas_message(caz, "NOTE:\\s+simple")
  expect_true("simple" %in% listActionSets(caz)$actionset)
  listActionParms(caz, actn="summary")
  expect_cas_message(caz, 'cas.summary')
})
options(orig_options)

test_that("Test that the class for R-SWAT objects is returned correctly", {
  expect_is(caz, "CAS")
  expect_is(ct, "CASTable")
  expect_is(to.casDataFrame(ct), "casDataFrame")
})
