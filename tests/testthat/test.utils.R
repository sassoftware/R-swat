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


context("test.utils.R")

test_that("cas.trace.actions", {
  if (caz$protocol == "cas") {
    skip("Only the HTTP interface can capture messages")
  }
  
  options(cas.trace.actions = TRUE, cas.trace.ui.actions = TRUE)

  expect_message(cas.simple.summary(caz, table = t, subset = c("min", "max")), "\\[simple\\.summary\\]")

  options(cas.trace.actions = FALSE, cas.trace.ui.actions = FALSE)
})
