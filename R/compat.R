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


runAction <- function(.x, .action, ..., stop.on.error = TRUE) {
  return(cas.retrieve(.x, .action, ..., stop.on.error = stop.on.error))$results
}

loadActionSet <- function(cas.conn, action.set) {
  return(cas.retrieve(cas.conn, action.set))
}

listActionParms <- function(cas.conn, .action) {
  return(NULL)
}

as.casTable <- function(cas.conn, frame, casOut = "") {
  name <- deparse(substitute(frame))
  if (casOut == "") {
    casOut <- name
  }
  else if (is.null(casOut$name) || casOut$name == "") {
    casOut$name <- name
  }
  return(as.CASTable(cas.conn, frame, casOut = casOut))
}
