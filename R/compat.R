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


runAction <- function(cas.conn, action.name, ..., stop.on.error = TRUE) {
  return(cas.retrieve(cas.conn, action.name, ..., stop.on.error = stop.on.error))$results
}

loadActionSet <- function(cas.conn, action.set) {
  return(cas.retrieve(cas.conn, action.set))
}

listActionParms <- function(cas.conn, action.name) {
  return(NULL)
}

as.casTable <- function(cas.conn, frame) {
  return(as.CASTable(cas.conn, frame))
}
