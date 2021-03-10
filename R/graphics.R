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


setMethod(
  "plot",
  signature(x = "CASTable"),
  function(x, y, max.rows = getOption("cas.max.download.rows"), ...) {
    stopifnot(class(y) == "CASTable" & x@tname == y@tname)

    vars <- c(x@names, y@names)
    vars <- vars[vars != ""]
    if (length(vars) == 0) {
      vars <- ""
    }

    cvars <- c(x@computedVars, y@computedVars)
    cvars <- cvars[cvars != ""]
    if (length(cvars) == 0) {
      cvars <- ""
      cpgm <- ""
    }
    else {
      cpgm <- c(x@computedVarsProgram, y@computedVarsProgram)
      cpgm <- cpgm[cpgm != ""]
      if (length(cpgm) > 1) {
        cpgm <- paste(cpgm, collapse = ";")
      }
    }

    v2 <- x
    v2@names <- c(vars, cvars)
    v2@names <- v2@names[v2@names != ""]
    v2@computedVars <- cvars
    v2@computedVarsProgram <- cpgm

    t1 <- as.data.frame(v2, max.rows = max.rows)

    return(plot(t1, ...))
  }
)
