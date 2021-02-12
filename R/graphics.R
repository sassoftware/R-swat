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
  function(x, y, ...) {
    stopifnot(class(y) == "CASTable" & x@tname == y@tname)
    download_obs <- as.numeric(getOption("cas.max.download.rows"))
    # Did the user override the download size
    allargs <- list(...)
    for (i in seq_len(length(allargs))) {
      if ("downloadObs" %in% attributes(allargs)$names) {
        download_obs <- allargs$downloadObs
      }
    }

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

    # If the number of observations is less than download size
    if (nrow(x) <= download_obs) {
      t1 <- as.data.frame(v2)
      return(plot(t1, ... = ...))
    }
    else {
      # Sample rows
      if (length(x@groupby)) { # SRS
        name <- uniqueTableName(x@tname)
        res <- cas.retrieve(x@conn, "sampling.srs",
          stop.on.error = TRUE,
          samppct = eval(download_obs / nrow(x) * 100),
          table = x@tname,
          output = list(casOut = list(
            name = name,
            replace = TRUE
          ), copyvars = list(vars))
        )$results
        srs <- CASTable(x@conn, name,
          columns = vars, where = x@where,
          orderby = x@orderby, groupby = x@groupby, gbmode = x@gbmode
        )
        m1 <- paste("Sampling (SRS) was done prior to", sys.call(1),
                    "because the nrows in", nrow(x),
                    "which is greater than the max download size of", download_obs)
        cat(m1[1])
        srs2 <- as.data.frame(srs, obs = eval(nrow(srs)))
        return(plot(srs2, ... = ...))
      }
      else { # Stratified
        name <- uniqueTableName(x@tname)
        res <- cas.retrieve(x@conn, "sampling.srs",
          stop.on.error = TRUE,
          samppct = eval(download_obs / nrow(x) * 100),
          table = x@tname,
          output = list(casOut = list(
            name = name,
            replace = TRUE
          ), copyvars = list(vars))
        )$results
        srs <- CASTable(x@conn, name,
          columns = vars, where = x@where,
          orderby = x@orderby, groupby = x@groupby, gbmode = x@gbmode
        )
        m1 <- paste("Sampling (Stratified) was done prior to", sys.call(1),
                    "because the nrows in", nrow(x),
                    "which is greater than the max download size of", download_obs)
        cat(m1[1])
        srs2 <- as.data.frame(srs, obs = eval(nrow(srs)))
        return(plot(srs2, ... = ...))
      }
    }
  }
)
