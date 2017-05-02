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



setMethod("plot",
    signature(x = "CASTable"),
    function (x, y, ...) 
    {
      stopifnot(class(y)=='CASTable' & x@tname==y@tname)
      downloadObs <- as.numeric(getOption('cas.max.download.rows'))
      # did the user override the download size
      allargs <- list(...)
      for (i in seq(length(allargs))){
        if ("downloadObs" %in% attributes(allargs)$names) {
          downloadObs <- allargs$downloadObs
        }
      }

      vars = c(x@names, y@names)
      vars = vars[vars != ""]
      if (length(vars) == 0)
         vars = ""
      cvars = c(x@computedVars, y@computedVars)
      cvars = cvars[cvars != ""]
      if (length(cvars) == 0)
         {
         cvars = ""
         cpgm  = ""
         }
      else
         {
         cpgm = c(x@computedVarsProgram, y@computedVarsProgram)
         cpgm = cpgm[cpgm != ""]
         if (length(cpgm) > 1)
            cpgm = paste(cpgm, collapse=';')
         }
      v2 <- x
      v2@names = c(vars, cvars)
      v2@names = v2@names[v2@names != ""]
      v2@computedVars        = cvars
      v2@computedVarsProgram = cpgm


      # if the number of observations is less than download size
      if (nrow(x) <= downloadObs){
        t1 <-to.casDataFrame(v2)
        return(plot(t1,...=...))
      }
      else{
        # sample rows
        if (nchar(x@groupby)==0){ # SRS
          name <- paste(x@tname, random::randomStrings(n = 1, len = 9, unique = TRUE), sep='')
          res <- runAction(x@conn, "sampling.srs", 
                            samppct=eval(downloadObs/nrow(x)*100),
                            table=x@tname,
                            output=list(casOut=list(name=name, 
                                                    replace=TRUE), copyvars=list(vars))
                            )
          check_for_cas_errors(res)
          srs <- defCasTable(x@conn, name, columns = vars, where = x@where, 
                              orderby = x@orderby, groupby = x@groupby, gbmode = x@gbmode)
          m1 <-paste("sampling (SRS) was done prior to", sys.call(1), "because the nrows in", nrow(x), "which is greater than the max download size of", downloadObs)
          cat(m1[1])
          srs2 <- to.casDataFrame(srs, obs=eval(nrow(srs)))
          return(plot(srs2, ... = ...))
        }
        else { # Stratified
          name <- paste(x@tname, random::randomStrings(n = 1, len = 9, unique = TRUE), sep='')
          res <- runAction(x@conn, "sampling.srs", 
                            samppct=eval(downloadObs/nrow(x)*100),
                            table=x@tname,
                            output=list(casOut=list(name=name, 
                                                    replace=TRUE), copyvars=list(vars))
          )
          check_for_cas_errors(res)
          srs <- defCasTable(x@conn, name, columns = vars, where = x@where, 
                              orderby = x@orderby, groupby = x@groupby, gbmode = x@gbmode)
          m1 <-paste("sampling (Stratified) was done prior to", sys.call(1), "because the nrows in", nrow(x), "which is greater than the max download size of", downloadObs)
          cat(m1[1])
          srs2 <- to.casDataFrame(srs, obs=eval(nrow(srs)))
          return(plot(srs2, ... = ...))
          
        }
      }
    }
)
