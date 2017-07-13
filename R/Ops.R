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






#' @rdname CASTable-Extract
#' @export
#' @rawRd % Copyright SAS Institute
.cas.arith <- function(e1, op, e2) {
         if (class(e1) == "CASTable")
            {
            rct = new("CASTable", e1@conn, e1@tname, e1@caslib, e1@names)
            rct@compcomp = e1@compcomp
            if (nchar(e1@XcomputedVarsProgram))
               {
               e1p                     = e1@XcomputedVarsProgram
               rct@XcomputedVars       = e1@XcomputedVars
               rct@computedVars        = e1@computedVars
               rct@computedVarsProgram = e1@computedVarsProgram
               }
            else
               {
               e1p = c(e1@names, e1@computedVars)
               e1p = e1p[e1p != ""]
               e1p = paste('"', e1p, '"n', sep='')
               if (sum(nchar(e1@computedVars)))
                  {
                  rct@compcomp            = TRUE
                  rct@XcomputedVars       = c(e1@XcomputedVars, e1@computedVars) 
                  rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
                  rct@computedVars        = e1@computedVars
                  rct@computedVarsProgram = e1@computedVarsProgram
                  }
               }
            }
         else
            {
            rct = new("CASTable", e2@conn, e2@tname, e2@caslib, e2@names)
            rct@compcomp = e2@compcomp
            e1p = e1
            }

         if (class(e2) == "CASTable")
            {
            if (nchar(e2@XcomputedVarsProgram))
               {
               e2p                     = e2@XcomputedVarsProgram
               rct@XcomputedVars       = c(rct@XcomputedVars, e2@XcomputedVars)
               rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
               rct@computedVars        = c(rct@computedVars, e2@computedVars)
               rct@computedVars        = rct@computedVars[rct@computedVars != ""]
               rct@computedVarsProgram = c(rct@computedVarsProgram, e2@computedVarsProgram)
               rct@computedVarsProgram = rct@computedVarsProgram[rct@computedVarsProgram != ""]
               }
            else
               {
               e2p = c(e2@names, e2@computedVars)
               e2p = e2p[e2p != ""]
               e2p = paste('"', e2p, '"n', sep='')
               if (sum(nchar(e2@computedVars)))
                  {
                  rct@compcomp            = TRUE
                  rct@XcomputedVars       = c(rct@XcomputedVars, e2@XcomputedVars, e2@computedVars) 
                  rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
                  rct@computedVars        = c(rct@computedVars, e2@computedVars)
                  rct@computedVars        = rct@computedVars[rct@computedVars != ""]
                  rct@computedVarsProgram = c(rct@computedVarsProgram, e2@computedVarsProgram)
                  rct@computedVarsProgram = rct@computedVarsProgram[rct@computedVarsProgram != ""]
                  }
               }
            }
         else
            {
            e2p = e2
            }

         if (op == " %% ")
            rct@XcomputedVarsProgram = paste('mod(', e1p, ', ', e2p, ')', sep='')
         else
            if (op == " %/% ")
               rct@XcomputedVarsProgram = paste('floor(', e1p, ' / ', e2p, ')', sep='')
            else
               rct@XcomputedVarsProgram = paste('(', e1p, op, e2p, ')', sep='')
         return(rct)
         }



#' @rdname CASTable-Extract
#' @export
setMethod("-",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' - ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("-",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.arith(e1, ' - ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("-",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' - ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("+",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' + ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("+",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.arith(e1, ' + ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("+",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' + ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("/",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' / ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("/",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.arith(e1, ' / ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("/",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' / ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("*",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' * ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("*",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.arith(e1, ' * ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("*",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' * ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("^",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' ** ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("^",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.arith(e1, ' ** ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("^",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' ** ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("%%",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' %% ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("%%",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.arith(e1, ' %% ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("%%",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' %% ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("%/%",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' %/% ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("%/%",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.arith(e1, ' %/% ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("%/%",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.arith(e1, ' %/% ', e2))
          })










#' @rdname CASTable-Extract
#' @export
.cas.compare <- function(e1, op, e2) {
         if (class(e1) == "CASTable")
            {
            rct = new("CASTable", e1@conn, e1@tname, e1@caslib, e1@names)
            rct@compcomp = e1@compcomp
            if (nchar(e1@XcomputedVarsProgram))
               {
               e1p                     = e1@XcomputedVarsProgram
               rct@XcomputedVars       = e1@XcomputedVars
               rct@computedVars        = e1@computedVars
               rct@computedVarsProgram = e1@computedVarsProgram
               }
            else
               {
               e1p = c(e1@names, e1@computedVars)
               e1p = e1p[e1p != ""]
               e1p = paste('"', e1p, '"n', sep='')
               if (sum(nchar(e1@computedVars)))
                  {
                  rct@compcomp            = TRUE
                  rct@XcomputedVars       = c(e1@XcomputedVars, e1@computedVars) 
                  rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
                  rct@computedVars        = e1@computedVars
                  rct@computedVarsProgram = e1@computedVarsProgram
                  }
               }
            }
         else
            {
            rct = new("CASTable", e2@conn, e2@tname, e2@caslib, e2@names)
            rct@compcomp = e2@compcomp
            if (class(e1) == "character")
               e1p = paste("'", e1, "'", sep='')
            else
               e1p = e1
            }

         if (class(e2) == "CASTable")
            {
            if (nchar(e2@XcomputedVarsProgram))
               {
               e2p                     = e2@XcomputedVarsProgram
               rct@XcomputedVars       = c(rct@XcomputedVars, e2@XcomputedVars)
               rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
               rct@computedVars        = c(rct@computedVars, e2@computedVars)
               rct@computedVars        = rct@computedVars[rct@computedVars != ""]
               rct@computedVarsProgram = c(rct@computedVarsProgram, e2@computedVarsProgram)
               rct@computedVarsProgram = rct@computedVarsProgram[rct@computedVarsProgram != ""]
               }
            else
               {
               e2p = c(e2@names, e2@computedVars)
               e2p = e2p[e2p != ""]
               e2p = paste('"', e2p, '"n', sep='')
               if (sum(nchar(e2@computedVars)))
                  {                  
                  rct@compcomp            = TRUE
                  rct@XcomputedVars       = c(rct@XcomputedVars, e2@XcomputedVars, e2@computedVars) 
                  rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
                  rct@computedVars        = c(rct@computedVars, e2@computedVars)
                  rct@computedVars        = rct@computedVars[rct@computedVars != ""]
                  rct@computedVarsProgram = c(rct@computedVarsProgram, e2@computedVarsProgram)
                  rct@computedVarsProgram = rct@computedVarsProgram[rct@computedVarsProgram != ""]
                  }
               }
            }
         else
            {
            if (class(e2) == "character")
               e2p = paste("'", e2, "'", sep='')
            else
               e2p = e2
            }

         rct@XcomputedVarsProgram = paste('(', e1p, op, e2p, ')', sep='')
         return(rct)
         }




#' @rdname CASTable-Extract
#' @export
setMethod(">",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' > ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod(">",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' > ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod(">",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' > ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("<",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' < ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("<",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' < ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("<",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' < ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod(">=",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' >= ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod(">=",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' >= ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod(">=",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' >= ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("<=",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' <= ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("<=",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' <= ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("<=",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' <= ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("==",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' = ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("==",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' = ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("==",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' = ', e2))
          })



#' @rdname CASTable-Extract
#' @export
setMethod("!=",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' ^= ', e2))
            return(rct)
          })

#' @rdname CASTable-Extract
#' @export
setMethod("!=",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' ^= ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("!=",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' ^= ', e2))
          })







#' @rdname CASTable-Extract
#' @export
.cas.logic <- function(e1, op, e2) {
         if (class(e1) == "CASTable")
            {
            rct = new("CASTable", e1@conn, e1@tname, e1@caslib, e1@names)
            rct@compcomp = e1@compcomp
            if (nchar(e1@XcomputedVarsProgram))
               {
               e1p                     = e1@XcomputedVarsProgram
               rct@XcomputedVars       = e1@XcomputedVars
               rct@computedVars        = e1@computedVars
               rct@computedVarsProgram = e1@computedVarsProgram
               }
            else
               {
               e1p = c(e1@names, e1@computedVars)
               e1p = e1p[e1p != ""]
               e1p = paste('"', e1p, '"n', sep='')
               if (sum(nchar(e1@computedVars)))
                  {
                  rct@compcomp            = TRUE
                  rct@XcomputedVars       = c(e1@XcomputedVars, e1@computedVars) 
                  rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
                  rct@computedVars        = e1@computedVars
                  rct@computedVarsProgram = e1@computedVarsProgram
                  }
               }
            }
         else
            {
            rct = new("CASTable", e2@conn, e2@tname, e2@caslib, e2@names)
            rct@compcomp = e2@compcomp
            e1p = e1
            }

         if (class(e2) == "CASTable")
            {
            if (nchar(e2@XcomputedVarsProgram))
               {
               e2p                     = e2@XcomputedVarsProgram
               rct@XcomputedVars       = c(rct@XcomputedVars, e2@XcomputedVars)
               rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
               rct@computedVars        = c(rct@computedVars, e2@computedVars)
               rct@computedVars        = rct@computedVars[rct@computedVars != ""]
               rct@computedVarsProgram = c(rct@computedVarsProgram, e2@computedVarsProgram)
               rct@computedVarsProgram = rct@computedVarsProgram[rct@computedVarsProgram != ""]
               }
            else
               {
               e2p = c(e2@names, e2@computedVars)
               e2p = e2p[e2p != ""]
               e2p = paste('"', e2p, '"n', sep='')
               if (sum(nchar(e2@computedVars)))
                  {
                  rct@compcomp            = TRUE
                  rct@XcomputedVars       = c(rct@XcomputedVars, e2@XcomputedVars, e2@computedVars) 
                  rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
                  rct@computedVars        = c(rct@computedVars, e2@computedVars)
                  rct@computedVars        = rct@computedVars[rct@computedVars != ""]
                  rct@computedVarsProgram = c(rct@computedVarsProgram, e2@computedVarsProgram)
                  rct@computedVarsProgram = rct@computedVarsProgram[rct@computedVarsProgram != ""]
                  }
               }
            }
         else
            {
            e2p = e2
            }

         rct@XcomputedVarsProgram = paste('(', e1p, op, e2p, ')', sep='')
         return(rct)
         }







#' @rdname CASTable-Extract
#' @export
setMethod("&",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' AND ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("&",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' AND ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("&",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' AND ', e2))
          })




#' @rdname CASTable-Extract
#' @export
setMethod("|",
          signature(e1 = "CASTable", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' OR ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("|",
          signature(e1 = "CASTable", e2 = "ANY"),
          function(e1, e2) {
            return(.cas.compare(e1, ' OR ', e2))
          })

#' @rdname CASTable-Extract
#' @export
setMethod("|",
          signature(e1 = "ANY", e2 = "CASTable"),
          function(e1, e2) {
            return(.cas.compare(e1, ' OR ', e2))
          })





#' @rdname CASTable-Extract
#' @export
setMethod("!",
          signature(x = "CASTable"),
          function(x) {
            rct = new("CASTable", x@conn, x@tname, x@caslib, x@names)
            rct@compcomp = e1@compcomp
            if (sum(nchar(x@XcomputedVarsProgram)))
               {
               e1p                     = x@XcomputedVarsProgram
               rct@XcomputedVars       = x@XcomputedVars
               rct@computedVars        = x@computedVars
               rct@computedVarsProgram = x@computedVarsProgram
               }
            else
               {
               e1p = c(x@names, x@computedVars)
               e1p = e1p[e1p != ""]
               if (sum(nchar(x@computedVars)))
                  {
                  rct@compcomp            = TRUE
                  rct@XcomputedVars       = c(x@XcomputedVars, x@computedVars) 
                  rct@XcomputedVars       = rct@XcomputedVars[rct@XcomputedVars != ""]
                  rct@computedVars        = x@computedVars
                  rct@computedVarsProgram = x@computedVarsProgram
                  }
               }
            rct@XcomputedVarsProgram = paste('NOT ', e1p, sep='')
            return(rct)
          })


