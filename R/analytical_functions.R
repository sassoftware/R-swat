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


sasDecisionTree <- function(x, formula=NULL, target="", inputs="", nominals="",
                             crit="gain", maxlevel=6L, leafsize=5L, missing="useInSearch", assessment=TRUE, event=""){

  if (exists("formula") & class(formula)=='formula') {
    f1 <- swat::casFormula(formula, x)
    target <-toString(f1[1])
    inputs <-f1[[2]]
  }

  # Are there nominals
  if (length(nominals)>1 || nchar(nominals)>0){
    nom <- as.list(nominals)
    res <-runAction(x, "decisionTree.dtreeTrain", inputs=as.list(inputs), target=target, casout = list(name=paste(x@tname, "_modelout", sep=''), replace=TRUE),
                     encodeName=TRUE, nominals=nom, crit=crit, prune=TRUE, stat=TRUE, varImp=TRUE, missing=toupper(missing))
  }
  else {
    res <-runAction(x, "decisionTree.dtreeTrain", inputs=as.list(inputs), target=target,
                     crit=crit, prune=TRUE, varImp=TRUE, stat=TRUE, missing=toupper(missing))
  }
  check_for_cas_errors(res)
  res$call <- match.call()
  vars = c(x@names, x@computedVars)
  vars = vars[vars != ""]
  score <- runAction(x, "decisionTree.dtreeScore", 
                      target=target,
                      encodeName=TRUE,
                      copyVars=as.list(vars),
                      modelTable=paste(x@tname, "_modelout", sep=''),
                      casout = list(name=paste(x@tname, "_scoreout", sep=''), replace=TRUE))
  check_for_cas_errors(score)
  # define score CAS table
  score_out <- defCasTable(x@conn,toString(score$OutputCasTables[2]))
  if (assessment==TRUE){
    if (paste("I_",target,sep='') %in% colnames(score_out)){
      nom_target = TRUE
      stopifnot(nchar(score_out@tname)>0 & nchar(event)>0)
      assess <- runAction(score_out@conn,
                           "percentile.assess",
                           table=list(name=score_out@tname),
                           inputs=as.list(trimws(score$EncodedName[trimws(score$EncodedName$LEVNAME)==event,3])),
                           pvar=as.list(trimws(score$EncodedName[! trimws(score$EncodedName$LEVNAME)==event,3])),
                           event = trimws(score$EncodedName[trimws(score$EncodedName$LEVNAME)==event,1]),
                           pevent=as.list(trimws(score$EncodedName[! trimws(score$EncodedName$LEVNAME)==event,1])),
                           response=target,
                           casout=list(name=paste(x@tname, "_lift", sep=''), replace=TRUE),
                           fitstatout=list(name=paste(x@tname, "_fitstat", sep=''), replace=TRUE),
                           rocout=list(name=paste(x@tname, "_roc", sep=''), replace=TRUE)
      )
      check_for_cas_errors(assess)
      res$roc <- defCasTable(x@conn,toString(assess$OutputCasTables[2,2]))

    }
    else {
      nom_target = FALSE
    }
    check_for_cas_errors(assess)
    res$lift <- defCasTable(x@conn,toString(assess$OutputCasTables[1,2]))
    res$fitstat <- defCasTable(x@conn,toString(assess$OutputCasTables[3,2]))
  }
  res$score <- score
  sasCode <- runAction(x@conn, "decisionTree.dtreeCode", modelTable=paste(x@tname, "_modelout", sep=''))
  check_for_cas_errors(sasCode)
  res$sasCode <- sasCode
  return(res)
}

sasDecisionForest <- function(x,  formula=NULL, target="", inputs="", nominals="",
                             crit="gain", maxlevel=6L, leafsize=5L, missing="useInSearch",
                             ntree=50, vote="majority", bootstrap=.632120559 ){

  if (exists("formula") & class(formula)=='formula') {
    f1 <- swat::casFormula(formula, x)
    target <-toString(f1[1])
    inputs <-f1[[2]]
  }

  # Are there nominals
  if (length(nominals)>1 || nchar(nominals)>0){
    nom <- as.list(nominals)
    res <-runAction(x, "decisionTree.forestTrain", inputs=as.list(inputs), target=target,
                     nominals=nom, crit=crit, prune=TRUE, varImp=TRUE, missing=toupper(missing),
                     nTree=ntree, bootstrap=bootstrap, vote=toupper(vote) )
  }
  else {
    res <-runAction(x, "decisionTree.forestTrain", inputs=as.list(inputs), target=target,
                     crit=crit, prune=TRUE, varImp=TRUE, missing=toupper(missing),
                     nTree=ntree, bootstrap=bootstrap, vote=toupper(vote))
  }
  check_for_cas_errors(res)

  return(res)
}
