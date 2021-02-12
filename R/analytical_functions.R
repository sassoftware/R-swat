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


sasDecisionTree <- function(x, formula = NULL, target = "", inputs = "", nominals = "",
                            crit = "gain", maxlevel = 6L, leafsize = 5L,
                            missing = "useInSearch", assessment = TRUE, event = "") {
  if (exists("formula") & class(formula) == "formula") {
    f1 <- .cas_formula(formula, x)
    target <- toString(f1[1])
    inputs <- f1[[2]]
  }

  # Are there nominals
  if (length(nominals) > 1 || nchar(nominals) > 0) {
    nom <- as.list(nominals)
    res <- cas.retrieve(x, "decisionTree.dtreeTrain",
      stop.on.error = TRUE, inputs = as.list(inputs), target = target,
      casout = list(name = paste(x@tname, "_modelout", sep = ""), replace = TRUE),
      encodeName = TRUE, nominals = nom, crit = crit, prune = TRUE,
      stat = TRUE, varImp = TRUE, missing = toupper(missing)
    )$results
  }
  else {
    res <- cas.retrieve(x, "decisionTree.dtreeTrain",
      stop.on.error = TRUE, inputs = as.list(inputs), target = target,
      crit = crit, prune = TRUE, varImp = TRUE, stat = TRUE, missing = toupper(missing)
    )$results
  }

  res$call <- match.call()
  vars <- c(x@names, x@computedVars)
  vars <- vars[vars != ""]
  score <- cas.retrieve(x, "decisionTree.dtreeScore",
    stop.on.error = TRUE,
    target = target,
    encodeName = TRUE,
    copyVars = as.list(vars),
    modelTable = paste(x@tname, "_modelout", sep = ""),
    casout = list(name = paste(x@tname, "_scoreout", sep = ""), replace = TRUE)
  )$results

  # Define score CAS table
  score_out <- CASTable(x@conn, toString(score$OutputCasTables[2]))
  if (assessment == TRUE) {
    if (paste("I_", target, sep = "") %in% colnames(score_out)) {
      stopifnot(nchar(score_out@tname) > 0 & nchar(event) > 0)
      assess <- cas.retrieve(score_out@conn,
        "percentile.assess",
        stop.on.error = TRUE,
        table = list(name = score_out@tname),
        inputs = as.list(trimws(score$EncodedName[trimws(score$EncodedName$LEVNAME) == event, 3])),
        pvar = as.list(trimws(score$EncodedName[!trimws(score$EncodedName$LEVNAME) == event, 3])),
        event = trimws(score$EncodedName[trimws(score$EncodedName$LEVNAME) == event, 1]),
        pevent = as.list(trimws(score$EncodedName[!trimws(score$EncodedName$LEVNAME) == event, 1])),
        response = target,
        casout = list(name = paste(x@tname, "_lift", sep = ""), replace = TRUE),
        fitstatout = list(name = paste(x@tname, "_fitstat", sep = ""), replace = TRUE),
        rocout = list(name = paste(x@tname, "_roc", sep = ""), replace = TRUE)
      )$results
      res$roc <- CASTable(x@conn, toString(assess$OutputCasTables[2, 2]))
    }
    .check_for_cas_errors(assess)
    res$lift <- CASTable(x@conn, toString(assess$OutputCasTables[1, 2]))
    res$fitstat <- CASTable(x@conn, toString(assess$OutputCasTables[3, 2]))
  }

  res$score <- score
  sascode <- cas.retrieve(x@conn, "decisionTree.dtreeCode", stop.on.error = TRUE,
                          modelTable = paste(x@tname, "_modelout", sep = ""))$results
  res$sasCode <- sascode
  return(res)
}

sasDecisionForest <- function(x, formula = NULL, target = "", inputs = "", nominals = "",
                              crit = "gain", maxlevel = 6L, leafsize = 5L,
                              missing = "useInSearch", ntree = 50, vote = "majority",
                              bootstrap = .632120559) {
  if (exists("formula") & class(formula) == "formula") {
    f1 <- .cas_formula(formula, x)
    target <- toString(f1[1])
    inputs <- f1[[2]]
  }

  # Are there nominals
  if (length(nominals) > 1 || nchar(nominals) > 0) {
    nom <- as.list(nominals)
    res <- cas.retrieve(x, "decisionTree.forestTrain",
      stop.on.error = TRUE, inputs = as.list(inputs), target = target,
      nominals = nom, crit = crit, prune = TRUE, varImp = TRUE, missing = toupper(missing),
      nTree = ntree, bootstrap = bootstrap, vote = toupper(vote)
    )$results
  }
  else {
    res <- cas.retrieve(x, "decisionTree.forestTrain",
      stop.on.error = TRUE, inputs = as.list(inputs), target = target,
      crit = crit, prune = TRUE, varImp = TRUE, missing = toupper(missing),
      nTree = ntree, bootstrap = bootstrap, vote = toupper(vote)
    )$results
  }

  return(res)
}
