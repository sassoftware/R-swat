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

#' Run DATA step action
#'
#' @param x \code{\link{CAS}} connection object.
#' @param code DATA step code.
#'
#' @return datastep.runCode action results
#'
#' @keywords internal
.run_sas_code <- function(x, code = "") {
  res <- cas.retrieve(x, "dataStep.runCode", code = code, stop.on.error = TRUE)
  return(res$results)
}

#' List CAS Action Sets
#'
#' This function lists all action sets that are installed on the
#' CAS server. The results include a column that is named loaded
#' to indicate if the action set is already available for use
#' or must be loaded before it can be used.
#'
#' @param x An instance of a CAS object that represents
#'  a connection and CAS session.
#'
#' @return List of actionset information
#'
#' @examples
#' \dontrun{
#' .list_action_sets(conn)
#' }
#'
#' @keywords internal
#'
.list_action_sets <- function(x) {
  res <- cas.retrieve(x, "builtins.actionSetInfo", all = "FALSE", stop.on.error = TRUE)
  return(as.list(res$results$setinfo))
}

#' Get reflection signature of action
#'
#' @keywords internal
#'
.gen_sig <- function(x, action) {
  args <- list(quote(x), "builtins.reflect", stop.on.error = TRUE, action = action, showLabels = FALSE)
  if ("reflection.levels" %in% x$serverFeatures) {
    args$levels <- 1
  }
  res <- do.call(cas.retrieve, args)$results
  str <- ""
  str2 <- ""
  str3 <- "   args <- list(...)\n"
  for (parms in res[[1]]$actions[[1]]$params) {
    if (!is.null(parms$isRequired)) {
      str <- paste(str, ", `", parms$name, "`", sep = "")
      str2 <- paste(str2, ", `", parms$name, "`=`", parms$name, "`", sep = "")
    } else if (!is.null(parms$default)) {
      if (parms$parmType == "string") {
        s1 <- if (parms$default == "") " " else parms$default
        s1 <- if (s1 == "\\") "\\\\" else parms$default
        str <- paste(str, ", `", parms$name, '`="', s1, '"', sep = "")
      }
      else {
        str <- paste(str, ", `", parms$name, "`=", parms$default, sep = "")
      }

      str2 <- paste(str2, ", `", parms$name, "`=`", parms$name, "`", sep = "")
    } else {
      str <- paste(str, ", `", parms$name, "`=NULL", sep = "")
    }
    str3 <- paste(str3, "   if( ! missing(`", parms$name, "`)) args = c('",
                  parms$name, "'=substitute(`", parms$name, "`), args)\n", sep = "")
  }

  str3 <- paste(str3, "   args = c('action'='", action, "', args)\n", sep = "")
  str3 <- paste(str3, "   args = c('CASorCASTab'=substitute(CASorCASTab), args)\n", sep = "")

  str1 <- paste("cas.", action, " <- function(CASorCASTab", str, ", ...) {\n", sep = "")
  str1 <- paste(str1, str3, "do.call('swat::cas.retrieve', args)$results\n}\n", sep = "")

  return(str1)
}

#' Generate action function wrappers using actionset reflection information
#'
#' @keywords internal
#'
.gen_functions <- function(x, actionset) {
  res <- cas.retrieve(x, "builtins.actionSetInfo", "extensions" = actionset)
  if (toupper(res$results$setinfo$actionset[1]) != toupper(actionset)) {
    message(paste("ActionSet ", actionset, " not found"))
  } else {
    actionset <- res$results$setinfo$actionset[1]
    res <- cas.retrieve(x, "builtins.listActions", "actionSet" = actionset)
    if (length(res$results) > 0) {
      acts <- as.data.frame(res$results)[, 1]
      env <- globalenv()
      for (name in acts) {
        if (!as.logical(getOption("cas.gen.function.sig"))) {
          val <- paste("cas.", actionset, ".", name,
                       " <- function(object = NULL, ...) {swat::cas.retrieve(object, '",
                       paste(actionset, name, sep = "."), "', ...)$results } ", sep = "")
          defn <- eval(parse(text = val, env))
          environment(defn) <- env
          fname <- paste("cas.", actionset, ".", name, sep = "")
          setGeneric(name = fname, def = defn, package = "swat", where = env)
        } else {
          val <- .gen_sig(x, paste(actionset, name, sep = "."))
          tryCatch({
              defn <- eval(parse(text = val, env))
              environment(defn) <- env
              fname <- paste("cas.", actionset, ".", name, sep = "")
              setGeneric(name = fname, def = defn, package = "swat", where = env)
          }, error = function(e) {
              message(paste("Action ", actionset, ".", name, " Had invalid syntax: \n", val, sep = ""))
              message(paste("Error was: ", e))
              message(paste("Defining syntax as function(object, ...) instead. "))
              val <- paste("cas.", actionset, ".", name,
                           " <- function(object = NULL, ...) {swat::cas.retrieve(object, '",
                           paste(actionset, name, sep = "."), "', ...)$results } ", sep = "")
              defn <- eval(parse(text = val, env))
              environment(defn) <- env
              fname <- paste("cas.", actionset, ".", name, sep = "")
              setGeneric(name = fname, def = defn, package = "swat", where = env)
          })
        }
      }
    }
  }
}

#' List CAS action parameters by name
#'
#' This function displays the parameters for the specified
#' CAS action.
#'
#' @param x An instance of a CAS object that represents
#'   a connection and CAS session.
#' @param action A \code{string} value that specifies
#'   the action name. You can specify the following forms:
#'   \itemize{
#'     \item \emph{action-set.action-name}
#'     \item \emph{action-name}
#'     \item cas.\emph{action-set.action-name}
#'   }
#'   The third form matches the generated functions for
#'   the CAS actions.
#' @param display Should the parameters be printed to the
#'   screen?
#'
#' @return List of action parameters
#'
#' @examples
#' \dontrun{
#' # specify the action set and action name
#' .list_action_params(x, action = "simple.summary")
#'
#' # fetch is in the table action set
#' .list_action_params(s, action = "fetch")
#'
#' # specify the generated function name
#' .list_action_params(s, cas.regression.logistic)
#' }
#'
#' @keywords internal
#'
.list_action_params <- function(x, action, display = TRUE) {
  if (typeof(action) == "closure") {
    action <- toString(substitute(action))
  }
  if (startsWith(action, "cas.")) {
    action <- substr(action, 5, nchar(action))
  }
  args <- list("builtins.reflect", action = action, showLabels = FALSE)
  if ("reflection.levels" %in% x$serverFeatures) {
    args$levels <- 1
  }
  res <- do.call(x$retrieve, args)
  .check_for_cas_errors(res)
  str <- paste("cas.", action, "(", sep = "")
  plist <- list()
  i <- 1
  sep <- ""
  for (parms in res$results[[1]]$actions[[1]]$params) {
    str <- paste(str, sep, parms$name, '=\"', parms$parmType, '\"', sep = "")
    sep <- ", "
    plist[[i]] <- list("name" = parms$name, "parmType" = parms$parmType)
    i <- i + 1
  }
  str <- paste(str, ")\n")
  if (display) {
    cat(str)
  }
  return(plist)
}
