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

setGeneric(
  "cas.run",
  function (x, actn, ..., stop.on.error = FALSE) {
    standardGeneric("cas.run")
  }
)

#' Run a CAS Action by Name
#'
#' This function enables you to specify a CAS action
#' name and run it. This is an alternative to running
#' an action from the generated function names. An
#' example of a generated function name is cas.table.tableInfo.
#'
#' @param CASorCASTab An instance of a CAS object that represents
#'  a connection and CAS session, or an instance of a CASTable.
#' @param actn A \code{character} string that specifies
#'  the action set and action name to run.
#' @param stop.on.error Logical indicating that if errors occurred
#'   when running the action, an error is raised.
#' @param \dots Parameters that are passed to the CAS action.
#'
#' @return List of action results
#'
#' @examples
#' \dontrun{
#' # display the active caslib for the session
#' cas.run(conn, "sessionProp.getSessOpt", name = "caslib")
#'
#' # display simple summary statistics for an uploaded data frame
#' mtcarsct <- as.CASTable(conn, mtcars)
#' cas.run(mtcarsct, "simple.summary")
#'
#' # the preceding cas.run function is functionally
#' # equivalent to the following generated function
#' cas.simple.summary(mtcarsct)
#' }
#'
#' @export
setMethod(
  "cas.run",
  signature(x = "CAS"),
  function(x, actn, ..., stop.on.error = FALSE) {
    return(x$retrieve(actn, ..., stop.on.error = stop.on.error)$results)
  }
)

#' @export
setMethod(
  "cas.run",
  signature(x = "CASTable"),
  function(x, actn, ..., stop.on.error = FALSE) {
    args <- list(...)
    # TODO: Need reflection information to verify a table parameter is needed.
    if (is.null(args$table)) {
      return(x@conn$retrieve(actn, ..., table = x, stop.on.error = stop.on.error)$results)
    } else {
      return(x@conn$retrieve(actn, ..., stop.on.error = stop.on.error)$results)
    }
  }
)

#' Run DATA step action
#'
#' @param conn \code{\link{CAS}} connection object.
#' @param code DATA step code.
#'
#' @return datastep.runCode action results
#'
#' @keywords internal
#'
.run_sas_code <- function(conn, code = "") {
  res <- cas.retrieve(conn, "dataStep.runCode", code = code, stop.on.error = TRUE)
  return(res$results)
}

#' List CAS Action Sets
#'
#' This function lists all action sets that are installed on the
#' CAS server. The results include a column that is named loaded
#' to indicate if the action set is already available for use
#' or must be loaded before it can be used.
#'
#' @param conn An instance of a CAS object that represents
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
.list_action_sets <- function(conn) {
  res <- cas.retrieve(conn, "builtins.actionSetInfo", all = "FALSE", stop.on.error = TRUE)
  return(as.list(res$results$setinfo))
}

#' Get reflection signature of action
#'
#' @keywords internal
#'
.gen_sig <- function(conn, actn) {
  args <- list(quote(conn), "builtins.reflect", stop.on.error = TRUE, action = actn, showLabels = FALSE)
  if ("reflection.levels" %in% conn$serverFeatures) {
    args$levels <- 1
  }
  res <- do.call(cas.run, args)
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

  str3 <- paste(str3, "   args = c('actn'='", actn, "', args)\n", sep = "")
  str3 <- paste(str3, "   args = c('CASorCASTab'=substitute(CASorCASTab), args)\n", sep = "")

  str1 <- paste("cas.", actn, " <- function(CASorCASTab", str, ", ...) {\n", sep = "")
  str1 <- paste(str1, str3, "do.call('swat::cas.run', args)\n}\n", sep = "")

  return(str1)
}

#' Generate action function wrappers using actionset reflection information
#'
#' @keywords internal
#'
.gen_functions <- function(conn, actionset) {
  res <- cas.retrieve(conn, "builtins.actionSetInfo", "extensions" = actionset)
  if (toupper(res$results$setinfo$actionset[1]) != toupper(actionset)) {
    message(paste("ActionSet ", actionset, " not found"))
  } else {
    actionset <- res$results$setinfo$actionset[1]
    res <- cas.retrieve(conn, "builtins.listActions", "actionSet" = actionset)
    if (length(res$results) > 0) {
      acts <- as.data.frame(res$results)[, 1]
      env <- globalenv()
      for (name in acts) {
        if (!as.logical(getOption("cas.gen.function.sig"))) {
          val <- paste("cas.", actionset, ".", name,
                       " <- function(object = NULL, ...) {swat::cas.run(object, '",
                       paste(actionset, name, sep = "."), "', ...) } ", sep = "")
          defn <- eval(parse(text = val, env))
          environment(defn) <- env
          fname <- paste("cas.", actionset, ".", name, sep = "")
          setGeneric(name = fname, def = defn, package = "swat", where = env)
        } else {
          val <- .gen_sig(conn, paste(actionset, name, sep = "."))
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
                           " <- function(object = NULL, ...) {swat::cas.run(object, '",
                           paste(actionset, name, sep = "."), "', ...) } ", sep = "")
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
#' @param conn An instance of a CAS object that represents
#'   a connection and CAS session.
#' @param actn A \code{string} value that specifies
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
#' .list_action_params(conn, actn = "simple.summary")
#'
#' # fetch is in the table action set
#' .list_action_params(s, actn = "fetch")
#'
#' # specify the generated function name
#' .list_action_params(s, cas.regression.logistic)
#' }
#'
#' @keywords internal
#'
.list_action_params <- function(conn, actn, display = TRUE) {
  if (typeof(actn) == "closure") {
    actn <- toString(substitute(actn))
  }
  if (startsWith(actn, "cas.")) {
    actn <- substr(actn, 5, nchar(actn))
  }
  args <- list("builtins.reflect", action = actn, showLabels = FALSE)
  if ("reflection.levels" %in% conn$serverFeatures) {
    args$levels <- 1
  }
  res <- do.call(conn$retrieve, args)
  .check_for_cas_errors(res)
  str <- paste("cas.", actn, "(", sep = "")
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
