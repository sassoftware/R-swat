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

#'
#' SWAT: SAS Wrapper for Analytics Transfer (SWAT)
#'
#' This package enables you to connect from \R to a SAS Cloud Analytic Services
#' host, run actions on in-memory tables, and work with the results of the
#' actions.
#' \itemize{
#'   \item The \code{\link{CAS}} class provides an interface to your
#'         connection to the CAS server and CAS session.
#'   \item The \code{\link{CASTable}} class provides an interface to the
#'         in-memory tables.
#' }
#'
#' Depending on how you install the package, you might be able to use binary
#' communication with CAS. This is more efficient for bandwidth, but requires
#' that your \R installation have access to a precompiled library (rswat.so or
#' rswat.dll).  An alternative is to communicate with the server using the REST
#' interface of the server over HTTP. See the connection examples that follow.
#'
#' The responses and results of the actions are returned as \R objects.
#'
#' @section Connect and start a session:
#' \preformatted{
#'   s  <- CAS('cloud.example.com', 5570)  # binary communication
#'
#'   s2 <- CAS('cloud.example.com', 8777, protocol='https')
#' }
#'
#' @section Run a simple action:
#' \preformatted{
#'    results <- cas.retrieve(s, "builtins.serverStatus")$results
#'    results$server
#'    nodes actions
#'  1     1      15
#' }
#'
#' You can also run an action using the generated \R function:
#' \preformatted{
#'    results <- cas.builtins.serverStatus(s)
#'    results$server
#'    nodes actions
#'  1     1      15
#' }
#'
#' @section Upload a data.frame to a CASTable:
#' \preformatted{
#'   irisct <- as.CASTable(s, iris)
#' }
#'
#' @section Load a CAS actionSet:
#' \preformatted{
#'   cas.retrieve(s, "builtins.loadActionSet", actionSet="regression")
#' }
#'
#' @section Useful links:
#' \itemize{
#'   \item{\url{http://developer.sas.com/guides/r.html}}
#'   \item{\url{https://github.com/sassoftware/r-swat}}
#'   \item{Enter issues at \url{https://github.com/sassoftware/r-swat/issues}}
#' }
#'
#' @section Action documentation:
#' See the \href{http://documentation.sas.com/?softwareId=pgm
#' &softwareVersion=production.a&softwareContextId=allProdsActions&showBanner=develop}{
#' Actions and Action Sets by Name and Product}
#'
#' @docType package
#' @name swat
#' @rawNamespace exportPattern("^[[:alpha:]]+")
#' @rawNamespace importFrom("methods", "as", "callNextMethod", "is", "new", "setClass", "setGeneric", "slot", "initialize", "show")
#' @rawNamespace importFrom("utils", "read.csv", "read.table", "str", "write.csv", "write.csv2", "write.table")
#' @rawNamespace if ( grepl('linux', R.version$os) ) {useDynLib(rswat)}
#' @rawRd % Copyright SAS Institute
"_PACKAGE"

.onAttach <- function(lib, pkg) { # nocov start
  packageStartupMessage("SWAT ",
    utils::packageDescription("swat", field = "Version"),
    appendLF = TRUE
  )
} # nocov end

.onLoad <- function(lib, pkg) { # nocov start
  if (is.null(getOption("cas.trace.actions"))) {
    options(cas.trace.actions = FALSE)
  }
  if (is.null(getOption("cas.trace.ui.actions"))) {
    options(cas.trace.ui.actions = FALSE)
  }
  if (is.null(getOption("cas.print.messages"))) {
    options(cas.print.messages = TRUE)
  }
  if (is.null(getOption("cas.message.level"))) {
    options(cas.message.level = "note")
  }
  if (is.null(getOption("cas.message.level.ui"))) {
    options(cas.message.level.ui = "error")
  }
  if (is.null(getOption("cas.max.download.rows"))) {
    options(cas.max.download.rows = 10000)
  }
  if (is.null(getOption("cas.gen.function"))) {
    options(cas.gen.function = TRUE)
  }
  if (is.null(getOption("cas.gen.function.sig"))) {
    options(cas.gen.function.sig = FALSE)
  }
  # raw, formatted, both, none
  if (is.null(getOption("cas.bygroup.mode"))) {
    options(cas.bygroup.mode = "raw")
  }
  if (is.null(getOption("cas.bygroup.dup.suffix"))) {
    options(cas.bygroup.dup.suffix = "_f")
  }

  libssl_locs <- Sys.glob(c(
     "/usr/lib64/libssl.so.10",
     "/usr/lib64/libssl.so.1.0*",
     "/usr/lib/x86_64-linux-gnu/libssl.so.1.0*",
     file.path(normalizePath(R.home("..")), "libssl.so.10"),
     file.path(normalizePath(R.home("..")), "libssl.so.1.0*"),
     "/usr/lib64/libssl.so.1.1*",
     "/usr/lib/x86_64-linux-gnu/libssl.so.1.1*",
     file.path(normalizePath(R.home("..")), "libssl.so.1.1*")
  ))

  libcrypto_locs <- Sys.glob(c(
     "/usr/lib64/libcrypto.so*",
     "/usr/lib/x86_64-linux-gnu/libcrypto.so*",
     file.path(normalizePath(R.home("..")), "libcrypto.so*")
  ))

  if ( Sys.getenv("TKESSL_OPENSSL_LIB") == "" && length(libssl_locs) > 0 ) {
     Sys.setenv(TKESSL_OPENSSL_LIB=libssl_locs[length(libssl_locs)])
  }

  if ( Sys.getenv("TKERSA2_OPENSSL_LIB") == "" && length(libssl_locs) > 0 ) {
     Sys.setenv(TKERSA2_OPENSSL_LIB=libssl_locs[length(libssl_locs)])
  }

  if ( Sys.getenv("TKECERT_CRYPTO_LIB") == "" && length(libcrypto_locs) > 0 ) {
     Sys.setenv(TKECERT_CRYPTO_LIB=libcrypto_locs[length(libcrypto_locs)])
  }

  if (file.exists(file.path(lib, pkg, "libs", "rswat.so"))) {
    library.dynam("rswat", pkg, lib)
    if (file.exists(file.path(lib, pkg, "libs", "tkmk.so"))) {
      InitializeTK(file.path(lib, pkg, "libs:"))
    } else {
      # TODO: InitializeTK('') once new extension has been released with support for that.
    }
  } else if (file.exists(file.path(lib, pkg, "libs", "x64", "rswat.dll"))) {
    library.dynam("rswat", pkg, lib)
    if (file.exists(file.path(lib, pkg, "libs", "x64", "tkmk.dll"))) {
      InitializeTK(file.path(lib, pkg, "libs", "x64;"))
    } else {
      # TODO: InitializeTK('') once new extension has been released with support for that.
    }
  } else if (file.exists(file.path(lib, pkg, "libs", "rswat.dylib"))) {
    library.dynam("rswat", pkg, lib, file.ext = ".dylib")
    if (file.exists(file.path(lib, pkg, "libs", "tkmk.dylib"))) {
      InitializeTK(file.path(lib, pkg, "libs:"))
    } else {
      # TODO: InitializeTK('') once new extension has been released with support for that.
    }
  } else {
    packageStartupMessage(
      paste("NOTE: The extension module for using the CAS binary protocol can ",
            "      not be located. Only the CAS REST interface may be used.",
            sep = "\n")
    )
  }
} # nocov end

.onUnload <- function(lib) { # nocov start
  try(library.dynam.unload("rswat", lib), silent = FALSE)
} # nocov end

#' Determine if binary communication with a CAS server is possible
#'
#' @noRd
.binary_enabled <- function() {
  return(any(grepl("/rswat.(dll|so|dylib)", as.character(.dynLibs()))))
}

cacheMetaData(1)

retry_action_code <- 0x280034

#' Check a C extension object for error messages
#'
#' If the object does have an error message attached, the \code{stop()}
#' function is called with that message.
#' 
#' @param x C extension object.
#'
#' @noRd
.error_check <- function(x) {
  if (!is.null(x)) {
    m <- x$getLastErrorMessage()
    if (!is.null(m) && nchar(m[[1]]) > 0) {
      stop(m[[1]])
    }
  }
}

# Make getClass happy when using InitializeTK
setClass("_p_TKHndlp", slots = c(ref = "externalptr"))
setClass("_p_void", slots = c(ref = "externalptr"))

#' Convert a value to an int64
#'
#' This is an internal function to convert an arbitrary value to
#' an int64 value. The current implementation simply returns a
#' numeric since int64 values are not well supported in R at this time.
#'
#' @noRd
.as.integer64 <- function(value) {
  return(as.numeric(value))
}

#' Object containing a CAS server request
#'
#' @param sw_request The C extension request object.
#'
#' @noRd
CASRequest <- setRefClass(
  Class = "CASRequest",

  fields = list(
    sw_request = "ANY",
    params = "list"
  ),

  methods = list(
    initialize = function(sw_request) {
      callSuper(sw_request = sw_request)
      paramlist <- sw_request$getParameters()
      .error_check(sw_request)
      nparams <- sw_request$getNParameters()
      .error_check(sw_request)
      params <<- .casvaluelist2r(paramlist, nparams)
      .self
    }
  )
)

#' Object containing a response from the CAS server
#' 
#' This object is typically used behind-the-scenes in the CAS action
#' wrappers, but is not surfaced. The one use-case where it may be
#' encountered is when using the \code{CAS$invoke()} method and 
#' retrieving the responses from the server manually.
#'
#' @param sw_response The C extension response object.
#'
#' @field disposition List containing information about the final
#'   status of the CAS action run.
#' @field performance List containing performance metrics of the
#'   CAS action.
#' @field messages    List of messages produced by the CAS action.
#'
#' @noRd
CASResponse <- setRefClass(
  Class = "CASResponse",

  fields = list(
    sw_response = "ANY",
    disposition = "list",
    performance = "list",
    messages = "character"
  ),

  methods = list(
    initialize = function(sw_response) {
      callSuper(sw_response = sw_response)

      output <- list()

      d <- list()
      d[["severity"]] <- sw_response$getDispositionSeverity()
      .error_check(sw_response)
      d[["reason"]] <- sw_response$getDispositionReason()
      .error_check(sw_response)
      if (is.null(d[["reason"]])) {
        d[["reason"]] <- character()
      }
      d[["status"]] <- sw_response$getDispositionStatus()
      .error_check(sw_response)
      if (is.null(d[["status"]])) {
        d[["status"]] <- character()
      }
      d[["statusCode"]] <- sw_response$getDispositionStatusCode()
      .error_check(sw_response)
      d[["debug"]] <- sw_response$getDispositionDebug()
      .error_check(sw_response)
      if (is.null(d[["debug"]])) {
        d[["debug"]] <- character()
      }
      disposition <<- d

      p <- list()
      p[["elapsedTime"]] <- sw_response$getElapsedTime()
      .error_check(sw_response)
      p[["cpuUserTime"]] <- sw_response$getCPUUserTime()
      .error_check(sw_response)
      p[["cpuSystemTime"]] <- sw_response$getCPUSystemTime()
      .error_check(sw_response)
      p[["systemTotalMemory"]] <- sw_response$getSystemTotalMemory()
      .error_check(sw_response)
      p[["systemNodes"]] <- sw_response$getSystemNodes()
      .error_check(sw_response)
      p[["systemCores"]] <- sw_response$getSystemCores()
      .error_check(sw_response)
      p[["memory"]] <- sw_response$getMemory()
      .error_check(sw_response)
      p[["memoryOS"]] <- sw_response$getMemoryOS()
      .error_check(sw_response)
      p[["memorySystem"]] <- sw_response$getMemorySystem()
      .error_check(sw_response)
      p[["memoryQuota"]] <- sw_response$getMemoryQuota()
      .error_check(sw_response)
      performance <<- p

      msgs <- character()
      nmessages <- sw_response$getNMessages()
      for (i in 1:nmessages) {
        m <- sw_response$getNextMessage()
        .error_check(sw_response)
        if (!is.null(m) && nchar(m) > 0) {
          msgs <- c(msgs, m)
          if (as.logical(getOption("cas.print.messages"))) {
            message(m)
          }
        }
      }
      messages <<- msgs

      .self
    }
  )
)

#' CAS Data Message Handler
#'
#' Data message handlers are required when uploading data to the
#' server using the \code{table.addtable} action. Currently, only
#' binary CAS connections support the \code{table.addtable} action.
#' It is recommended to use \code{table.upload} if possible since
#' it works with all protocols.
#'
#' All fields in the object are read-only. They are computed based
#' on the data in the constructor arguments.
#'
#' @param data   The \code{data.frame} to upload.
#' @param nrecs  Optional parameter indicating the size of the
#'   row buffer which is sent to the server.
#' @param locale Optional parameter indicating the client locale.
#'
#' @field data      The \code{data.frame} to upload.
#' @field reclen    The byte-length of one buffer row.
#' @field nbuffrows The number of rows in the buffer.
#' @field vars      List of lists containing the variable definitions.
#'   These definitions correspond to the variable definitions used by the
#'   \code{table.loadTable} action.
#' @field ncols     Integer number of columns in the data.
#' @field nrows     Integer number of rows in the data.
#' @field finished  Logical indicating that the data message handler
#'   has completed.
#' @field sw_databuffer Data buffer object in the C extension.
#'
#' @export
CASDataMsgHandler <- setRefClass(
  Class = "CASDataMsgHandler",

  fields = list(
    data = "data.frame",
    reclen = "numeric",
    nbuffrows = "numeric",
    vars = "list",
    ncols = "integer",
    nrows = "integer",
    finished = "logical",
    sw_databuffer = "ANY"
  ),

  methods = list(
    initialize = function(data, nrecs = 1000, locale = NULL) {
      data <<- data
      reclen <<- 0
      nrows <<- dim(data)[[1]]
      nbuffrows <<- min(nrecs, nrows)
      vars <<- list()
      finished <<- FALSE
      cnames <- names(data)
      ctypes <- lapply(data, function(x) class(x)[[1]])

      for (i in seq_len(length(ctypes))) {
        if (ctypes[[i]] == "integer") {
          vars[[i]] <<- list(
            name = cnames[[i]], rtype = "numeric",
            length = 8, offset = reclen, type = "int64"
          )
          reclen <<- reclen + 8
        }
        else if (ctypes[[i]] == "integer64") {
          vars[[i]] <<- list(
            name = cnames[[i]], rtype = "numeric",
            length = 8, offset = reclen, type = "int64"
          )
          reclen <<- reclen + 8
        }
        else if (ctypes[[i]] == "numeric") {
          vars[[i]] <<- list(
            name = cnames[[i]], rtype = "numeric",
            length = 8, offset = reclen, type = "sas"
          )
          reclen <<- reclen + 8
        }
        else if (ctypes[[i]] == "character" || ctypes[[i]] == "factor") {
          vars[[i]] <<- list(
            name = cnames[[i]], rtype = "char",
            length = 16, offset = reclen, type = "varchar"
          )
          reclen <<- reclen + 16
        }
        else if (ctypes[[i]] == "Date") {
          vars[[i]] <<- list(
            name = cnames[[i]], rtype = "numeric", format = "DATE",
            formattedlength = 9,
            length = 4, offset = reclen, type = "date"
          )
          reclen <<- reclen + 4
        }
        else if (ctypes[[i]] == "POSIXct" || ctypes[[i]] == "POSIXlt") {
          vars[[i]] <<- list(
            name = cnames[[i]], rtype = "numeric", format = "DATETIME",
            formattedlength = 20,
            length = 8, offset = reclen, type = "datetime"
          )
          reclen <<- reclen + 8
        }
        else {
          message(sprintf("Unrecognized type for column %s.", cnames[[i]]))
        }
      }

      ncols <<- length(vars)

      soptions <- ""
      if (!is.null(locale)) {
        soptions <- gsub("^\\s+|\\s+$", "", paste(soptions, "locale=", locale, sep = ""))
      }

      sw_error <- SW_CASError(soptions)

      sw_databuffer <<- SW_CASDataBuffer(reclen, nbuffrows, soptions, sw_error)
      .error_check(sw_error)

      .self
    },

    show = function() {
      " Print string representation of the CASDataMsgHandler. "
      cat("CASDataMsgHandler()\n")
    },

    write = function(row, values) {
      "
      Write a set of values to a specified row in the buffer. The \\code{row}
      parameter indicates the buffer row index. The \\code{values} parameter
      is the list of values to write to the buffer. Each element in the list
      corresponds to a column in the buffer.

      "
      for (col in seq_len(ncols)) {
        if (tolower(vars[[col]][["type"]]) == "varchar") {
          sw_databuffer$setString(
            row - 1, vars[[col]][["offset"]],
            as.character(values[[col]])
          )
          .error_check(sw_databuffer)
        }
        else if (tolower(vars[[col]][["type"]]) == "int32") {
          sw_databuffer$setInt32(row - 1, vars[[col]][["offset"]], as.integer(values[[col]]))
          .error_check(sw_databuffer)
        }
        else if (tolower(vars[[col]][["type"]]) == "int64") {
          tryCatch({
            sw_databuffer$setInt64FromString(
              row - 1, vars[[col]][["offset"]],
              format(values[[col]], scientific = FALSE)
            )
          }, error = function(e) {
            sw_databuffer$setInt64(row - 1, vars[[col]][["offset"]], as.integer(values[[col]]))
          })
          .error_check(sw_databuffer)
        }
        else if (tolower(vars[[col]][["type"]]) == "date") {
          sw_databuffer$setInt32(row - 1, vars[[col]][["offset"]], Date.as.CASd(values[[col]]))
          .error_check(sw_databuffer)
        }
        else if (tolower(vars[[col]][["type"]]) == "datetime" ||
          tolower(vars[[col]][["type"]]) == "time") {
          if (class(values[[col]])[[1]] == "POSIXlt") {
            value <- POSIXlt.as.CASdt(values[[col]])
          } else {
            value <- POSIXct.as.CASdt(values[[col]])
          }
          tryCatch({
            sw_databuffer$setInt64FromString(
              row - 1, vars[[col]][["offset"]],
              format(value, scientific = FALSE)
            )
          }, error = function(e) {
            sw_databuffer$setInt64(row - 1, vars[[col]][["offset"]], as.integer(value))
          })
          .error_check(sw_databuffer)
        }
        else {
          sw_databuffer$setDouble(row - 1, vars[[col]][["offset"]], as.numeric(values[[col]]))
          .error_check(sw_databuffer)
        }
      }
    },

    send = function(conn, nrecs) {
      "
      Send a buffer to the server. \\code{conn} is a CAS connection object.
      \\code{nrecs} is the number of records in the buffer to send.

      "
      rc <- sw_databuffer$send(conn$sw_connection, nrecs)
      .error_check(sw_databuffer)
    },

    finish = function(conn) {
      " Tell the server that you are finished sending data. "
      finished <<- TRUE
      out <- sw_databuffer$send(conn$sw_connection, 0)
      .error_check(sw_databuffer)
      return(out)
    },

    getrow = function(row) {
      " Return the row of data for the specified index. "
      if (row <= nrows) {
        return(as.list(data[row, ]))
      }
      return(NULL)
    },

    call = function(request, conn) {
      "
      Start the data upload loop. The \\code{request} parameter is a
      \\code{\\link{CASRequest}} object containing information about
      the data request. \\code{conn} is a CAS connection object.

      This method will loop as long as \\code{CASDataMsgHandler$getrow()}
      returns data to upload. Once all data has been uploaded, the
      method returns a \\code{\\link{CASResponse}} for the upload
      action call.
       
      "
      if (finished) {
        stop("The data message handler has already been used.")
      }

      inputrow <- 0
      row <- 0

      while (TRUE) {
        written <- FALSE

        # populate buffer
        for (row in 1:nbuffrows) {
          inputrow <- inputrow + 1
          values <- .self$getrow(inputrow)
          if (is.null(values)) {
            row <- row - 1
            break
          }
          .self$write(row, values)
          written <- TRUE
        }

        # send it
        if (written) {
          .self$send(conn, row)
          res <- conn$getone()
          if (class(res) == "CASRequest") {
            next
          }
          else if (class(res) == "CASResponse") {
            if (res$disposition$severity <= 1) {
              messages <- res$messages
              while (class(res) == "CASResponse") {
                res <- conn$getone()
                messages <- c(messages, res$messages)
                if (res$disposition$severity > 1) {
                  res$messages <- messages
                  break
                }
              }
              if (class(res) == "CASRequest") {
                next
              }
            }
          }
        }
        else {
          break
        }

        # If we failed for some reason, return the last response
        if (class(res) == "CASResponse" && res$disposition$severity > 1) {
          return(res)
        }
      }

      # end it
      .self$finish(conn)
      return(conn$getone())
    }
  )
)

#' Attempt to detect the protocol of the CAS server
#'
#' @param hostname The hostname of the CAS server.
#' @param port     The port of the CAS server.
#' @param protocol The protocol of the CAS server; can be \code{"auto"}.
#'
#' @return String containing the detected protocol of the server.
#'
#' @noRd
.detect_protocol <- function(hostname, port, protocol) {
  if (protocol[[1]] != "auto") {
    return(protocol[[1]])
  }

  for (i in seq_len(length(hostname))) {
    tryCatch({
      url <- paste("http://", hostname[[i]], ":", port, "/cas", sep = "")
      r <- httr::GET(url)
      httr::handle_reset(url)
      if (r$status_code == 400) {
        return("https")
      } else if (r$status_code == 401) {
        return("http")
      }
    }, error = function(e) {
    })
  }

  return("cas")
}

#' Expand URL macro
#'
#' @param hostname String containing a URL with possible expansions.
#'   Portions of the string contained within square brackets can
#'   contain comma-separated lists of permutations.
#'
#' @return List of resulting hostnames.
#'
#' @noRd
.expand_url <- function(hostname) {
  final <- c()

  for (i in seq_len(length(hostname))) {
    total_items <- 1
    items <- list()
    parts <- unlist(strsplit(hostname[[i]], "\\[|\\]"))
    for (j in seq_len(length(parts))) {
      if (j %% 2) {
        res <- list(parts[[j]])
      } else {
        res <- unlist(strsplit(parts[[j]], "\\s*,\\s*"))
      }
      total_items <- total_items * length(res)
      items[[length(items) + 1]] <- res
    }

    # Build all linear combinations of names
    out <- vector(mode = "character", length = total_items)
    for (j in seq_len(length(items))) {
      n <- 1
      item <- items[[j]]
      while (n <= total_items) {
        for (k in seq_len(length(item))) {
          out[[n]] <- paste(out[[n]], item[[k]], sep = "")
          n <- n + 1
        }
      }
    }

    for (i in seq_len(length(out))) {
      final[[length(final) + 1]] <- out[[i]]
    }
  }

  return(final)
}

#' Resolve the connection information from given parameters and env variables
#'
#' @param hostname The CAS server hostname.
#' @param port     The CAS server port number.
#' @param username CAS username.
#' @param password CAS password.
#' @param protocol CAS server protocol. This should be one of the following:
#'   \code{cas}, \code{http}, or \code{https}.
#' @param path     The base path of the CAS server. This is only valid
#'   when using HTTP(S) connections.
#'
#' @return List containing all connection parmeters resolved.
#'
#' @noRd
.get_connection_info <- function(hostname, port, username, password, protocol, path) {
  # Get defaults from environment
  hostname <- hostname[hostname != ""]
  if (is.null(hostname) || length(hostname) == 0) {
    if (Sys.getenv("CAS_URL") != "") {
      hostname <- Sys.getenv("CAS_URL")
    } else if (Sys.getenv("CASURL") != "") {
      hostname <- Sys.getenv("CASURL")
    } else if (Sys.getenv("CAS_HOST") != "") {
      hostname <- Sys.getenv("CAS_HOST")
    } else if (Sys.getenv("CASHOST") != "") {
      hostname <- Sys.getenv("CASHOST")
    } else {
      hostname <- "localhost"
    }
    hostname <- unlist(strsplit(hostname, "\\s+"))
  }

  port <- port[port > 0]
  if (is.null(port) || length(port) == 0) {
    if (Sys.getenv("CAS_PORT") != "") {
      port <- as.integer(Sys.getenv("CAS_PORT"))
    } else if (Sys.getenv("CASPORT") != "") {
      port <- as.integer(Sys.getenv("CASPORT"))
    } else {
      port <- 0
    }
  }
  port <- port[[1]]

  protocol <- protocol[protocol != ""]
  if (is.null(protocol) || length(protocol) == 0 || protocol[[1]] == "auto") {
    if (Sys.getenv("CAS_PROTOCOL") != "") {
      protocol <- Sys.getenv("CAS_PROTOCOL")
    } else if (Sys.getenv("CASPROTOCOL") != "") {
      protocol <- Sys.getenv("CASPROTOCOL")
    } else {
      protocol <- "auto"
    }
  }
  protocol <- protocol[[1]]

  username <- username[username != ""]
  if (is.null(username) || length(username) == 0) {
    if (Sys.getenv("CAS_USER") != "") {
      username <- Sys.getenv("CAS_USER")
    } else if (Sys.getenv("CASUSER") != "") {
      username <- Sys.getenv("CASUSER")
    } else if (Sys.getenv("CAS_USERNAME") != "") {
      username <- Sys.getenv("CAS_USERNAME")
    } else if (Sys.getenv("CASUSERNAME") != "") {
      username <- Sys.getenv("CASUSERNAME")
    } else {
      username <- ""
    }
  }
  username <- username[[1]]

  password <- password[password != ""]
  if (is.null(password) || length(password) == 0) {
    if (Sys.getenv("CAS_TOKEN") != "") {
      password <- Sys.getenv("CAS_TOKEN")
    } else if (Sys.getenv("CASTOKEN") != "") {
      password <- Sys.getenv("CASTOKEN")
    } else if (Sys.getenv("CAS_PASSWORD") != "") {
      password <- Sys.getenv("CAS_PASSWORD")
    } else if (Sys.getenv("CASPASSWORD") != "") {
      password <- Sys.getenv("CASPASSWORD")
    } else {
      password <- ""
    }
  }
  password <- password[[1]]

  # Check hostname for other components
  new_hostname <- list()
  for (i in seq_len(length(hostname))) {
    if (!grepl("^\\w+://", hostname[[1]], perl = TRUE)) {
      new_hostname <- c(new_hostname, paste(protocol, "://", hostname[[i]], sep = ""))
    } else {
      new_hostname <- c(new_hostname, hostname[[i]])
    }
  }

  hostname <- .expand_url(new_hostname)
  urlp <- httr::parse_url(hostname[[1]])
  protocol <- if (length(urlp$scheme[urlp$scheme != ""]) != 0) urlp$scheme else protocol
  new_hostname <- list()
  for (i in seq_len(length(hostname))) {
    hurlp <- httr::parse_url(hostname[[i]])
    if (!is.null(hurlp$hostname) && hurlp$hostname != "") {
      new_hostname[[length(new_hostname) + 1]] <- hurlp$hostname
    }
  }
  hostname <- new_hostname
  port <- if (length(urlp$port[urlp$port != 0]) != 0) urlp$port else port
  username <- if (length(urlp$username[urlp$username != ""]) != 0) urlp$username else username
  password <- if (length(urlp$password[urlp$password != ""]) != 0) urlp$password else password
  path <- if (length(urlp$path[urlp$path != ""]) != 0) urlp$path else path

  # Set port based on protocol, if port number is missing
  if (length(port) == 0 || port[[1]] == 0) {
    if (protocol[[1]] == "http") {
      port <- 80
    } else if (protocol[[1]] == "https") {
      port <- 443
    } else if (protocol[[1]] == "cas") {
      port <- 5570
    } else {
      stop("CAS server port number was not specified")
    }
  }

  # Auto-detect protocol if still missing
  if (protocol[[1]] == "auto") {
    protocol <- .detect_protocol(hostname, port, protocol)
  }

  if (protocol[[1]] != "http" && protocol[[1]] != "https" && protocol[[1]] != "cas") {
    stop(paste("Unrecognized protocol for CAS server: ", protocol[[1]]))
  }

  # For http(s), construct URLs
  if (protocol[[1]] == "http" || protocol[[1]] == "https") {
    urls <- list()
    for (i in seq_len(length(hostname))) {
      url <- paste(protocol, "://", hostname[[i]], ":", port, sep = "")
      if (length(path) > 0 && path[[1]] != "") {
        url <- paste(url, "/", gsub("^/+", "", path[[1]], perl = TRUE), sep = "")
      }
      urls[[length(urls) + 1]] <- url
    }
    hostname <- do.call("paste", urls)
  } else {
    hostname <- do.call("paste", hostname)
  }

  return(list(hostname = hostname, port = as.numeric(port), username = username,
              password = password, protocol = protocol))
}

.get_token <- function(username=NULL, password=NULL, authcode=NULL, client_id=NULL,
                       client_secret=NULL, url=NULL) {
  if ( is.null(client_id) ) {
    client_id <- getOption('cas.client_id')
    if ( is.null(client_id) ) {
      client_id <- 'SWAT'
    }
  }

  if ( is.null(client_secret) ) {
    client_secret <- getOption('cas.client_secret')
    if ( is.null(client_secret) ) {
      client_secret <- ''
    }
  }

  if ( is.null(username) ) {
    username <- getOption('cas.username')
  }

  if ( is.null(password) ) {
    password <- getOption('cas.token')
  }

  config <- .setup_ssl()
  auth <- httr::authenticate(client_id, '')

  url <- httr::parse_url(url)
  url$path <- '/SASLogon/oauth/token'
  url$query <- NULL
  url$params <- NULL
  url$fragment <- NULL
  url$username <- NULL
  url$password <- NULL
  url <- httr::build_url(url)
  httr::handle_reset(url)

  if ( !is.null(authcode) ) {
    body <- paste0('grant_type=authorization_code',
                   '&code=', URLencode(authcode),
                   '&client_id=', URLencode(client_id),
                   '&client_secret=', URLencode(client_secret))
  } else if ( !is.null(username) && !is.null(password) ) {
    body <- paste0('grant_type=password',
                   '&username=', URLencode(username),
                   '&password=', URLencode(password))
  } else {
    stop('no authcode, username, or password was given')
  }

  res <- httr::POST(url, auth, config, body=body,
                    httr::add_headers('Accept'='application/vnd.sas.compute.session+json',
                                      'Content-Type'='application/x-www-form-urlencoded'))

  out <- httr::content(res, as='parsed', type='application/json', encoding='utf-8')

  return(out$access_token)
}

#' CAS Object Class
#'
#' An instance of this class represents a connection and session
#' between the client (R) and the server (SAS Cloud Analytic Services).
#'
#' @param hostname A \code{character} string that specifies the
#'   host name of the CAS controller.
#' @param port A \code{numeric} value that specifies the network
#'   port number that the CAS controller listens on.
#' @param protocol A \code{string} that specifies one of the following:
#'   \describe{
#'     \item{cas}{use binary communication. This is the default.}
#'     \item{http}{use HTTP communication with the REST interface
#'       on the CAS controller.}
#'     \item{https}{use HTTPS communication with the REST interface
#'       on the CAS controller. This protocol must be specified
#'       explicity.}
#'     \item{auto}{automatically detect between the binary and HTTP.}
#'   }
#' @param username A \code{character} string that identifies the
#'   user ID to authenticate as.
#' @param password A \code{character} string that specifies your
#'   password.
#' @param session A \code{character} string that identifies the
#'   32 character UUID of an existing session, if you want to
#'   connect to an existing session. This is rare.
#' @param locale A \code{character} string that specifies the
#'   locale to use for the CAS session. By default, the locale
#'   is set to the locale of the server.
#' @param authinfo A \code{character} string that specifies
#'   an alternative path to a .authinfo file that is used
#'   for authentication.  By default, ~/.authinfo is used on
#'   Linux and \%HOMEDRIVE\% \\\%HOMEPATH\%\\_authinfo is used on
#'   Windows.
#' @param path Base path of the connection URL
#' @param authcode Authorization code from SASLogon used to retrieve
#'   an OAuth token.
#'
#' @return A CAS object.
#'
#' @examples
#' \dontrun{
#' # URL form for binary communication and credentials from default authinfo location.
#' s <- CAS("cas://cloud.example.com:5570")
#'
#' # URL form for HTTP communication and credentials from default authinfo location.
#' s <- CAS("http://cloud.example.com:8777")
#'
#' # Use binary communication and credentals from the default authinfo location.
#' s <- CAS("cloud.example.com", 5570)
#'
#' # Use HTTPS and credentials from the default authinfo location.
#' s <- CAS("cloud.example.com", 8777, protocol = "https")
#'
#' # Use binary or HTTP communication and credentials from an alternative authinfo.
#' s <- CAS("cloud.example.com", 5570, protocol = "auto", authinfo = "~/alt.txt")
#'
#' # Use binary or HTTP communcation and specify credentials.
#' s <- CAS("cloud.example.com", 8777, protocol = "auto", username = "sasdemo", password = "!s3cret")
#' }
#'
#' @export
CAS <- setRefClass(
  Class = "CAS",

  fields = list(
    sw_connection = "ANY",
    sw_error = "ANY",
    soptions = "character",
    hostname = "character",
    port = "numeric",
    protocol = "character",
    username = "character",
    session = "character",
    performance = "list",
    severity = "numeric",
    statusCode = "numeric",
    reason = "character",
    status = "character",
    messages = "character",
    debug = "character",
    events = "list",
    serverFeatures = "character"
  ),

  methods = list(
    initialize = function(hostname = NULL, port = NULL, username = NULL,
                          password = NULL, protocol = "auto", path = NULL,
                          authcode = NULL, ...) {
      prototype <- NULL
      options <- list(...)

      info <- .get_connection_info(hostname, port, username, password, protocol, path)
      protocol <<- info$protocol
      hostname <<- info$hostname
      port <<- info$port
      username <<- info$username
      password <- info$password

      soptions <<- ""

      if (!is.null(options) && "locale" %in% names(options)) {
        soptions <<- gsub("^\\s+|\\s+$", "",
                          paste(soptions,
                                paste("locale=", options$locale, sep = ""), sep = " "))
      }

      if (!is.null(options) && "session" %in% names(options)) {
        soptions <<- gsub("^\\s+|\\s+$", "",
                          paste(soptions,
                                paste("session=", options$session, sep = ""), sep = " "))
      }

      soptions <<- gsub("^\\s+|\\s+$", "",
                        paste(soptions,
                              paste("protocol=", .self$protocol, sep = ""), sep = " "))

      authinfo <- NULL
      if (!is.null(options) && "authinfo" %in% names(options) && password == "") {
        authinfo <- options$authinfo
        if (class(options$authinfo) == "character" && length(options$authinfo) == 1) {
          if (!file.exists(options$authinfo)) {
            stop(paste("The specified authinfo file does not exist:", options$authinfo))
          }
          password <- paste("authinfo={{", options$authinfo, "}}", sep = "")
        }
        else if (class(options$authinfo) == "list" || length(options$authinfo) > 1) {
          found <- FALSE
          for (i in seq_len(length(options$authinfo))) {
            if (file.exists(options$authinfo[[i]])) {
              found <- TRUE
            }
          }
          if (!found) {
            msg <- "None of the specified authinfo files exist:"
            for (i in seq_len(length(options$authinfo))) {
              msg <- paste(msg, options$authinfo[[i]], sep = "\n    ")
            }
            stop(msg)
          }
          password <- ""
          for (i in seq_len(length(options$authinfo))) {
            password <- paste(password, "{", options$authinfo[[i]], "}", sep = "")
          }
          password <- paste("authinfo={", password, "}", sep = "")
        }
      }

      if (!is.null(options) && "prototype" %in% names(options)) {
        prototype <- options$prototype
      }

      if (.self$protocol == "http" || .self$protocol == "https") {
        if ( !is.null(authcode) )
        {
          .self$username <<- ''
          password <- .get_token(authcode=authcode, url=.self$hostname)
        }
        sw_error <<- REST_CASError(soptions)
        cas_connection_class <- REST_CASConnection
      }
      else if (!.binary_enabled()) { # nocov start
        stop(paste("The extension module for the CAS binary protocol can not be located;",
                   "only the CAS REST interface may be used.", sep = " "))
      } # nocov end
      else {
        sw_error <<- SW_CASError(soptions)
        cas_connection_class <- SW_CASConnection
      }

      if (is.null(prototype)) {
        conn <- cas_connection_class(.self$hostname, .self$port, .self$username,
                                     password, soptions, sw_error)
        .error_check(sw_error)
        sw_connection <<- conn
      }
      else {
        sw_connection <<- prototype$sw_connection$copy()
        .error_check(prototype$sw_connection)
      }

      # This should always be disabled.  Messages are printed by CASResponse.
      sw_connection$setBooleanOption("print_messages", FALSE)

      hostname <<- sw_connection$getHostname()
      .error_check(sw_connection)
      port <<- sw_connection$getPort()
      .error_check(sw_connection)
      username <<- sw_connection$getUsername()
      .error_check(sw_connection)
      session <<- sw_connection$getSession()
      .error_check(sw_connection)
      soptions <<- sw_connection$getSOptions()
      .error_check(sw_connection)
      callSuper(
        sw_connection = .self$sw_connection, sw_error = .self$sw_error,
        soptions = .self$soptions, hostname = .self$hostname, port = .self$port,
        username = .self$username, session = .self$session
      )

      # Get server features
      res <- .self$retrieve("builtins.reflect",
        action = "builtins.reflect",
        `_messagelevel` = "error"
      )
      params <- res$results[[1]]$actions[[1]]$params
      for (i in seq_len(length(params))) {
        if (params[[i]]$name == "levels") {
          serverFeatures <<- c(.self$serverFeatures, "reflection.levels")
        }
      }
      res <- .self$retrieve("builtins.about", `_messagelevel` = "error")
      if (as.numeric(res$results$About$Version) >= 3.5) {
        serverFeatures <<- c(.self$serverFeatures, "reflection.show.labels")
      }

      if (as.logical(getOption("cas.gen.function"))) {
        ml <- getOption("cas.message.level")
        options(cas.message.level = "error")
        actsets <- .list_action_sets(.self)
        if (!as.logical(getOption("cas.gen.function.sig"))) {
          message(paste("NOTE: To generate the functions with signatures (for tab completion), set ",
                        "      options(cas.gen.function.sig=TRUE).", sep="\n"))
        }
        for (i in seq_len(length(actsets$actionset))) {
          if (actsets$loaded[i] == 1) {
            .gen_functions(.self, actsets$actionset[i])
          }
        }
        options(cas.message.level = ml)
      }
      else {
          message(paste("NOTE: To generate the functions with signatures (for tab completion),",
                        "      set options(cas.gen.function.sig=TRUE).",
                  sep = "\n")
          )
      }

      if (grepl('^https?:', .self$hostname[[1]], perl = TRUE)) {
        conn_str <- paste0("library(swat);\n",
                           "conn <- swat::CAS('", .self$hostname[[1]], "'")
      } else {
        conn_str <- paste0("library(swat);\n",
                           "conn <- swat::CAS('", .self$protocol,
                           "://", .self$hostname[[1]], ":", .self$port, "'")
      }

      if (!is.null(authinfo)) {
        conn_str <- paste0(conn_str, ", authinfo = '", authinfo, "')")
      }

      .on_connection_opened(.self, conn_str)

      return(.self)
    },

    close = function() {
      # Close the connection to the server. This does not end the CAS session,
      # it just closes the connection. You must call the
      # \\code{session.endsession()} action to end a session.
      # 
      # Typically the \\code{cas.close()} function is used to close
      # connections. This function has a parameter that can also end the
      # session.
      sw_conn <- sw_connection
      .self$sw_connection <- NULL
      rc <- sw_conn$close()
      .on_connection_closed(.self)
      .error_check(sw_conn)
      return(rc)
    },

    isConnected = function() {
       return(!is.null(sw_connection))
    },

    copy = function() {
      # Return a copy of the CAS connection object
      return(CAS$new(hostname = "", port = 0, prototype = .self))
    },

    fork = function(num = 2) {
      # Return a vector of \\code{num} CAS connection objects. The
      # returned vector will always contain the current CAS connection
      # object as the first item.
      output <- c(.self)
      for (i in 1:(num - 1)) {
        output <- c(output, .self$copy())
      }
      return(output)
    },

    show = function() {
      # Print string representation of CAS connection
      cat("CAS(hostname=", hostname, ", port=", port, ", username=",
          username, ", session=", session, ", protocol=", protocol, ")\n", sep = "")
    },

    enableDataMessages = function() {
      # Enable data messages for table.addtable action.
      rc <- sw_connection$enableDataMessages()
      .error_check(sw_connection)
      return(rc)
    },

    disableDataMessages = function() {
      # Disable data messages after table.addtable action.
      rc <- sw_connection$enableDataMessages()
      .error_check(sw_connection)
      return(rc)
    },

    getone = function(...) {
      # Return the next response from the CAS server. The returned object
      # is a \\code{\\link{CASResponse}} object.
      args <- list(...)
      output <- NULL
      datamsghandler <- NULL
      if (!is.null(args$datamsghandler)) {
        .self$enableDataMessages()
        datamsghandler <- args$datamsghandler
      }
      sw_message <- sw_connection$receive()
      .error_check(sw_connection)
      if (!is.null(sw_message) && !sw_message$isNULL()) {
        t <- sw_message$getType()
        if (t == "response") {
          sw_response <- sw_message$toResponse(sw_connection)
          .error_check(sw_message)
          if (!is.null(sw_response) && !sw_message$isNULL()) {
            output <- CASResponse$new(sw_response = sw_response)
          }
        }
        else if (t == "request" && !is.null(datamsghandler)) {
          sw_request <- sw_message$toRequest(sw_connection)
          .error_check(sw_message)
          if (!is.null(sw_request) && !sw_message$isNULL()) {
            output <- datamsghandler$call(CASRequest$new(sw_request = sw_request), .self)
          }
        }
        else if (t == "request") {
          sw_request <- sw_message$toRequest(sw_connection)
          .error_check(sw_message)
          if (!is.null(sw_request) && !sw_message$isNULL()) {
            output <- CASRequest$new(sw_request = sw_request)
          }
        }
      }
      if (!is.null(datamsghandler)) {
        .self$disableDataMessages()
      }
      return(output)
    },

    invoke = function(.action, ...) {
      # Invoke an action on the CAS server and return. The \\code{.action}
      # parameter is a string containing the action name. All other arguments
      # are passed to the CAS action.

      # This method is only called in special circumstances where you want
      # to run an action in the background and retrieve the responses
      # manually. Typically, the generated action wrappers 
      # (e.g., \\code{cas.table.columnInfo()}, \\code{cas.simple.summary()},
      # and so on) are used to make action calls.
      args <- list(...)
      if (class(.self$sw_connection) == "REST_CASConnection") {
        sw_connection$invoke(.action, args)
      } else {
        if (grepl("nil", capture.output(slot(sw_connection, "ref")))) {
          stop("The connection object is invalid.  Please create a new connection.")
        }
        sw_connection$setBooleanOption("trace_actions",
                                       as.logical(getOption("cas.trace.actions")))
        sw_connection$setBooleanOption("trace_ui_actions",
                                       as.logical(getOption("cas.trace.ui.actions")))
        sw_connection$invoke(.action, .r2cas(.self$soptions, .self$sw_error, args))
      }
      return(.self)
    },

    generate_wrappers = function(.action, ...) {
      # Generate functions on loadactionset.
      args <- list(...)
      if (as.logical(getOption("cas.gen.function"))) {
        if (tolower(.action) == "builtins.loadactionset" ||
            tolower(.action) == "loadactionset") {
          if (!is.null(args$actionSet)) {
            res <- .gen_functions(.self, args$actionSet)
            .check_for_cas_errors(res)
          } else if (!is.null(args$actionset)) {
            res <- .gen_functions(.self, args$actionset)
            .check_for_cas_errors(res)
          }
        } else if (tolower(.action) == "builtins.defineactionset" ||
                   tolower(.action) == "defineactionset") {
          if (!is.null(args$name)) {
            res <- .gen_functions(.self, args$name)
            .check_for_cas_errors(res)
          }
        }
      }
    },

    retrieve = function(.action, ..., stop.on.error = FALSE) {
      # Invoke a CAS action and return the results. The \\code{.action} parameter
      # is a string containing the name of the action. All other arguments
      # are passed to the CAS action. 
      #
      # The results are returned in a list with the following fields:
      # 
      # \\describe{
      #   \\item{\\code{results}}{The results of the CAS action.}
      #   \\item{\\code{messages}}{Any message printed by the action.}
      #   \\item{\\code{performance}}{Performance metrics.}
      #   \\item{\\code{disposition}}{Information about errors or warning produced
      #     during the action run.}
      #   \\item{\\code{events}}{Events posted by the server; this typically
      #     corresponds to things like caslibs and tables being updated.}
      # }
      #
      # This method is not typically called directly (although it can be).
      # The generated action wrappers (e.g., \\code{cas.table.columnInfo()},
      # \\code{cas.simple.summary()}, and so on) call this method behind
      # the scenes and return the \\code{results} field.
      args <- list(...)
      datamsghandler <- NULL
      if (!is.null(args$datamsghandler)) {
        datamsghandler <- args$datamsghandler
      }
      if (is.null(args$`_messagelevel`)) {
        args$`_messagelevel` <- as.character(getOption("cas.message.level"))
      }
      args[[".action"]] <- .action
      repeat {
        do.call(.self$invoke, args)
        output <- list()
        results <- list()
        msgs <- character()
        evts <- list()
        idx <- 1
        while (TRUE) {
          nextresp <- getnext(.self, datamsghandler = datamsghandler)
          if (is.null(nextresp$response)) break

          while (TRUE) {
            result <- getnext(nextresp$response)
            if (is.null(result) || length(result) < 1) break
            for (i in seq_len(length(result))) {
              key <- names(result[i])
              if (is.null(key) || nchar(key) == 0) {
                results[[idx]] <- result[[i]]
                idx <- idx + 1
              }
              else if (substr(key, 1, 1) == "$") {
                evts[[key]] <- result[[i]]
              }
              else {
                results[[key]] <- result[[i]]
              }
            }
          }

          msgs <- c(msgs, nextresp$response$messages)

          output[["performance"]] <- nextresp$response$performance
          output[["disposition"]] <- nextresp$response$disposition
        }
        if (!is.null(output[["disposition"]][["statusCode"]]) &&
          output[["disposition"]][["statusCode"]] != retry_action_code) {
          break
        }
      }
      output[["messages"]] <- msgs
      output[["results"]] <- results
      output[["events"]] <- evts

      .self$performance <- output[["performance"]]
      .self$severity <- output[["disposition"]][["severity"]]
      .self$statusCode <- output[["disposition"]][["statusCode"]]
      .self$reason <- output[["disposition"]][["reason"]]
      .self$status <- output[["disposition"]][["status"]]
      .self$debug <- output[["disposition"]][["debug"]]
      .self$messages <- output[["messages"]]
      .self$events <- output[["events"]]

      if (stop.on.error) {
        .check_for_cas_errors(output)
      }

      # Generate action wrappers for loadActionSet / defineActionSet
      .self$generate_wrappers(.action, ...)

      if (.self$severity > 1) {
        return(invisible(output))
      }

      .action <- tolower(.action)
      if (.action == "table.loadtable" || .action == "loadtable" ||
          .action == "table.addcaslib" || .action == "addcaslib") {
        .on_connection_updated(.self, "")
      }

      return(output)
    },

    upload = function(data, stop.on.error = FALSE, ...) {
      # Upload a \code{file} or \code{data.frame} to a CAS table. The
      # \code{data} argument must be either a string containing a path
      # to a file or URL, or a \code{data.frame}. The remaining arguments
      # are passed to the \code{table.addtable} action.
      #
      # The return value is a list with the following fields:
      # \describe{
      #   \item{\code{results}}{The results of the CAS action.}
      #   \item{\code{messages}}{Any message printed by the action.}
      #   \item{\code{performance}}{Performance metrics.}
      #   \item{\code{disposition}}{Information about errors or warning produced
      #     during the action run.}
      # }
      filename <- data
      args <- list(...)
      tmp <- ""

      if (class(data) == "character") {
        if (grepl("^(https?|ftps?|file)://", data, perl = TRUE)) {
          tmp <- tempfile()
          filename <- tmp
          name <- basename(data)
          ext <- tools::file_ext(data)
          download.file(data, destfile = tmp, method = "auto", quiet = TRUE)
        }
        else {
          if (!file.exists(data)) {
            stop(paste("The file", data, "does not exist."))
          }
          ext <- tools::file_ext(data)
          name <- tools::file_path_sans_ext(basename(data))
        }
      }
      else {
        ext <- "csv"
        tmp <- tempfile()
        filename <- tmp
        name <- basename(tmp)
        write.csv(data, file = tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      }

      if (!("casout" %in% names(args)) && !("casOut" %in% names(args))) {
        args[["casout"]] <- name
      }

      if (!("importoptions" %in% names(args)) && !("importOptions" %in% names(args))) {
        if (ext == "sav") {
          ext <- "spss"
        } else if (ext == "xlsx") {
          ext <- "excel"
        } else if (ext == "sashdat") {
          ext <- "hdat"
        } else if (ext == "sas7bdat") ext <- "basesas"
        args[["importoptions"]] <- list(filetype = ext)
      }

      if (class(.self$sw_connection) == "REST_CASConnection") {
        response <- sw_connection$upload(filename, args)
      } else {
        # The upload method may print messages.  This is the only way to see them.
        sw_connection$setBooleanOption("print_messages", getOption("cas.print.messages"))
        response <- sw_connection$upload(filename, .r2cas(.self$soptions, .self$sw_error, args))
        sw_connection$setBooleanOption("print_messages", FALSE)
      }

      if (tmp != "") unlink(tmp)

      output <- list()
      results <- list()
      msgs <- list()
      idx <- 1
      if (!is.null(response)) {
        response <- CASResponse(response)
        while (TRUE) {
          result <- getnext(response)
          if (is.null(result)) break
          for (i in seq_len(length(result))) {
            key <- names(result[i])
            if (is.null(key) || nchar(key) == 0) {
              results[[idx]] <- result[[i]]
              idx <- idx + 1
            }
            else {
              results[[key]] <- result[[i]]
            }
          }
        }

        msgs <- c(msgs, response$messages)

        output[["performance"]] <- response$performance
        output[["disposition"]] <- response$disposition
      }
      output[["messages"]] <- msgs
      output[["results"]] <- results

      if (stop.on.error) {
        .check_for_cas_errors(output)
      }

      .on_connection_updated(.self, "")

      return(output)
    }
  )
)

setGeneric("cas.invoke",
  function(.x, .action, ...) {
    standardGeneric("cas.invoke")
  }
)

#' Invoke an action on the CAS server and return immediately
#'
#' The \code{.action} parameter is a string containing the action
#' name. All other arguments are passed to the CAS action.
#'
#' This method is only called in special circumstances where you want
#' to run an action in the background and retrieve the responses
#' manually. Typically, the generated action wrappers 
#' (e.g., \code{cas.table.columninfo()}, \code{cas.simple.summary()},
#' and so on) are used to make action calls.
#'
#' @param .x      \code{CAS} connection object.
#' @param .action   String containing action name.
#' @param \ldots Action parameters.
#'
#' @return CAS connection object
#'
#' @export
setMethod(
  "cas.invoke",
  signature(.x = "CAS"),
  function(.x, .action, ...) {
    return(invisible(.x$invoke(.action, ...)))
  }
)

#' Invoke an action on the CAS server and return immediately
#'
#' The \code{.action} parameter is a string containing the action
#' name. All other arguments are passed to the CAS action.
#'
#' This method is only called in special circumstances where you want
#' to run an action in the background and retrieve the responses
#' manually. Typically, the generated action wrappers 
#' (e.g., \code{cas.table.columninfo()}, \code{cas.simple.summary()},
#' and so on) are used to make action calls.
#'
#' @param .x      \code{CASTable} object.
#' @param .action   String containing action name.
#' @param \ldots Action parameters.
#'
#' @return CAS connection object
#'
#' @export
setMethod(
  "cas.invoke",
  signature(.x = "CASTable"),
  function(.x, .action, ...) {
    return(invisible(cas.invoke(.x@conn, .action, ...)))
  }
)

setGeneric("cas.retrieve",
  function(.x, .action, ..., stop.on.error = FALSE) {
    standardGeneric("cas.retrieve")
  }
)

#' Invoke a CAS action and return the results
#'
#' The \code{.action} parameter is a string containing the name of
#' the action. All other arguments are passed to the CAS action. 
#'
#' The results are returned in a list with the following fields:
#'
#' \describe{
#'   \item{\code{results}}{The results of the CAS action.}
#'   \item{\code{messages}}{Any message printed by the action.}
#'   \item{\code{performance}}{Performance metrics.}
#'   \item{\code{disposition}}{Information about errors or warning produced
#'     during the action run.}
#'   \item{\code{events}}{Events posted by the server; this typically
#'     corresponds to things like caslibs and tables being updated.}
#' }
#'
#' This method is not typically called directly (although it can be).
#' The generated action wrappers (e.g., \code{cas.table.columnInfo()},
#' \code{cas.simple.summary()}, and so on) call this method behind
#' the scenes and return the \code{results} field.
#'
#' @param .x      \code{CAS} connection object.
#' @param .action   String containing action name.
#' @param \ldots Action parameters.
#'
#' @return 
#'
#' @export
setMethod(
  "cas.retrieve",
  signature(.x = "CAS"),
  function(.x, .action, ..., stop.on.error = FALSE) {
    res <- .x$retrieve(.action, ..., stop.on.error = stop.on.error)
    if (res$disposition$severity > 1) {
      return(invisible(res))
    }
    return(res)
  }
)

#' Invoke a CAS action and return the results
#'
#' The \code{.action} parameter is a string containing the name of
#' the action. All other arguments are passed to the CAS action. 
#'
#' The results are returned in a list with the following fields:
#'
#' \describe{
#'   \item{\code{results}}{The results of the CAS action.}
#'   \item{\code{messages}}{Any message printed by the action.}
#'   \item{\code{performance}}{Performance metrics.}
#'   \item{\code{disposition}}{Information about errors or warning produced
#'     during the action run.}
#'   \item{\code{events}}{Events posted by the server; this typically
#'     corresponds to things like caslibs and tables being updated.}
#' }
#'
#' This method is not typically called directly (although it can be).
#' The generated action wrappers (e.g., \code{cas.table.columnInfo()},
#' \code{cas.simple.summary()}, and so on) call this method behind
#' the scenes and return the \code{results} field.
#'
#' @param .x      \code{CASTable} object.
#' @param .action   String containing action name.
#' @param \ldots Action parameters.
#'
#' @return 
#'
#' @export
setMethod(
  "cas.retrieve",
  signature(.x = "CASTable"),
  function(.x, .action, ..., stop.on.error = FALSE) {
    # TODO: Need reflection information to verify action needs a table= param.
    args <- list(...)
    if (is.null(args$table)) {
      res <- .x@conn$retrieve(.action, table = .x, ..., stop.on.error = stop.on.error)
    } else {
      res <- .x@conn$retrieve(.action, ..., stop.on.error = stop.on.error)
    }
    if (res$disposition$severity > 1) {
      return(invisible(res))
    }
    return(res)
  }
)

setGeneric("cas.copy",
  function(x) {
    standardGeneric("cas.copy")
  }
)

#' Return a copy of the CAS connection object
#'
#' The new connection will use all of the same settings as the original,
#' but it will be attached to a new session.
#'
#' @param x \code{CAS} connection object.
#'
#' @return \code{CAS} connection object
#'
#' @export
setMethod(
  "cas.copy",
  signature(x = "CAS"),
  function(x) {
    return(x$copy())
  }
)

#' Return a copy of the CASTable object
#'
#' A new \code{CASTable} object will be created that references the
#' same table in the server as the original \code{CASTable} object.
#'
#' Note that this does *not* create a new table on the server.
#'
#' @param x \code{CASTable} object.
#'
#' @return \code{CASTable} object
#'
#' @export
#' @export
setMethod(
  "cas.copy",
  signature(x = "CASTable"),
  function(x) {
    tbl <- CASTable(x@conn, x@tname, caslib = c(x@caslib),
                    columns = c(x@names),
                    where = c(x@where), orderby = c(x@orderby),
                    groupby = c(x@groupby), gbmode = c(x@gbmode),
                    computedOnDemand = x@computedOnDemand,
                    computedVars = c(x@computedVars),
                    computedVarsProgram = c(x@computedVarsProgram))
    tbl@.computedVarsProgram <- c(x@.computedVarsProgram)
    tbl@.computedVars <- c(x@.computedVars)
    tbl@.compcomp <- x@.compcomp
    return(tbl)
  }
)

setGeneric("cas.fork",
  function(x, num = 2) {
    standardGeneric("cas.fork")
  }
)

#' Return a vector of \code{num} CAS connection objects
#'
#' The returned vector will always contain the current CAS connection
#' object as the first item.
#'
#' @param conn CAS connection object.
#' @param num  Integer indicating the number of returned objects.
#'
#' @return Vector of CAS connection objects
#'
#' @export
setMethod(
  "cas.fork",
  signature(x = "CAS"),
  function(x, num = 2) {
    return(x$fork(num))
  }
)

#' Event watcher for multiple CAS connections
#'
#' Event watchers monitor one or more CAS connections for responses coming
#' from the server. When a connection gets a response, the event watcher
#' returns the connection where the event occurred. This process is
#' handled in the \code{getnext()} function.
#'
#' @param connections A list of \code{CAS} connections.
#' @param \ldots      Options for the event watcher including the following:
#'   \describe{
#'     \item{timeout}{Integer indicating the connection timeout in seconds.}
#'     \item{datamsghandler}{A \code{CASDataMsgHandler} class used to handle
#'       data upload requests when calling the \code{table.addtable} action.}
#'   }
#'
#' @field sw_watcher The watcher object in the C extension.
#' @field connections A list of \code{CAS} connections.
#' @field datamsghandler A data message handler class used for actions
#'   that require data uploads.
#'
#' @noRd
CASEventWatcher <- setRefClass(
  Class = "CASEventWatcher",

  fields = list(
    sw_watcher = "ANY",
    connections = "list",
    datamsghandler = "ANY"
  ),

  methods = list(
    initialize = function(connections, ...) {
      if (class(connections) == "list") {
        connections <<- connections
      }
      else {
        connections <<- list(connections)
      }

      rest <- 0
      binary <- 0
      for (i in seq_len(length(connections))) {
        if (grepl("^REST_", class(connections[[i]]$sw_connection), perl = TRUE)) {
          rest <- rest + 1
        } else {
          binary <- binary + 1
        }
      }
      if (rest > 0 && binary > 0) {
        stop("REST and binary connections can not be mixed")
      }

      options <- list(...)
      if (!is.null(options) && "timeout" %in% names(options)) {
        timeout <- options$timeout
      }
      else {
        timeout <- 0
      }
      if (!is.null(options) && "datamsghandler" %in% names(options)) {
        datamsghandler <<- options$datamsghandler
      }
      else {
        datamsghandler <<- NULL
      }

      if (binary) {
        sw_watcher <<- SW_CASConnectionEventWatcher(
          length(connections), timeout,
          connections[[1]]$soptions, connections[[1]]$sw_error
        )
      }
      else {
        sw_watcher <<- REST_CASConnectionEventWatcher(
          length(connections), timeout,
          connections[[1]]$soptions, connections[[1]]$sw_error
        )
      }
      .error_check(connections[[1]]$sw_error)

      for (i in seq_len(length(connections))) {
        sw_watcher$addConnection(connections[[i]]$sw_connection)
        .error_check(sw_watcher)
      }

      callSuper(
        sw_watcher = .self$sw_watcher, connections = .self$connections,
        datamsghandler = .self$datamsghandler
      )
      return(.self)
    }
  )
)

#' Return the next response or result from given object
#'
#' This function is typically used in conjunction with \code{CAS$invoke()}.
#' It can be used to retrieve responses from multiple CAS action calls
#' as they become available.
#'
#' @param \ldots One or more \code{\link{CAS}},
#'   \code{\link{CASEventWatcher}}, or \code{\link{CASResponse}} objects.
#'
#' @return If the arguments given are \code{CAS} or \code{CASEventWatcher}
#'   objects, the return value is a two-element list with fields \code{response}
#'   and \code{connection} corresponding to the next retrieved response and
#'   the connection that it came from. If there are no more respnoses, a NULL
#'   is returned. If the arguments given, are \code{CASResponse} objects,
#'   the next value in the response is returned as an R object.
#'
#' @examples
#' \dontrun{
#'   while (TRUE) {
#'     nextresp <- getnext(.self, datamsghandler = datamsghandler)
#'     if (is.null(nextresp$response)) break
#'
#'     while (TRUE) {
#'       result <- getnext(nextresp$response)
#'       if (is.null(result)) break
#'
#'       print(result)
#'     }
#'   }
#' }
#'
#' @export
getnext <- function(...) {
  args <- list(...)
  connections <- args[unlist(lapply(args, function(x) class(x) == "CAS"))]
  responses <- args[unlist(lapply(args, function(x) class(x) == "CASResponse"))]
  watchers <- args[unlist(lapply(args, function(x) class(x) == "CASEventWatcher"))]
  kwargs <- args[unlist(lapply(args, function(x) class(x) != "CAS"))]

  # Get next result from response
  if (length(responses) > 0) {
    sw_result <- responses[[1]]$sw_response$getNextResult()
    .error_check(responses[[1]]$sw_response)
    if (is.null(sw_result) || sw_result$isNULL()) {
      return(NULL)
    }
    output <- list()
    key <- sw_result$getKey()
    .error_check(sw_result)
    if (is.null(key) || nchar(key) == 0) {
      val <- .cas2r(sw_result)
      if (!is.null(val)) {
        if (length(val) > length(output[1])) {
          output[[1]] <- val
        } else {
          output[1] <- val
        }
      }
    }
    else {
      val <- .cas2r(sw_result)
      if (!is.null(val)) {
        output[[key]] <- val
      }
    }
    return(output)
  }

  # Handle multiple connections
  else if (length(watchers) > 0) {
    watcher <- watchers[[1]]
    while (TRUE) {
      idx <- watcher$sw_watcher$wait()
      .error_check(watcher$sw_watcher)

      # Finished
      if (idx == -2) {
        break
      }

      # Timeout
      # TODO: Return something else so they know it's a timeout
      if (idx == -1) {
        return(NULL, NULL)
      }

      # TODO: Merge datamsghandler from watcher
      # TODO: Return response and connection
      return(list(
        response = do.call(watcher$connections[[idx + 1]]$getone, kwargs),
        connection = watcher$connections[[idx + 1]]
      ))
    }
  }

  # Get next response from connection
  else if (length(connections) > 0) {
    return(list(
      response = do.call(connections[[1]]$getone, kwargs),
      connection = connections[[1]]
    ))
  }

  return(NULL)
}

#' Utility function for guaranteeing a string result
#'
#' @param str An arbitrary string
#'
#' @return A string is always returned. If the given value was NULL,
#'   an empty string is returned; otherwise, the string argument
#'   given is returned.
#'
#' @noRd
.no_null_string <- function(str) {
  if (is.null(str)) {
    return("")
  }
  return(str)
}

#' Convert a CASValue to an R object
#'
#' @param sw_value The \code{CASValue} to convert
#'
#' @return An R object corresponding to the given value.
#'
#' @noRd
.cas2r <- function(sw_value) {
  t <- sw_value$getType()
  .error_check(sw_value)
  if (t == "nil") {
    return(NULL)
  } else if (t == "int32") {
    out <- sw_value$getInt32()
    .error_check(sw_value)
    return(out)
  } else if (t == "int64") {
    out <- sw_value$getInt64AsString()
    .error_check(sw_value)
    return(.as.integer64(out))
  } else if (t == "double") {
    out <- sw_value$getDouble()
    .error_check(sw_value)
    return(out)
  } else if (t == "string") {
    out <- sw_value$getString()
    if (is.null(out)) {
      out <- ""
    }
    .error_check(sw_value)
    return(out)
  } else if (t == "boolean") {
    out <- sw_value$getBoolean()
    .error_check(sw_value)
    return(out)
  } else if (t == "date") {
    out <- sw_value$getDate()
    .error_check(sw_value)
    return(CASd.as.Date(out))
  } else if (t == "time") {
    out <- sw_value$getTimeAsString()
    .error_check(sw_value)
    return(CASdt.as.POSIXct(out))
  } else if (t == "datetime") {
    out <- sw_value$getDateTimeAsString()
    .error_check(sw_value)
    return(CASdt.as.POSIXct(out))
  } else if (t == "table") {
    sw_table <- sw_value$getTable()
    .error_check(sw_value)
    n_cols <- sw_table$getNColumns()
    .error_check(sw_table)
    n_rows <- sw_table$getNRows()
    .error_check(sw_table)
    table <- NULL

    name <- .no_null_string(sw_table$getName())
    .error_check(sw_table)
    label <- .no_null_string(sw_table$getLabel())
    .error_check(sw_table)
    title <- .no_null_string(sw_table$getTitle())
    .error_check(sw_table)

    # Get table extended attributes
    if (class(sw_table) == "REST_CASTable") {
      attrs <- sw_table$getAttributes()
    }
    else {
      attrs <- list()
      while (TRUE) {
        key <- sw_table$getNextAttributeKey()
        .error_check(sw_table)
        if (is.null(key)) break
        typ <- sw_table$getAttributeType(key)
        .error_check(sw_table)

        if (typ == "int32") {
          attrs[[key]] <- sw_table$getInt32Attribute(key)
          .error_check(sw_table)
        }
        else if (typ == "int64") {
          attrs[[key]] <- sw_table$getInt64AttributeAsString(key)
          .error_check(sw_table)
          attrs[[key]] <- .as.integer64(attrs[[key]])
        }
        else if (typ == "double") {
          attrs[[key]] <- sw_table$getDoubleAttribute(key)
          .error_check(sw_table)
        }
        else if (typ == "string") {
          attrs[[key]] <- .no_null_string(sw_table$getStringAttribute(key))
          .error_check(sw_table)
        }
        else if (typ == "date") {
          attrs[[key]] <- sw_table$getInt32Attribute(key)
          .error_check(sw_table)
          attrs[[key]] <- CASd.as.Date(attrs[[key]])
        }
        else if (typ == "time") {
          attrs[[key]] <- sw_table$getInt64AttributeAsString(key)
          .error_check(sw_table)
          attrs[[key]] <- CASdt.as.POSIXct(attrs[[key]])
        }
        else if (typ == "datetime") {
          attrs[[key]] <- sw_table$getInt64AttributeAsString(key)
          .error_check(sw_table)
          attrs[[key]] <- CASdt.as.POSIXct(attrs[[key]])
        }
        else if (typ == "int32-array") {
          nitems <- sw_table$getAttributeNItems()
          .error_check(sw_table)
          value <- list()
          for (i in 1:nitems) {
            value[[i]] <- sw_table$getInt32ArrayAttributeItem(key, i - 1)
            .error_check(sw_table)
          }
          attrs[[key]] <- value
        }
        else if (typ == "int64-array") {
          nitems <- sw_table$getAttributeNItems()
          .error_check(sw_table)
          value <- list()
          for (i in 1:nitems) {
            value[[i]] <- sw_table$getInt64ArrayAttributeItemAsString(key, i - 1)
            .error_check(sw_table)
            value[[i]] <- .as.integer64(value[[i]])
          }
          attrs[[key]] <- value
        }
        else if (typ == "double-array") {
          nitems <- sw_table$getAttributeNItems()
          .error_check(sw_table)
          value <- list()
          for (i in 1:nitems) {
            value[[i]] <- sw_table$getDoubleArrayAttributeItem(key, i - 1)
            .error_check(sw_table)
          }
          attrs[[key]] <- value
        }
      }
    }

    col_labels <- c()
    col_formats <- c()
    col_types <- c()
    col_widths <- c()
    col_sizes <- c()

    if (n_cols > 0) {
      for (col in 0:(n_cols - 1)) {
        col_labels <- c(col_labels, .no_null_string(sw_table$getColumnLabel(col)))
        .error_check(sw_table)
        col_formats <- c(col_formats, .no_null_string(sw_table$getColumnFormat(col)))
        .error_check(sw_table)
        col_widths <- c(col_widths, sw_table$getColumnWidth(col))
        .error_check(sw_table)
        col_types <- c(col_types, sw_table$getColumnType(col))
        .error_check(sw_table)
        col_sizes <- c(col_sizes, list(1, sw_table$getColumnArrayNItems(col)))
        .error_check(sw_table)
      }
    }

    # Get column extended attributes
    if (class(sw_table) == "REST_CASTable") {
      col_attrs <- sw_table$getColumnAttributes()
    }
    else {
      # Get column extended attributes
      col_attrs <- c()
      if (n_cols > 0) {
        for (col in 0:(n_cols - 1)) {
          info <- list()
          col_attrs <- c(col_attrs, info)

          while (TRUE) {
            key <- sw_table$getNextColumnAttributeKey(col)
            .error_check(sw_table)
            if (is.null(key)) break
            typ <- sw_table$getColumnAttributeType(col, key)
            .error_check(sw_table)

            if (typ == "double") {
              info[[key]] <- sw_table$getColumnDoubleAttribute(col, key)
              .error_check(sw_table)
            }
            else if (typ == "int32") {
              info[[key]] <- sw_table$getColumnInt32Attribute(col, key)
              .error_check(sw_table)
            }
            else if (typ == "int64") {
              info[[key]] <- sw_table$getColumnInt64AttributeAsString(col, key)
              .error_check(sw_table)
              info[[key]] <- .as.integer64(info[[key]])
            }
            else if (typ == "string") {
              info[[key]] <- .no_null_string(sw_table$getColumnStringAttribute(col, key))
              .error_check(sw_table)
            }
            else if (typ == "date") {
              info[[key]] <- sw_table$getColumnInt32Attribute(col, key)
              .error_check(sw_table)
              info[[key]] <- CASd.as.Date(info[[key]])
            }
            else if (typ == "time") {
              info[[key]] <- sw_table$getColumnInt64AttributeAsString(col, key)
              .error_check(sw_table)
              info[[key]] <- CASdt.as.POSIXct(info[[key]])
            }
            else if (typ == "datetime") {
              info[[key]] <- sw_table$getColumnInt64AttributeAsString(col, key)
              .error_check(sw_table)
              info[[key]] <- CASdt.as.POSIXct(info[[key]])
            }
            else if (typ == "int32-array") {
              nitems <- sw_table$getColumnAttributeNItems(col, key)
              .error_check(sw_table)
              value <- list()
              for (i in 1:nitems) {
                value[[i]] <- sw_table$getColumnInt32ArrayAttributeItem(col, key, i - 1)
                .error_check(sw_table)
              }
              info[[key]] <- value
            }
            else if (typ == "int64-array") {
              nitems <- sw_table$getColumnAttributeNItems(col, key)
              .error_check(sw_table)
              value <- list()
              for (i in 1:nitems) {
                value[[i]] <- sw_table$getColumnInt64ArrayAttributeItemAsString(col, key, i - 1)
                .error_check(sw_table)
                value[[i]] <- .as.integer64(value[[i]])
              }
              info[[key]] <- value
            }
            else if (typ == "double-array") {
              nitems <- sw_table$getColumnAttributeNItems(col, key)
              .error_check(sw_table)
              value <- list()
              for (i in 1:nitems) {
                value[[i]] <- sw_table$getColumnDoubleArrayAttributeItem(col, key, i - 1)
                .error_check(sw_table)
              }
              info[[key]] <- value
            }
          }
        }
      }
    }

    add_column <- function(table, newname, newcol) {
      if (is.null(table)) {
        table <- data.frame(newcol, stringsAsFactors = FALSE)
        names(table) <- newname
      } else {
        table[newname] <- list(newcol)
      }
      return(table)
    }

    add_bygroup_columns <- function(table) {
      if (getOption("cas.bygroup.mode") == "none") {
        return(table)
      }

      grpnum <- 1
      grpvar_lbl <- paste("ByVar", grpnum, sep = "")
      grpval_lbl <- paste("ByVar", grpnum, "Value", sep = "")
      grpvalf_lbl <- paste("ByVar", grpnum, "ValueFormatted", sep = "")

      while (!is.null(attrs[[grpvar_lbl]])) {
        varname <- attrs[[grpvar_lbl]]
        rawval <- attrs[[grpval_lbl]]
        fmtval <- attrs[[grpvalf_lbl]]

        if (getOption("cas.bygroup.mode") == "raw") {
          table <- add_column(table, varname, rep(rawval, n_rows))
        }
        else if (getOption("cas.bygroup.mode") == "formatted") {
          table <- add_column(table, varname, rep(fmtval, n_rows))
        }
        else if (getOption("cas.bygroup.mode") == "both") {
          table <- add_column(table, varname, rep(rawval, n_rows))
          table <- add_column(
            table,
            paste(varname,
              getOption("cas.bygroup.dup.suffix"),
              sep = ""
            ),
            rep(fmtval, n_rows)
          )
        }
        else {
          stop(paste(
            "Unrecognized value for cas.bygroup.mode:",
            getOption("cas.bygroup.mode")
          ))
        }

        grpnum <- grpnum + 1
        grpvar_lbl <- paste("ByVar", grpnum, sep = "")
        grpval_lbl <- paste("ByVar", grpnum, "Value", sep = "")
        grpvalf_lbl <- paste("ByVar", grpnum, "ValueFormatted", sep = "")
      }

      return(table)
    }

    int32_missval <- -2147483648
    int64_missval <- "-9223372036854775808"
    set_missing <- function(value, missval) {
      if (is.na(value) || is.nan(value)) {
        return(NA)
      }
      if (value == missval) {
        return(NA)
      }
      return(value)
    }

    get_transformer <- function (type) {
      if ( type == "int64" )
        return(function (out) .as.integer64(set_missing(out[[1]], int64_missval)))
      else if ( type == "int64-array" )
        return(function (out) .as.integer64(set_missing(out[[1]], int64_missval)))
      else if ( type == "date" )
        return(CASd.as.Date)
      else if ( type == "time" )
        return(CASdt.as.POSIXct)
      else if ( type == "datetime" )
        return(CASdt.as.POSIXct)
      else
        return(NULL)
    }

    vectors <- NULL
    tryCatch({
      if ( nCols > 0 ) {
        vectors <- sw_table$toVectors()
      }
    }, error = function (e) {
       e <- as.character(e)
       if (!grepl("not available for .Call", e)) {
          stop(e)
       }
    }, silent = TRUE)

    # Use vectors if it exists, it will be much faster
    if ( !is.null(vectors) ) {
      col.names <- c()
      col.transformers <- list()
      for (col in 0 : (n_cols - 1)) {
        name <- sw_table$getColumnName(col)
        .error_check(sw_table)
        len <- sw_table$getColumnArrayNItems(col)
        .error_check(sw_table)
        type <- sw_table$getColumnType(col)
        .error_check(sw_table)
        if ( len == 1 ) {
          col.names <- c(col.names, name)
          col.transformers[[name]] <- get_transformer(type)
        } else {
          for (i in 1:len) {
            col.names <- c(col.names, paste0(name, i))
            col.transformers[[paste0(name, i)]] <- get_transformer(type)
          }
        }
      }
      table <- as.data.frame(sw_table$toVectors(),
                             stringsAsFactors = FALSE,
                             col.names = col.names,
                             check.names = FALSE)
      if ( nrows > 0 ) {
        for (col in names(col.transformers)) {
          table[col] <- lapply(table[col], col.transformers[[col]])
        }
      }
      table <- add_bygroup_columns(table)
      table <- table[c(setdiff(names(table), col.names), col.names)]
    }

    else if (n_cols > 0) {
      table <- add_bygroup_columns(table)

      for (col in 0:(n_cols - 1)) {
        column <- c()
        t <- sw_table$getColumnType(col)
        .error_check(sw_table)
        len <- sw_table$getColumnArrayNItems(col)
        .error_check(sw_table)
        name <- sw_table$getColumnName(col)
        .error_check(sw_table)

        if (n_rows == 0) {
          table <- add_column(table, name, character())
        }
        else if (t == "int32") {
          for (row in 0:(n_rows - 1)) {
            out <- sw_table$getInt32Value(row, col)
            .error_check(sw_table)
            if (identical(out, numeric(0))) {
              out <- 0
            }
            column <- c(column, set_missing(out, int32_missval))
          }
          table <- add_column(table, name, as.integer(column))
        }
        else if (t == "int32-array") {
          for (elem in 0:(len - 1)) {
            column <- c()
            for (row in 0:(n_rows - 1)) {
              out <- sw_table$getInt32ArrayValue(row, col, elem)
              .error_check(sw_table)
              if (identical(out, numeric(0))) {
                out <- 0
              }
              column <- c(column, set_missing(out, int32_missval))
            }
            table <- add_column(table, paste(name, elem + 1, sep = ""), as.integer(column))
          }
        }
        else if (t == "int64") {
          column <- c()
          for (row in 0:(n_rows - 1)) {
            out <- sw_table$getInt64ValueAsString(row, col)
            .error_check(sw_table)
            if (identical(out, numeric(0))) {
              out <- 0
            }
            column <- c(column, .as.integer64(set_missing(out, int64_missval)))
          }
          table <- add_column(table, name, column)
        }
        else if (t == "int64-array") {
          for (elem in 0:(len - 1)) {
            column <- c()
            for (row in 0:(n_rows - 1)) {
              out <- sw_table$getInt64ArrayValueAsString(row, col, elem)
              .error_check(sw_table)
              if (identical(out, numeric(0))) {
                out <- 0
              }
              column <- c(column, .as.integer64(set_missing(out, int64_missval)))
            }
            table <- add_column(table, paste(name, elem + 1, sep = ""), column)
          }
        }
        else if (t == "double") {
          for (row in 0:(n_rows - 1)) {
            out <- sw_table$getDoubleValue(row, col)
            .error_check(sw_table)
            if (identical(out, numeric(0))) {
              out <- 0
            }
            column <- c(column, out)
          }
          table <- add_column(table, name, column)
        }
        else if (t == "double-array") {
          for (elem in 0:(len - 1)) {
            column <- c()
            for (row in 0:(n_rows - 1)) {
              out <- sw_table$getDoubleArrayValue(row, col, elem)
              .error_check(sw_table)
              if (identical(out, numeric(0))) {
                out <- 0
              }
              column <- c(column, out)
            }
            table <- add_column(table, paste(name, elem + 1, sep = ""), column)
          }
        }
        else if (t == "char") {
          for (row in 0:(n_rows - 1)) {
            strval <- sw_table$getStringValue(row, col)
            .error_check(sw_table)
            if (is.null(strval)) {
              strval <- ""
            }
            column <- c(column, strval)
          }
          table <- add_column(table, name, column)
        }
        else if (t == "varchar") {
          for (row in 0:(n_rows - 1)) {
            strval <- sw_table$getStringValue(row, col)
            .error_check(sw_table)
            if (is.null(strval)) {
              strval <- ""
            }
            column <- c(column, strval)
          }
          table <- add_column(table, name, column)
        }
        else if (t == "datetime") {
          column <- c()
          for (row in 0:(n_rows - 1)) {
            value <- set_missing(sw_table$getDatetimeValueAsString(row, col), int64_missval)
            .error_check(sw_table)
            if (is.na(value)) {
              column <- c(column, value)
            } else {
              column <- c(column, CASdt.as.POSIXct(value))
            }
          }
          table <- add_column(table, name, as.POSIXct(column, origin = "1970-01-01"))
        }
        else if (t == "date") {
          column <- c()
          for (row in 0:(n_rows - 1)) {
            value <- set_missing(sw_table$getDateValue(row, col), int32_missval)
            .error_check(sw_table)
            if (is.na(value)) {
              column <- c(column, value)
            } else {
              column <- c(column, CASd.as.Date(value))
            }
          }
          table <- add_column(table, name, as.Date(column, origin = "1970-01-01"))
        }
        else if (t == "time") {
          column <- c()
          for (row in 0:(n_rows - 1)) {
            value <- set_missing(sw_table$getTimeValueAsString(row, col), int64_missval)
            .error_check(sw_table)
            if (is.na(value)) {
              column <- c(column, value)
            } else {
              column <- c(column, CASt.as.POSIXct(value))
            }
          }
          table <- add_column(table, name, as.POSIXct(column, origin = "1970-01-01"))
        }
        else if (t == "binary") {
          for (row in 0:(n_rows - 1)) {
            strval <- sw_table$getBinaryBase64Value(row, col)
            .error_check(sw_table)
            if (is.null(strval)) {
              strval <- ""
            }
            column <- c(column, strval)
          }
          table <- add_column(table, name, column)
        }
        else if (t == "varbinary") {
          for (row in 0:(n_rows - 1)) {
            strval <- sw_table$getBinaryBase64Value(row, col)
            .error_check(sw_table)
            if (is.null(strval)) {
              strval <- ""
            }
            column <- c(column, strval)
          }
          table <- add_column(table, name, column)
        }
        else {
          for (row in 0:(n_rows - 1)) {
            column <- c(column, NULL)
          }
          table <- add_column(table, name, column)
        }
      }
    }

    output <- data.frame(table, check.names = FALSE)

    attributes(output)$table.name <- name
    attributes(output)$table.label <- label
    attributes(output)$table.title <- title
    attributes(output)$table.attrs <- attrs
    attributes(output)$col.labels <- col_labels
    attributes(output)$col.formats <- col_formats
    attributes(output)$col.attrs <- col_attrs
    attributes(output)$col.sizes <- col_sizes
    attributes(output)$col.types <- col_types
    attributes(output)$col.widths <- col_widths

    return(output)
  }
  else if (t == "list") {
    len <- sw_value$getListNItems()
    .error_check(sw_value)
    haskeys <- sw_value$hasKeys()
    .error_check(sw_value)
    output <- list()

    if (len < 1) {
      return(NULL)
    }

    if (haskeys == 0) {
      idx <- 1
      for (i in 1:len) {
        sw_item <- sw_value$getListItem(i - 1)
        .error_check(sw_value)
        val <- .cas2r(sw_item)

        if (!is.null(val)) {
          output[[idx]] <- val
          idx <- idx + 1
        }
      }
    }
    else {
      for (i in 0:(len - 1)) {
        sw_item <- sw_value$getListItem(i)
        .error_check(sw_value)
        key <- sw_item$getKey()
        .error_check(sw_item)
        val <- .cas2r(sw_item)
        if (!is.null(val)) {
          output[[key]] <- val
        }
      }
    }
    return(output)
  }
  else if (t == "blob") {
    out <- sw_value$getBlobBase64()
    .error_check(sw_value)
    return(jsonlite::base64_dec(as.character(out)))
  }
  else {
    return(paste("Invalid type: ", t))
  }
}

#' Set a key / value pair in a CASValueList
#'
#' @param sw_values a \code{CASValueList} object.
#' @param i The index where the key / value pair should be inserted.
#' @param key String containing the key. This can be an empty string
#'   for indexed lists.
#' @param value The value to set.
#'
#' @return The next list index to be populated.
#'
#' @noRd
.set_list_value <- function(sw_values, i, key, value) {
  if (key == "datamsghandler") {
    return(i)
  }
  t <- class(value)
  if (t == "raw")
  {
    rc <- sw_values$setBlobFromBase64(i, key, gsub("\\s+", "", perl=TRUE,
                                                   jsonlite::base64_enc(value)))
    .error_check(sw_values)
    return (i + 1)
  }
  else if (t == "CASTable") {
    return(.set_list_value(sw_values, i, key, .gen_table_param(value)))
  }
  else if (t == "list" || length(value) > 1) {
    sw_sublist <- sw_values$createListAt(i, key, length(value))
    .error_check(sw_values)
    for (j in seq_len(length(value))) {
      if (is.null(names(value[j])) || nchar(names(value[j])) == 0) {
        .set_list_value(sw_sublist, j - 1, "", value[[j]])
      }
      else {
        .set_list_value(sw_sublist, j - 1, names(value[j]), value[[j]])
      }
    }
    return(i + 1)
  }
  else if (t == "logical") {
    sw_values$setBoolean(i, key, value[[1]])
    .error_check(sw_values)
    return(i + 1)
  }
  else if (t == "integer") {
    sw_values$setInt64(i, key, value[[1]])
    .error_check(sw_values)
    return(i + 1)
  }
  else if (t == "numeric") {
    sw_values$setDouble(i, key, value[[1]])
    .error_check(sw_values)
    return(i + 1)
  }
  else if (t == "character" || t == "factor") {
    sw_values$setString(i, key, value[[1]])
    .error_check(sw_values)
    return(i + 1)
  }
  else {
    sw_values$setNil(i, key)
    .error_check(sw_values)
    return(i + 1)
  }
  return(i)
}

#' Convert an R object to a CASValueList
#'
#' @param soptions An soptions string, primarily used for locale information.
#' @param sw_error A \code{CASError} object for storing resulting error messages.
#' @param a Arbitrary R object to convert.
#'
#' @noRd
.r2cas <- function(soptions, sw_error, a) {
  len <- length(a)
  if (!is.null(a$datamsghandler)) {
    len <- len - 1
  }
  sw_values <- SW_CASValueList(len, soptions, sw_error)
  if (len > 0) {
    i <- 0
    for (j in seq_len(length(a))) {
      if (is.null(names(a[j])) || nchar(names(a[j])) == 0) {
        i <- .set_list_value(sw_values, i, "", a[[j]])
      }
      else {
        i <- .set_list_value(sw_values, i, names(a[j]), a[[j]])
      }
    }
  }
  return(sw_values)
}

#' Convert a CAS value list to an R list
#'
#' @param sw_values A \code{CASValueList} object.
#' @param len The length of the CAS value list
#'
#' @noRd
.casvaluelist2r <- function(sw_values, len) {
  num <- 1
  output <- list()
  for (i in 0:(len - 1)) {
    sw_item <- sw_values$getItem(i)
    .error_check(sw_values)
    key <- sw_item$getKey()
    .error_check(sw_item)
    if (is.null(key) || nchar(key) == 0) {
      output[[num]] <- .cas2r(sw_item)
      num <- num + 1
    }
    else {
      output[[key]] <- .cas2r(sw_item)
    }
  }
  return(output)
}

setGeneric("cas.close",
  function(x, close.session = FALSE) {
    standardGeneric("cas.close")
  }
)

#' Close a CAS connection while leaving the session alive
#'
#' @param x The \code{\link{CAS}} connection object
#' @param close.session Should the session be terminated in addition to 
#'                      closing the connection?
#'
#' @seealso \code{\link{cas.terminate}}
#'
#' @export
setMethod(
  "cas.close",
  signature(x = "CAS"),
  function(x, close.session = FALSE) {
    if (close.session) {
      x$retrieve("session.endsession", `_messagelevel` = "error")
    }
    return(invisible(x$close()))
  }
)

#' End a CAS session and close the connection
#'
#' @param conn The CAS connection object
#'
#' @seealso \code{\link{cas.close}}
#'
#' @export
cas.terminate.CAS <- function(conn) {
  return(invisible(cas.close(conn, close.session = TRUE)))
}

setGeneric("cas.upload",
  function(.x, .object, ...) {
    standardGeneric("cas.upload")
  }
)

#' Upload a data.frame or file to a CAS table
#'
#' @param .x The \code{\link{CAS}} connection object
#' @param .object The data.frame, filename, or URL to upload.
#' @param \dots Optional parameters that are passed to the table.loadtable action.
#'
#' @return List containing fields \code{results}, \code{messages}, \code{disposition},
#'   and \code{performance} which contain the results of the CAS action call
#'   to upload the data to a table.
#'
#' @seealso \code{\link{cas.upload.frame}}, \code{\link{cas.upload.file}}
#'
#' @export
setMethod(
  "cas.upload",
  signature(.x = "CAS"),
  function(.x, .object, ...) {
    return(.x$upload(.object, ...))
  }
)

setGeneric("cas.upload.frame",
  function(.x, .frame, ...) {
    standardGeneric("cas.upload.frame")
  }
)

#' Upload a data.frame to a CAS table
#'
#' @param .x The \code{\link{CAS}} connection object
#' @param .frame The \code{data.frame} to upload
#' @param \dots Optional parameters that are passed to the table.loadtable action.
#'
#' @return \code{\link{CASTable}} object which references a new CAS table
#'   on the server that contains the data from the given \code{data.frame}.
#'
#' @seealso \code{\link{cas.upload.file}}
#'
#' @export
setMethod(
  "cas.upload.frame",
  signature(.x = "CAS"),
  function(.x, .frame, ...) {
    res <- .x$upload(.frame, ...)
    return(CASTable(.x, res$results$tableName, caslib = res$results$caslib))
  }
)

setGeneric("cas.upload.file",
  function(.x, .file, ...) {
    standardGeneric("cas.upload.file")
  }
)

#' Upload a data file to a CAS table
#'
#' @param x The \code{\link{CAS}} connection object
#' @param file The filename to upload
#' @param \dots Optional parameters that are passed to the table.loadtable action.
#'
#' @return \code{\link{CASTable}} object which references a new CAS table
#'   on the server that contains the data from the given \code{data.frame}.
#'
#' @seealso \code{\link{cas.upload.frame}}
#'
#' @export
setMethod(
  "cas.upload.file",
  signature(.x = "CAS"),
  function(.x, .file, ...) {
    res <- .x$upload(.file, ...)
    return(CASTable(.x, res$results$tableName, caslib = res$results$caslib))
  }
)

setGeneric("cas.help",
  function(.x, .action) {
    standardGeneric("cas.help")
  }
)

#' Display help for a given action
#'
#' @param .x \code{\link{CAS}} connection object
#' @param .action The action name to display help for.
#'
#' @export
setMethod(
  "cas.help",
  signature(.x = "CAS"),
  function(.x, .action) {
    invisible(sapply(cas.retrieve(.x, "builtins.help", action=.action)$messages,
                     function(y) { cat(gsub("^.*?: ", '', y, perl=TRUE)); cat("\n") }))
  }
)

setGeneric("cas.performance",
  function(.x, .action) {
    standardGeneric("cas.performance")
  }
)

#' Return performance metrics for last CAS action
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return list
#'
#' @export
setMethod(
  "cas.performance",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$performance)
  }
)

#' Return performance metrics for last CAS action
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return list
#'
#' @export
setMethod(
  "cas.performance",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$performance)
  }
)

#' Return performance metrics from result set
#'
#' @param .x list
#'
#' @return list
#'
#' @export
setMethod(
  "cas.performance",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$performance)) {
      return(.x$performance)
    }
    return(list())
  }
)

setGeneric("cas.severity",
  function(.x) {
    standardGeneric("cas.severity")
  }
)

#' Return resulting severity of last CAS action
#'
#' Possible values are 0 for no errors or warnings,
#' 1 for warnings, and 2 for errors.
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return integer
#'
#' @export
setMethod(
  "cas.severity",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$severity)
  }
)

#' Return resulting severity of last CAS action
#'
#' Possible values are 0 for no errors or warnings,
#' 1 for warnings, and 2 for errors.
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return integer
#'
#' @export
setMethod(
  "cas.severity",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$severity)
  }
)

#' Return resulting severity from result set
#'
#' Possible values are 0 for no errors or warnings,
#' 1 for warnings, and 2 for errors.
#'
#' @param .x list
#'
#' @return integer
#'
#' @export
setMethod(
  "cas.severity",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$disposition) && !is.null(.x$disposition$severity)) {
      return(.x$disposition$severity)
    }
    return(0)
  }
)

setGeneric("cas.reason",
  function(.x) {
    standardGeneric("cas.reason")
  }
)

#' Return resulting reason for failure of last CAS action
#'
#' The reason is a general class of error that occurred.
#' Use `cas.status` for the full error message.
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.reason",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$reason)
  }
)

#' Return resulting reason for failure of last CAS action
#'
#' The reason is a general class of error that occurred.
#' Use `cas.status` for the full error message.
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.reason",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$reason)
  }
)

#' Return resulting reason for failure from result set
#'
#' The reason is a general class of error that occurred.
#' Use `cas.status` for the full error message.
#'
#' @param .x list
#'
#' @return character
#'
#' @export
setMethod(
  "cas.reason",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$disposition) && !is.null(.x$disposition$reason)) {
      return(.x$disposition$reason)
    }
    return(character())
  }
)

setGeneric("cas.status",
  function(.x) {
    standardGeneric("cas.status")
  }
)

#' Return resulting status message for failure of last CAS action
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.status",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$status)
  }
)

#' Return resulting status message for failure of last CAS action
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.status",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$status)
  }
)

#' Return resulting status message from result set
#'
#' @param .x list
#'
#' @return character
#'
#' @export
setMethod(
  "cas.status",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$disposition) && !is.null(.x$disposition$status)) {
      return(.x$disposition$status)
    }
    return(character())
  }
)

setGeneric("cas.status_code",
  function(.x) {
    standardGeneric("cas.status_code")
  }
)

#' Return resulting status code for failure of last CAS action
#'
#' This status code can be helpful in tech support issues to
#' determine the underlying problem.
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return numeric
#'
#' @export
setMethod(
  "cas.status_code",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$statusCode)
  }
)

#' Return resulting status code for failure of last CAS action
#'
#' This status code can be helpful in tech support issues to
#' determine the underlying problem.
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return numeric
#'
#' @export
setMethod(
  "cas.status_code",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$statusCode)
  }
)

#' Return resulting status code from result set
#'
#' This status code can be helpful in tech support issues to
#' determine the underlying problem.
#'
#' @param .x list
#'
#' @return numeric
#'
#' @export
setMethod(
  "cas.status_code",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$disposition) && !is.null(.x$disposition$statusCode)) {
      return(.x$disposition$statusCode)
    }
    return(0)
  }
)

setGeneric("cas.debug",
  function(.x) {
    standardGeneric("cas.debug")
  }
)

#' Return resulting debug messages of last CAS action
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.debug",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$debug)
  }
)

#' Return resulting debug messages of last CAS action
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.debug",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$debug)
  }
)

#' Return resulting debug messages from result set
#'
#' @param .x list
#'
#' @return character
#'
#' @export
setMethod(
  "cas.debug",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$disposition) && !is.null(.x$disposition$debug)) {
      return(.x$disposition$debug)
    }
    return(character())
  }
)

setGeneric("cas.messages",
  function(.x) {
    standardGeneric("cas.messages")
  }
)

#' Return messages from the last CAS action
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.messages",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$messages)
  }
)

#' Return messages from the last CAS action
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.messages",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$messages)
  }
)

#' Return messages from result set
#'
#' @param .x list
#'
#' @return character
#'
#' @export
setMethod(
  "cas.messages",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$messages)) {
      return(.x$messages)
    }
    return(character())
  }
)

setGeneric("cas.events",
  function(.x) {
    standardGeneric("cas.events")
  }
)

#' Return events from the last CAS action
#'
#' Events are things such as caslibs or tables being added,
#' updated, or deleted.
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return list
#'
#' @export
setMethod(
  "cas.events",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$events)
  }
)

#' Return events from the last CAS action
#'
#' Events are things such as caslibs or tables being added,
#' updated, or deleted.
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return list
#'
#' @export
setMethod(
  "cas.events",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$events)
  }
)

#' Return events from result set
#'
#' Events are things such as caslibs or tables being added,
#' updated, or deleted.
#'
#' @param .x list
#'
#' @return list
#'
#' @export
setMethod(
  "cas.events",
  signature(.x = "list"),
  function(.x) {
    if (!is.null(.x$events)) {
      return(.x$events)
    }
    return(list())
  }
)

setGeneric("cas.session",
  function(.x) {
    standardGeneric("cas.session")
  }
)

#' Return the session ID of the CAS connection
#'
#' @param .x \code{\link{CAS}} connection object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.session",
  signature(.x = "CAS"),
  function(.x) {
    return(.x$session)
  }
)

#' Return the session ID of the CAS connection
#'
#' @param .x \code{\link{CASTable}} object
#'
#' @return character
#'
#' @export
setMethod(
  "cas.session",
  signature(.x = "CASTable"),
  function(.x) {
    return(.x@conn$session)
  }
)
