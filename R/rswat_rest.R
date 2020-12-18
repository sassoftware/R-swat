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


#' Print tracing information for action call
#'
#' @keywords internal
#'
.trace_actions <- function(action_name, params) {
  ui <- FALSE
  if (!is.null(params$`_apptag`)) {
    ui <- params$`_apptag` == "UI"
  }

  if (!as.logical(getOption("cas.trace.actions"))) {
    return(FALSE)
  }

  if (ui && !as.logical(getOption("cas.trace.ui.actions"))) {
    return(FALSE)
  }

  message(paste("[", action_name, "]", sep = ""))
  .trace_list(params)
  message("")

  return(TRUE)
}

#' Paste strings together using sep as needed
#'
#' @keywords internal
#'
.paste_prefix <- function(str1, str2, sep = ".") {
  if (is.null(str1)) {
    return(str2)
  }
  return(paste(str1, str2, sep = sep))
}

#' Print tracing information for list parameter
#'
#' @keywords internal
#'
.trace_list <- function(params, prefix = NULL) {
  for (name in names(params)) {
    value <- params[[name]]
    if (class(value) == "list") {
      .trace_list(value, prefix = .paste_prefix(prefix, name))
    }
    else if ( class(value) == 'raw' ) {
       message(paste('   ', .paste_prefix(prefix, name), ' = binary-object (blob)', sep=''))
    }
    else if ( length(value) > 1 ) {
      for ( i in 1:length(value) ) {
        if ( class(value[i]) == 'list' || length(value[i]) > 1 ) {
          .trace_list( value[i], prefix=.paste_prefix(prefix, name) )
        } else {
          message(paste('   ', .paste_prefix(prefix, name), '[', i, '] = ', 
                        value[[i]], ' (', class(value), ')', sep=''))  
        }
      }
    } else {
      message(paste("   ", .paste_prefix(prefix, name), " = ",
        value, " (", class(value), ")",
        sep = ""
      ))
    }
  }
}

#' CAS error class
#'
#' @keywords internal
#'
REST_CASError <- setRefClass(
  Class = "REST_CASError",

  fields = list(
    soptions_ = "character",
    message_ = "character"
  ),

  methods = list(
    initialize = function(soptions) {
      soptions_ <<- soptions
      message_ <<- ""
    },

    getTypeName = function() {
      return("error")
    },

    getSOptions = function() {
      return(soptions_)
    },

    isNULL = function() {
      return(FALSE)
    },

    getLastErrorMessage = function() {
      return(message_)
    },

    setErrorMessage = function(msg) {
      message_ <<- msg
    }
  )
)

#' CAS results table class
#'
#' @keywords internal
#'
REST_CASTable <- setRefClass(
  Class = "REST_CASTable",

  fields = list(
    obj_ = "list",
    attrs_ = "list",
    colattrs_ = "list"
  ),

  methods = list(
    initialize = function(obj) {
      obj_ <<- obj

      attrs_ <<- list()
      for (name in names(obj_$attributes)) {
        attrs_[[name]] <<- .self$attr2R(obj_$attributes[[name]])
      }

      colattrs_ <<- list()
      for (i in seq_len(length(obj_$schema))) {
        attrs <- obj_$schema[[i]]$attributes
        newattrs <- list()
        for (name in names(attrs)) {
          newattrs[[name]] <- .self$attr2R(attrs[[name]])
        }
        colattrs_ <<- c(colattrs_, newattrs)
      }
    },

    getAttributes = function() {
      return(attrs_)
    },

    getColumnAttributes = function() {
      return(colattrs_)
    },

    attr2R = function(attr) {
      atype <- attr$type
      value <- attr$value
      if (atype == "double" || atype == "float") {
        if (is.null(value)) {
          return(NULL)
        }
        return(as.numeric(value))
      }
      if (atype == "int32") {
        return(as.integer(value))
      }
      if (atype == "int64") {
        return(as.character(value))
      }
      if (atype == "date") {
        return(cas.date2posix(value))
      }
      if (atype == "time") {
        return(cas.datetime2posix(value))
      }
      if (atype == "datetime") {
        return(cas.datetime2posix(value))
      }
      return(value)
    },

    getTypeName = function() {
      return("table")
    },

    getSOptions = function() {
      return("")
    },

    isNULL = function() {
      return(FALSE)
    },

    getName = function() {
      return(obj_$name)
    },

    getLabel = function() {
      return(obj_$label)
    },

    getTitle = function() {
      return(obj_$title)
    },

    getNColumns = function() {
      return(length(obj_$schema))
    },

    getNRows = function() {
      return(length(obj_$rows))
    },

    getColumnName = function(i) {
      return(obj_$schema[[i + 1]]$name)
    },

    getColumnLabel = function(i) {
      return(obj_$schema[[i + 1]]$label)
    },

    mapColumnType = function(type) {
      if (type == "string") {
        return("varchar")
      }
      if (type == "binary") {
        return("varbinary")
      }
      return(type)
    },

    getColumnType = function(i) {
      ctype <- .self$mapColumnType(obj_$schema[[i + 1]]$type)
      if (ctype == "int") {
        if (.self$getColumnWidth(i) == 4) {
          ctype <- "int32"
        } else {
          ctype <- "int64"
        }
      }
      if (ctype == "double" && .self$getNColumns() > 0 && .self$getNRows() > 0 &&
        length(obj_$rows[[1]][[i + 1]]) > 1) {
        return("double-array")
      }
      else if (ctype == "int64" && .self$getNColumns() > 0 && .self$getNRows() > 0 &&
        length(obj_$rows[[1]][[i + 1]]) > 1) {
        return("int64-array")
      }
      else if (ctype == "int32" && .self$getNColumns() > 0 && .self$getNRows() > 0 &&
        length(obj_$rows[[1]][[i + 1]]) > 1) {
        return("int64-array")
      }
      return(ctype)
    },

    getColumnWidth = function(i) {
      return(obj_$schema[[i + 1]]$width)
    },

    getColumnFormat = function(i) {
      return(obj_$schema[[i + 1]]$format)
    },

    getColumnArrayNItems = function(i) {
      ctype <- .self$getColumnType(i)
      if (ctype == "double-array") {
        return(length(obj_$rows[[1]][[i + 1]]))
      }
      if (ctype == "int64-array") {
        return(length(obj_$rows[[1]][[i + 1]]))
      }
      if (ctype == "int32-array") {
        return(length(obj_$rows[[1]][[i + 1]]))
      }
      return(1)
    },

    getStringValue = function(row, col) {
      return(as.character(obj_$rows[[row + 1]][[col + 1]]))
    },

    getBinaryValue = function(row, col) {
      return(jsonlite::base64_dec(.self$getBinaryBase64Value(row, col)))
    },

    getBinaryNBytes = function(row, col) {
      return(obj_$rows[[row + 1]][[col + 1]]$length)
    },

    getBinaryBase64Value = function(row, col) {
      return(as.character(obj_$rows[[row + 1]][[col + 1]]$data))
    },

    getDateValue = function(row, col) {
      return(obj_$rows[[row + 1]][[col + 1]])
    },

    getTimeValue = function(row, col) {
      return(obj_$rows[[row + 1]][[col + 1]])
    },

    getTimeValueAsString = function(row, col) {
      return(as.character(obj_$rows[[row + 1]][[col + 1]]))
    },

    getDatetimeValue = function(row, col) {
      return(obj_$rows[[row + 1]][[col + 1]])
    },

    getDatetimeValueAsString = function(row, col) {
      return(as.character(obj_$rows[[row + 1]][[col + 1]]))
    },

    getInt64Value = function(row, col) {
      return(swat.as.integer64(obj_$rows[[row + 1]][[col + 1]]))
    },

    getInt64ValueAsString = function(row, col) {
      return(as.character(obj_$rows[[row + 1]][[col + 1]]))
    },

    getInt64ArrayValue = function(row, col, elem) {
      return(swat.as.integer64(obj_$rows[[row + 1]][[col + 1]][[elem + 1]]))
    },

    getInt64ArrayValueAsString = function(row, col, elem) {
      return(as.character(obj_$rows[[row + 1]][[col + 1]][[elem + 1]]))
    },

    getInt32Value = function(row, col) {
      return(as.integer(obj_$rows[[row + 1]][[col + 1]]))
    },

    getInt32ArrayValue = function(row, col, elem) {
      return(as.integer(obj_$rows[[row + 1]][[col + 1]][[elem + 1]]))
    },

    getDoubleValue = function(row, col) {
      val <- obj_$rows[[row + 1]][[col + 1]]
      if (is.null(val)) {
        return(NaN)
      }
      return(as.numeric(val))
    },

    getDoubleArrayValue = function(row, col, elem) {
      val <- obj_$rows[[row + 1]][[col + 1]][[elem + 1]]
      if (is.null(val)) {
        return(NaN)
      }
      return(as.numeric(val))
    },

    getLastErrorMessage = function() {
      return("")
    }
  )
)

#' CAS value class
#'
#' @keywords internal
#'
REST_CASValue <- setRefClass(
  Class = "REST_CASValue",

  fields = list(
    key_ = "ANY",
    value_ = "ANY",
    items_ = "list"
  ),

  methods = list(
    initialize = function(key, value) {
      key_ <<- key
      value_ <<- value

      items_ <<- list()
      if (.self$getType() == "list") {
        if (length(names(value_))) {
          for (name in names(value_)) {
            items_[[length(items_) + 1]] <<- REST_CASValue(name, value_[[name]])
          }
        } else if (length(value_)) {
          for (i in seq_len(length(value_))) {
            items_[[length(items_) + 1]] <<- REST_CASValue(NULL, value_[[i]])
          }
        }
      }
    },

    hasKeys = function() {
      if (.self$getType() == "list" && length(names(value_))) {
        return(TRUE)
      }
      return(FALSE)
    },

    getTypeName = function() {
      return("value")
    },

    getSOptions = function() {
      return("")
    },

    isNULL = function() {
      if (is.null(key_) && is.null(value_)) {
        return(TRUE)
      }
      return(FALSE)
    },

    getType = function() {
      if (length(names(value_))) {
        if (!is.atomic(value_) && !is.null(value_$`_ctb`) && value_$`_ctb`) {
          return("table")
        }
        if (!is.atomic(value_) && !is.null(value_$`_blob`) && value_$`_blob`) {
          return("blob")
        }
        return("list")
      }
      if (class(value_) == "list" || length(value_) > 1) {
        return("list")
      }
      if (class(value_) == "numeric") {
        return("double")
      }
      if (class(value_) == "integer") {
        return("int64")
      }
      if (class(value_) == "character") {
        return("string")
      }
      if (class(value_) == "logical") {
        return("boolean")
      }
      if (is.null(value_)) {
        return("nil")
      }
      stop(paste("Could not determine type of result", key_, ":", value_, paste = " "))
    },

    getKey = function() {
      return(key_)
    },

    getInt32 = function() {
      return(as.integer(value_))
    },

    getDate = function() {
      return(as.integer(value_))
    },

    getInt64 = function() {
      return(swat.as.integer64(value_))
    },

    getInt64AsString = function() {
      return(as.character(value_))
    },

    getTime = function() {
      return(swat.as.integer64(value_))
    },

    getTimeAsString = function() {
      return(as.character(value_))
    },

    getDatetime = function() {
      return(swat.as.integer64(value_))
    },

    getDatetimeAsString = function() {
      return(as.character(value_))
    },

    getDouble = function() {
      if (is.null(value_)) {
        return(NaN)
      }
      return(as.numeric(value_))
    },

    getString = function() {
      return(as.character(value_))
    },

    getBlob = function() {
      return(jsonlite::base64_dec(as.character(value_$data)))
    },

    getBlobBase64 = function() {
      return(as.character(value_))
    },

    getBoolean = function() {
      return(as.logical(value_))
    },

    getList = function() {
      return(value_)
    },

    getListNItems = function() {
      return(length(items_))
    },

    getListItem = function(i) {
      return(items_[[i + 1]])
    },

    getTable = function() {
      return(REST_CASTable(value_))
    },

    getLastErrorMessage = function() {
      return("")
    }
  )
)

#' Convert camel-case name to underscore-delimited
#'
#' @keywords internal
#'
.camel2underscore <- function(str) {
  return(tolower(gsub("^_([A-Z])", "\\1", gsub("([A-Z])", "_\\1", str, perl = TRUE), perl = TRUE)))
}

#' Map severity name to integer valuu
#'
#' @keywords internal
#'
.mapseverity <- function(sev) {
  if (is.null(sev)) {
    return(0)
  }
  if (sev == "Error") {
    return(2)
  }
  if (sev == "Warning") {
    return(1)
  }
  return(0)
}

#' Map reason string to normalized case
#'
#' @keywords internal
#'
.mapreason <- function(reason) {
  if (is.null(reason) || reason == "ok") {
    return("")
  }
  return(tolower(reason))
}

#' Response from the CAS server
#'
#' @keywords internal
#'
REST_CASResponse <- setRefClass(
  Class = "REST_CASResponse",

  fields = list(
    obj_ = "list",
    disposition_ = "list",
    update_flags_ = "list",
    messages_ = "list",
    metrics_ = "list",
    results_ = "list",
    result_idx_ = "numeric",
    message_idx_ = "numeric",
    update_flag_idx_ = "numeric"
  ),

  methods = list(
    initialize = function(obj) {
      obj_ <<- obj

      disp <- obj_$disposition
      disposition_ <<- list(
        debug = disp$debugInfo,
        status = disp$formattedStatus,
        reason = .mapreason(disp$reason),
        severity = .mapseverity(disp$severity),
        statusCode = disp$statusCode
      )

      update_flags_ <<- list()
      if (length(obj_$changedResources)) {
        for (i in seq_len(length(obj_$changedResources))) {
          update_flags_[[i]] <<- .camel2underscore(obj_$changedResources[[i]])
        }
      }

      messages_ <<- list()
      if (length(obj_$logEntries)) {
        for (i in seq_len(length(obj_$logEntries))) {
          messages_[[i]] <<- obj_$logEntries[[i]]$message
        }
      }

      results_ <<- list()
      if (length(names(obj_$results))) {
        for (name in names(obj_$results)) {
          results_[[length(results_) + 1]] <<- REST_CASValue(name, obj_$results[[name]])
        }
      } else if (length(obj_$results)) {
        for (i in seq_len(length(obj_$results))) {
          results_[[length(results_) + 1]] <<- REST_CASValue(i, obj_$results[[i]])
        }
      }

      if (!is.null(obj_$metrics)) {
        metrics_ <<- obj_$metrics
      } else {
        metrics_ <<- list()
      }

      result_idx_ <<- 1
      message_idx_ <<- 1
      update_flag_idx_ <<- 1
    },

    getNextMessage = function() {
      if (message_idx_ <= length(messages_)) {
        message_idx_ <<- message_idx_ + 1
        return(messages_[[message_idx_ - 1]])
      }
      return(NULL)
    },

    getNextUpdateFlag = function() {
      if (update_flag_idx_ <= length(update_flags_)) {
        update_flag_idx_ <<- update_flag_idx_ + 1
        return(update_flags_[[update_flag_idx_ - 1]])
      }
      return(NULL)
    },

    getNextResult = function() {
      if (result_idx_ <= length(results_)) {
        result_idx_ <<- result_idx_ + 1
        return(results_[[result_idx_ - 1]])
      }
      return(NULL)
    },

    getTypeName = function() {
      return("response")
    },

    getSOptions = function() {
      return("")
    },

    isNULL = function() {
      return(FALSE)
    },

    getNMessages = function() {
      return(length(messages_))
    },

    getNUpdateFlags = function() {
      return(length(update_flags_))
    },

    getNResults = function() {
      return(length(results_))
    },

    getDispositionSeverity = function() {
      return(disposition_$severity)
    },

    getDispositionStatusCode = function() {
      return(disposition_$statusCode)
    },

    getDispositionStatus = function() {
      return(disposition_$status)
    },

    getDispositionReason = function() {
      return(disposition_$reason)
    },

    getDispositionDebug = function() {
      return(disposition_$debug)
    },

    getPerformanceNExtended = function() {
      if ("extend" %in% names(metrics_)) {
        return(length(metrics_$extend))
      }
      return(0)
    },

    getPerformanceExtended = function() {
      if ("extend" %in% names(metrics_)) {
        return(metrics_$extend)
      }
      return(list())
    },

    getElapsedTime = function() {
      return(metrics_$elapsed_time)
    },

    getDataMovementTime = function() {
      return(metrics_$dataMovementTime)
    },

    getDataMovementBytes = function() {
      return(metrics_$dataMovementBytes)
    },

    getCPUUserTime = function() {
      return(metrics_$cpuUserTime)
    },

    getCPUSystemTime = function() {
      return(metrics_$cpuSystemTime)
    },

    getSystemTotalMemory = function() {
      return(metrics_$systemTotalMemory)
    },

    getSystemNodes = function() {
      return(metrics_$systemNodes)
    },

    getSystemCores = function() {
      return(metrics_$systemCores)
    },

    getMemory = function() {
      return(metrics_$memory)
    },

    getMemoryOS = function() {
      return(metrics_$memoryOS)
    },

    getMemorySystem = function() {
      return(metrics_$memorySystem)
    },

    getMemoryQuota = function() {
      return(metrics_$memoryQuota)
    },

    getLastErrorMessage = function() {
      return("")
    }
  )
)

#' Message object from CAS server
#'
#' @keywords internal
#'
REST_CASMessage <- setRefClass(
  Class = "REST_CASMessage",

  fields = list(
    obj_ = "ANY",
    connection_ = "ANY"
  ),

  methods = list(
    initialize = function(obj, connection) {
      obj_ <<- obj
      connection_ <<- connection
    },

    getTypeName = function() {
      return("message")
    },

    getSOptions = function() {
      return("")
    },

    isNULL = function() {
      if (is.null(obj_)) {
        return(TRUE)
      }
      return(FALSE)
    },

    getTag = function() {
      return("")
    },

    getType = function() {
      return("response")
    },

    getFlags = function() {
      return(list())
    },

    toResponse = function(connection) {
      return(REST_CASResponse(obj_))
    },

    toRequest = function() {
      stop("Not supported in the REST interface")
    },

    getLastErrorMessage = function() {
      return("")
    }
  )
)

#' Convert CASTable objects to list parameters
#'
#' @keywords internal
#'
.expand_params <- function(params) {
  out <- list()
  for (name in names(params)) {
    cls <- class(params[[name]])
    if (cls == "CASTable") {
      out[[name]] <- .gen_table_param(params[[name]])
    }
    else if (cls == "list" && length(names(params[[name]]))) {
      out[[name]] <- .expand_params(params[[name]])
    }
    else if (cls == 'raw')
    {
       out[[name]] <- list(`_blob`=TRUE,
                           data=gsub("\\s+", "", perl=TRUE, jsonlite::base64_enc(params[[name]])),
                           length=length(params[[name]]))
    }
    else {
      out[[name]] <- params[[name]]
    }
  }
  return(out)
}

#' CAS connection object
#'
#' @keywords internal
#'
REST_CASConnection <- setRefClass(
  Class = "REST_CASConnection",

  fields = list(
    hostname_ = "character",
    port_ = "numeric",
    username_ = "character",
    password_ = "character",
    error_ = "ANY",
    results_ = "ANY",
    soptions_ = "character",
    baseurl_ = "character",
    auth_ = "ANY",
    session_ = "character",
    orig_hostname_ = "character",
    orig_port_ = "numeric",
    current_baseurl_ = "character",
    current_hostname_ = "character",
    current_port_ = "numeric",
    host_index_ = "numeric",
    config_ = "ANY",
    tkhttp_id_ = "character"
  ),

  methods = list(
    initialize = function(hostname, port, username, password, soptions, error) {
      username_ <<- username
      error_ <<- error
      soptions_ <<- soptions
      results_ <<- NULL
      locale <- NULL
      session <- NULL
      orig_hostname_ <<- hostname
      orig_port_ <<- port
      tkhttp_id_ <<- ""

      if (!is.na(Sys.getenv("CAS_CLIENT_SSL_CA_LIST", unset = NA))) {
        config_ <<- httr::config(cainfo = Sys.getenv("CAS_CLIENT_SSL_CA_LIST"))
      }
      else if (!is.na(Sys.getenv("SAS_TRUSTED_CA_CERTIFICATES_PEM_FILE", unset = NA))) {
        config_ <<- httr::config(cainfo = Sys.getenv("SAS_TRUSTED_CA_CERTIFICATES_PEM_FILE"))
      }
      else if (!is.na(Sys.getenv("SSLCALISTLOC", unset = NA))) {
        config_ <<- httr::config(cainfo = Sys.getenv("SSLCALISTLOC"))
      }
      else {
        config_ <<- httr::config()
      }

      if (is.null(password)) {
        password <- ""
      }

      if (!grepl("^https?:", hostname[[1]], perl = TRUE)) {
        if (grepl("protocol=https", soptions)) {
          hostname <- paste("https://", hostname, sep = "")
        } else {
          hostname <- paste("http://", hostname, sep = "")
        }
      }

      if (grepl("^https?:", hostname[[1]], perl = TRUE)) {
        is_https <- FALSE
        if (grepl("^https", hostname[[1]], perl = TRUE)) {
          is_https <- TRUE
        }
        url <- httr::parse_url(hostname[[1]])
        baseurl_ <<- character()
        hostname_ <<- character()
        port_ <<- numeric()
        for (i in seq_len(length(hostname))) {
          url <- httr::parse_url(hostname[[i]])
          hostname_ <<- c(.self$hostname_, url$hostname)
          port_ <<- c(.self$port_, as.numeric(url$port))
          url$port <- .self$port_
          baseurl_ <<- c(.self$baseurl_, sub("/$", "", httr::build_url(url), perl = TRUE)[[1]])
        }
      }

      if (grepl("\\blocale=\\w+", soptions, perl = TRUE)) {
        m <- regexpr("\\blocale=(\\w+)", soptions_, perl = TRUE)
        locale <- gsub("^locale=", "", regmatches(soptions_, m)[[1]], perl = TRUE)[[1]]
      }

      if (grepl("\\bsession=[\\w\\-]+", soptions, perl = TRUE)) {
        m <- regexpr("\\bsession=([\\w\\-]+)", soptions_, perl = TRUE)
        session <- gsub("^session=", "", regmatches(soptions_, m)[[1]], perl = TRUE)[[1]]
      }

      host_index_ <<- 0
      .self$set_next_connection_(NULL)

      authinfo <- NULL
      if (grepl("^authinfo={", password, perl = TRUE)) {
        authinfo <- substr(password, 11, nchar(password) - 1)
        authinfo <- strsplit(authinfo, "\\}\\{", perl = TRUE)[[1]]
        authinfo <- gsub("^{", "", authinfo, perl = TRUE)
        authinfo <- gsub("}$", "", authinfo, perl = TRUE)
        authinfo <- query_authinfo(
          host = current_hostname_, user = username,
          protocol = current_port_, filepath = authinfo
        )
      }
      else if (password == "") {
        authinfo <- query_authinfo(current_hostname_, username = username, protocol = current_port_)
      }

      if (is.null(authinfo)) {
        if (is.null(username) || username == "") {
          stop("No username was specified.")
        }
        if (is.null(password) || password == "") {
          stop("No password was specified.")
        }
        auth_ <<- httr::authenticate(username, password)
        password_ <<- jsonlite::base64_enc(password)
      }
      else {
        if (is.null(authinfo$password) || authinfo$password == "") {
          stop("No password was specified.")
        }
        password_ <<- jsonlite::base64_enc(authinfo$password)
        if (is.null(authinfo$username)) {
          if (is.null(username) || username == "") {
            stop("No username was specified.")
          }
          auth_ <<- httr::authenticate(username, authinfo$password)
        } else {
          auth_ <<- httr::authenticate(authinfo$username, authinfo$password)
          username_ <<- authinfo$username
        }
      }

      while (TRUE) {
        tryCatch({
          if (is.null(session)) {
            url <- paste(current_baseurl_, "cas", "sessions", sep = "/")
            httr::handle_reset(url)
            res <- httr::PUT(url, auth_, config_)
            out <- httr::content(res, as = "parsed", type = "application/json", encoding = "utf-8")

            cookies <- httr::cookies(res)
            tkhttp_id_ <<- as.character(cookies[cookies$name == "tkhttp-id", ]$value)

            if (is.null(out$session)) {
              if (is.null(out$details)) {
                stop(paste(url, ":", out$error))
              } else {
                stop(paste(url, ":", out$error, "(", out$details, ")"))
              }
            }

            session_ <<- out$session

            if (!is.null(locale)) {
              .self$invoke("session.setlocale", list(locale = locale))
              if ("disposition" %in% names(results_)) {
                if ("severity" %in% names(results_[["disposition"]]) &&
                  results_[["disposition"]][["severity"]] == "Error") {
                  stop(results_[["disposition"]][["formattedStatus"]])
                }
              } else {
                stop("Unknown error when setting locale")
              }

              results_ <<- NULL
            }

            break
          }
          else {
            url <- paste(current_baseurl_, "cas", "sessions", session, sep = "/")
            httr::handle_reset(url)
            res <- httr::GET(url, auth_, config_)
            out <- httr::content(res, as = "parsed", type = "application/json", encoding = "utf-8")

            cookies <- httr::cookies(res)
            tkhttp_id_ <<- as.character(cookies[cookies$name == "tkhttp-id", ]$value)

            if (is.null(out$uuid)) {
              stop(paste(url, ":", out$error))
            }

            session_ <<- session

            break
          }
        }, error = function(e) {
          .self$set_next_connection_(e)
        })
      }
    },

    set_next_connection_ = function(connection_error) {
      host_index_ <<- host_index_ + 1
      tryCatch({
        current_hostname_ <<- hostname_[[host_index_]]
        current_baseurl_ <<- baseurl_[[host_index_]]
        current_port_ <<- port_[[host_index_]]
      }, error = function(e) {
        current_hostname_ <<- ""
        current_baseurl_ <<- ""
        current_port_ <<- -1
        if (is.null(connection_error)) {
          stop(e)
        }
        stop(connection_error)
      })
    },

    invoke = function(action_name, params) {
      body <- jsonlite::toJSON(.expand_params(params), auto_unbox = TRUE)
      while (TRUE) {
        out <- tryCatch({
          .trace_actions(action_name, params)
          res <- httr::POST(paste(current_baseurl_, "cas",
            "sessions", session_, "actions",
            action_name,
            sep = "/"
          ), auth_,
          httr::accept_json(),
          httr::content_type_json(),
          httr::add_headers("tkhttp-id" = tkhttp_id_),
          config_,
          body = body
          # , verbose()
          )

          cookies <- httr::cookies(res)
          tkhttp_id_ <<- as.character(cookies[cookies$name == "tkhttp-id", ]$value)

          results_ <<- httr::content(res, as = "text", encoding = "utf-8")

          break
        }, error = function(e) {
          .self$set_next_connection_(e)

          if (length(hostname_) > host_index_) {
            stop(e)
          }

          # Get ID of results
          action_name <- "session.listresults"
          body <- ""

          res <- httr::POST(paste(current_baseurl_, "cas",
            "sessions", session_, "actions",
            action_name,
            sep = "/"
          ), auth_,
          httr::accept_json(),
          httr::content_type_json(),
          httr::add_headers("tkhttp-id" = tkhttp_id_),
          config_,
          body = body
          # , verbose()
          )

          cookies <- httr::cookies(res)
          tkhttp_id_ <<- as.character(cookies[cookies$name == "tkhttp-id", ]$value)

          results_ <<- httr::content(res,
            as = "parsed",
            type = "application/json", encoding = "utf-8"
          )

          result_id <- res$result$`Queued Results`$rows[[1]][[1]]

          # Setup retrieval of results from ID
          return(list(
            action_name = "session.fetchresult",
            body = paste('{"id":', result_id, "}", sep = "")
          ))
        })

        if (!is.null(out)) {
          action_name <- out$action_name
          body <- out$body
        }
      }
>>>>>>> API cleanup

      if (class(results_) == "character") {
        results_ <<- jsonlite::fromJSON(gsub(
          "\f", "\\\\f",
          gsub("\r", "\\\\r", results_)
        ),
        simplifyVector = FALSE
        )
      }

      if (!("disposition" %in% names(results_))) {
        if ("error" %in% names(results_)) {
          stop(results_$error)
        } else {
          stop("Unknown error when invoking action")
        }
      }
    },

    receive = function() {
      out <- NULL
      if (!is.null(results_)) {
        out <- REST_CASMessage(results_, connection = .self)
      }
      results_ <<- NULL
      return(out)
    },

    getTypeName = function() {
      return("connection")
    },

    getSOptions = function() {
      return(soptions_)
    },

    destroy = function() {
      .self$close()
    },

    isNULL = function() {
      return(FALSE)
    },

    isConnected = function() {
      return(length(session_) > 1)
    },

    hasPendingResponses = function() {
      return(FALSE)
    },

    setZeroIndexedParameters = function() {
      return
    },

    copy = function() {
      return(REST_CASConnection(
        orig_hostname_, orig_port_, username_,
        rawToChar(jsonlite::base64_dec(password_)),
        soptions_, error_
      ))
    },

    getHostname = function() {
      if (grepl("^https:", current_hostname_, perl = TRUE)) {
        return(httr:parse_url(current_hostname_)$hostname)
      }
      return(current_hostname_)
    },

    getUsername = function() {
      return(username_)
    },

    getPort = function() {
      return(current_port_)
    },

    getSession = function() {
      return(session_)
    },

    close = function() {
      httr::DELETE(
        paste(current_baseurl_, "cas", "sessions", session_, sep = "/"),
        httr::add_headers("tkhttp-id" = tkhttp_id_),
        auth_, config_
      )
      session_ <<- ""
      return(0)
    },

    upload = function(file_name, params) {
      res <- httr::PUT(paste(current_baseurl_, "cas", "sessions",
        session_, "actions", "table.upload",
        sep = "/"
      ), auth_,
      httr::accept_json(),
      httr::add_headers(
        "JSON-Parameters" = jsonlite::toJSON(params, auto_unbox = TRUE),
        "Content-Type" = "application/octet-stream",
        "tkhttp-id" = tkhttp_id_
      ),
      config_,
      body = httr::upload_file(file_name)
      # , verbose()
      )

      cookies <- httr::cookies(res)
      tkhttp_id_ <<- as.character(cookies[cookies$name == "tkhttp-id", ]$value)

      results_ <<- httr::content(res, as = "parsed", type = "application/json", encoding = "utf-8")

      if (!("disposition" %in% names(results_))) {
        if ("error" %in% names(results_)) {
          stop(results_$error)
        } else {
          stop("Unknown error when uploading file")
        }
      }

      return(REST_CASResponse(results_))
    },

    stopAction = function() {
      return
    },

    getOptionType = function(option) {
      return("none")
    },

    getBooleanOption = function(option) {
      return
    },

    setBooleanOption = function(option, value) {
      return
    },

    getInt32Option = function(option) {
      return
    },

    setInt32Option = function(option, value) {
      return
    },

    getInt64Option = function(option) {
      return
    },

    setInt64Option = function(option, value) {
      return
    },

    setInt64OptionFromString = function(option, value) {
      return
    },

    getStringOption = function(option) {
      return
    },

    setStringOption = function(option, value) {
      return
    },

    getDoubleOption = function(option) {
      return
    },

    setDoubleOption = function(option, value) {
      return
    },

    enableDataMessages = function() {
      return
    },

    disableDataMessages = function() {
      return
    },

    getLastErrorMessage = function() {
      return("")
    }
  )
)

#' CAS connection event watcher class
#'
#' @keywords internal
#'
REST_CASConnectionEventWatcher <- setRefClass(
  Class = "REST_CASConnectionEventWatcher",

  fields = list(
    connections_ = "list",
    timeout_ = "numeric",
    soptions_ = "character",
    error_ = "ANY",
    idx_ = "numeric"
  ),

  methods = list(
    initialize = function(nconnections, timeout, soptions, error) {
      connections_ <<- list()
      timeout_ <<- timeout
      soptions_ <<- soptions
      error_ <<- error
      idx_ <<- 0
    },

    addConnection = function(connection) {
      connections_[[length(connections_) + 1]] <<- connection
    },

    wait = function() {
      if (idx_ < length(connections_)) {
        idx_ <<- idx_ + 1
        return(idx_ - 1)
      }
      return(-2)
    },

    getLastErrorMessage = function() {
      return("")
    }
  )
)
