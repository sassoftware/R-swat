# List available server actions
.connection_actions <- function(connection) {
  icons <- system.file(file.path("icons"), package = "swat")
  list(
    Help = list(
        icon = file.path(icons, "help.png"),
        callback = function() {
          utils::browseURL("https://go.documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=v_010&docsetId=caspg3r&docsetTarget=titlepage.htm&locale=en")
        }
    )
  )
}

.on_connection_opened <- function(connection, code) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionOpened(
    connectionObject = connection,

    type = "CAS",

    host = .host_spec(connection),

    icon = system.file(file.path("icons", "sas.png"), package = "swat"),

    displayName = .host_spec(connection),

    connectCode = code,

    disconnect = function() {
      cas.session.endSession(connection)
      cas.close(connection)
    },

    previewObject = function(rowLimit, ...) {
      .connection_preview_table(connection, rowLimit, ...)
    },

    listObjectTypes = function() {
      return(list(caslib = list(contains = list(table = list(contains = "data"),
                                                view = list(contains = "data")))))
    },

    listObjects = function(...) {
      return(.connection_list_objects(connection, ...))
    },

    listColumns = function(...) {
      .connection_list_columns(connection, ...)
    },

    actions = .connection_actions(connection)
  )
}

.on_connection_closed <- function(connection) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))
  observer$connectionClosed("CAS", .host_spec(connection))
}

.on_connection_updated <- function(connection, hint) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))
  observer$connectionUpdated("CAS", .host_spec(connection), hint = hint)
}

.connection_is_open <- function(connection) {
  return(connection$isConnected())
}

.connection_list_objects <- function(connection, ...) {
  if (is.null(connection) || !.connection_is_open(connection))
    return(character())

  args <- list(...)

  # Display tables for a specific caslib
  if (!is.null(args$caslib)) {
    out <- cas.table.tableInfo(connection, caslib = args$caslib, `_messageLevel` = "error")
    if (is.null(out$TableInfo))
      return(character())

    out <- out$TableInfo@df

    types <- out$View
    types[types == 0] <- "table"
    types[types == 1] <- "view"

    if (length(types) > 0) { 
      out <- data.frame(name = out$Name, type = types, stringsAsFactors = FALSE)
      return(out[order(out$name), ])
    }
  }

  # Display all caslibs
  out <- cas.table.caslibInfo(connection, `_messageLevel` = "error")
  if (is.null(out$CASLibInfo))
    return(character())

  out <- out$CASLibInfo@df

  types <- rep("caslib", dim(out)[1])
  if (length(types) > 0) { 
    out <- data.frame(name = out$Name, type = types, stringsAsFactors = FALSE)
    return(out[order(out$name), ])
  }

  character()
}

.connection_list_columns <- function(connection, ...) {
  if (!is.null(connection) && .connection_is_open(connection)) {
    args <- list(...)

    if (!is.null(args$view)) {
      table <- args$view
    } else {
      table <- args$table
    }

    out <- cas.table.columnInfo(connection, table = list(name = table, caslib = args$caslib),
                                `_messageLevel` = "error")
    if (is.null(out$ColumnInfo))
      return(NULL)

    out <- out$ColumnInfo@df

    return(data.frame(name = out$Column, type = out$Type, stringsAsFactors = FALSE))
  }
  NULL
}

.connection_preview_table <- function(connection, limit, ...) {
  if (!is.null(connection) && .connection_is_open(connection)) {
    args <- list(...)

    if (!is.null(args$view)) {
      table <- args$view
    } else {
      table <- args$table
    }

    out <- cas.table.fetch(connection, table = list(name = table, caslib = args$caslib),
                           to = limit, index = FALSE, `_messageLevel` = "error")
    if (is.null(out$Fetch))
      return(NULL)

    return(out$Fetch@df)
  }
  NULL
}

.host_spec <- function(connection) {
  return(connection$hostname[[1]])
}
