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
#'   \item The \code{\link{casDataFrame}} class provides an interface
#'         to the results for most actions.
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
#'    results <- runAction(s, "builtins.serverStatus")
#'    results$server
#'    nodes actions
#'  1     1      15
#' }
#'
#' @section Upload a data.frame to a CASTable:
#' \preformatted{
#'   irisct <- as.casTable(s, iris)
#' }
#'
#' @section Load a CAS actionSet:
#' \preformatted{
#'   runAction(s, "builtins.loadActionSet", actionSet="regression")
#' }
#'
#' @section Useful links:
#' \itemize{
#'   \item{\url{http://developer.sas.com/guides/r.html}}
#'   \item{\url{https://github.com/sassoftware/r-swat}}
#'   \item{Enter issues at \url{https://github.com/sassoftware/r-swat/issues}}
#' }
#'
#' @docType package
#' @name swat
#' @rawNamespace exportPattern("^[[:alpha:]]+")
#' @rawNamespace importFrom("methods", "as", "callNextMethod", "is", "new", "setClass", "setGeneric", "slot", "initialize", "show")
#' @rawNamespace importFrom("utils", "read.csv", "read.table", "str", "write.csv", "write.csv2", "write.table")
#' @rawNamespace if ( grepl('linux', R.version$os) ) {useDynLib(rswat)}
#' @rawRd % Copyright SAS Institute
"_PACKAGE"

.onAttach <- function(lib, pkg) {
    packageStartupMessage("SWAT ",
                          utils::packageDescription("swat", field="Version"),
                          appendLF = TRUE)
}

.onLoad <- function(lib, pkg) {
    if ( is.null(getOption('cas.print.messages')) )
    {
       options(
          cas.trace.actions=FALSE,
          cas.trace.ui.actions=FALSE,
          cas.print.messages=TRUE,
          cas.message.level='note',
          cas.message.level.ui='error',
          cas.max.download.rows=10000,
          cas.gen.function.sig=FALSE,
          cas.bygroup.mode='raw',  # raw, formatted, both, none
          cas.bygroup.dup.suffix='_f'
      )
   }

   if ( grepl('darwin', R.version$os) )
   {
      tryCatch(
         {
            library.dynam('rswat', pkg, lib, file.ext='.dylib')
            if ( Sys.getenv('TKPATH') == '' &&
                 file.exists(file.path(lib, pkg, 'libs', 'tkmk.dylib')) ) {
               InitializeTK(file.path(lib, pkg, 'libs:'))
            }
         }
         , error=function(e) {
                   message('NOTE: The extension module for binary protocol support is not available.')
                   message('      Only the CAS REST interface can be used.')
               })
   }
   else
   {
      tryCatch(
         {
            library.dynam('rswat', pkg, lib)
            if ( Sys.getenv('TKPATH') == '' ) {
               if ( file.exists(file.path(lib, pkg, 'libs', 'tkmk.so')) ) {
                  InitializeTK(file.path(lib, pkg, 'libs:'))
               }
               else if ( file.exists(file.path(lib, pkg, 'libs', 'x64', 'tkmk.dll')) ) {
                  InitializeTK(file.path(lib, pkg, 'libs', 'x64;'))
               }
            }
         }
         , error=function(e) {
                   message('NOTE: The extension module for binary protocol support is not available.')
                   message('      Only the CAS REST interface can be used.')
               })
   }
}

.onUnload <- function(lib)
{
   try(library.dynam.unload('rswat', lib), silent=FALSE)
}

cacheMetaData(1)

RETRY_ACTION_CODE <- 0x280034

errorcheck <- function(x) {
   if ( !is.null(x) ) {
      m <- x$getLastErrorMessage()
      if ( !is.null(m) && nchar(m[[1]]) > 0 ) {
         stop(m[[1]])
      }
   }
}

# Make getClass happy when using InitializeTK
setClass('_p_TKHndlp', slots=c(ref='externalptr'))
setClass('_p_void', slots=c(ref='externalptr'))

swat.as.integer64 <- function( value )
{
    return( as.numeric(value) )
}

CASRequest <- setRefClass(

   Class = 'CASRequest',

   fields = list(
      sw_request = 'ANY',
      params = 'list'
   ),

   methods = list (
      initialize = function( sw_request ) {
         callSuper( sw_request=sw_request )
         paramlist <- sw_request$getParameters()
         swat::errorcheck(sw_request)
         nparams <- sw_request$getNParameters()
         swat::errorcheck(sw_request)
         params <<- casvaluelist2r(paramlist, nparams)
         .self
      }
   )
)

CASResponse <- setRefClass(

   Class = 'CASResponse',

   fields = list(
      sw_response = 'ANY',
      disposition = 'list',
      performance = 'list',
      messages = 'list'
   ),

   methods = list(
      initialize = function( sw_response ) {
         callSuper( sw_response=sw_response )

         output = list()

         d <- list()
         d[['severity']] <- sw_response$getDispositionSeverity()
         swat::errorcheck(sw_response)
         d[['reason']] <- sw_response$getDispositionReason()
         swat::errorcheck(sw_response)
         d[['status']] <- sw_response$getDispositionStatus()
         swat::errorcheck(sw_response)
         d[['statusCode']] <- sw_response$getDispositionStatusCode()
         swat::errorcheck(sw_response)
         d[['debug']] <- sw_response$getDispositionDebug()
         swat::errorcheck(sw_response)
         disposition <<- d

         p <- list()
         p[['elapsedTime']] <- sw_response$getElapsedTime()
         swat::errorcheck(sw_response)
         p[['cpuUserTime']] <- sw_response$getCPUUserTime()
         swat::errorcheck(sw_response)
         p[['cpuSystemTime']] <- sw_response$getCPUSystemTime()
         swat::errorcheck(sw_response)
         p[['systemTotalMemory']] <- sw_response$getSystemTotalMemory()
         swat::errorcheck(sw_response)
         p[['systemNodes']] <- sw_response$getSystemNodes()
         swat::errorcheck(sw_response)
         p[['systemCores']] <- sw_response$getSystemCores()
         swat::errorcheck(sw_response)
         p[['memory']] <- sw_response$getMemory()
         swat::errorcheck(sw_response)
         p[['memoryOS']] <- sw_response$getMemoryOS()
         swat::errorcheck(sw_response)
         p[['memorySystem']] <- sw_response$getMemorySystem()
         swat::errorcheck(sw_response)
         p[['memoryQuota']] <- sw_response$getMemoryQuota()
         swat::errorcheck(sw_response)
         performance <<- p

         msgs <- list()
         nmessages <- sw_response$getNMessages()
         for ( i in 1:nmessages )
         {
             m <- sw_response$getNextMessage()
             swat::errorcheck(sw_response)
             if ( !is.null(m) && nchar(m) > 0 )
             {
                msgs[i] <- m
                if ( as.logical(getOption('cas.print.messages')) )
                {
                   message(m)
                }
            }
         }
         messages <<- msgs

         .self
      }
   )
)

CASDataMsgHandler <- setRefClass(

   Class = 'CASDataMsgHandler',

   fields = list(
      data = 'data.frame',
      reclen = 'numeric',
      nbuffrows = 'numeric',
      vars = 'list',
      ncols = 'integer',
      nrows = 'integer',
      finished = 'logical',
      sw_databuffer = 'ANY'
   ),

   methods = list(
      initialize = function( data, nrecs=1000, locale=NULL ) {
         data <<- data
         reclen <<- 0
         nrows <<- dim(data)[[1]]
         nbuffrows <<- min(nrecs, nrows)
         vars <<- list()
         finished <<- FALSE
         cnames <- names(data)
         ctypes <- lapply(data, class)

         for ( i in 1:length(ctypes) )
         {
            if ( ctypes[[i]] == 'integer' )
            {
               vars[[i]] <<- list(name=cnames[[i]], rtype='numeric',
                                       length=8, offset=reclen, type='int64')
               reclen <<- reclen + 8
            }
            else if ( ctypes[[i]] == 'integer64' )
            {
               vars[[i]] <<- list(name=cnames[[i]], rtype='numeric',
                                       length=8, offset=reclen, type='int64')
               reclen <<- reclen + 8
            }
            else if ( ctypes[[i]] == 'numeric' )
            {
               vars[[i]] <<- list(name=cnames[[i]], rtype='numeric',
                                       length=8, offset=reclen, type='sas')
               reclen <<- reclen + 8
            }
            else if ( ctypes[[i]] == 'character' || ctypes[[i]] == 'factor' )
            {
               vars[[i]] <<- list(name=cnames[[i]], rtype='char',
                                       length=16, offset=reclen, type='varchar')
               reclen <<- reclen + 16
            }
            else if ( ctypes[[i]] == 'Date' )
            {
               vars[[i]] <<- list(name=cnames[[i]], rtype='numeric', format='DATE',
                                       formattedlength=9,
                                       length=4, offset=reclen, type='date')
               reclen <<- reclen + 4
            }
            else if ( length(ctypes[[i]]) == 2 &&
                      ( ctypes[[i]][[1]] == 'POSIXct' || ctypes[[i]][[1]] == 'POSIXlt' ) )
            {
               vars[[i]] <<- list(name=cnames[[i]], rtype='numeric', format='DATETIME',
                                       formattedlength=20,
                                       length=8, offset=reclen, type='datetime')
               reclen <<- reclen + 8
            }
            else
            {
               message(sprintf("Unrecognized type for column %s.", cnames[[i]]))
            }
         }

         ncols <<- length(vars)

         soptions = ''
         if ( !is.null(locale) )
         {
            soptions <- gsub('^\\s+|\\s+$', '', paste(soptions, 'locale=', locale, sep=''))
         }

         sw_error <- SW_CASError(soptions)

         sw_databuffer <<- SW_CASDataBuffer(reclen, nbuffrows, soptions, sw_error)
         swat::errorcheck(sw_error)

         .self
      },

      show = function() {
         cat('CASDataMsgHandler()\n')
      },

      write = function( row, values ) {
         for ( col in 1:ncols )
         {
            if ( tolower(vars[[col]][['type']]) == 'varchar' )
            {
               sw_databuffer$setString(row-1, vars[[col]][['offset']],
                                       as.character(values[[col]]))
               swat::errorcheck(sw_databuffer)
            }
            else if ( tolower(vars[[col]][['type']]) == 'int32' )
            {
               sw_databuffer$setInt32(row-1, vars[[col]][['offset']], as.integer(values[[col]]))
               swat::errorcheck(sw_databuffer)
            }
            else if ( tolower(vars[[col]][['type']]) == 'int64' )
            {
               tryCatch({
                  sw_databuffer$setInt64FromString(row-1, vars[[col]][['offset']],
                                                   format(values[[col]], scientific=FALSE))
               }, error=function (e) {
                  sw_databuffer$setInt64(row-1, vars[[col]][['offset']], as.integer(values[[col]]))
               })
               swat::errorcheck(sw_databuffer)
            }
            else if ( tolower(vars[[col]][['type']]) == 'date' )
            {
               sw_databuffer$setInt32(row-1, vars[[col]][['offset']], rDate2cas(values[[col]]))
               swat::errorcheck(sw_databuffer)
            }
            else if ( tolower(vars[[col]][['type']]) == 'datetime' || 
                      tolower(vars[[col]][['type']]) == 'time' )
            {
               if ( class(values[[col]])[[1]] == 'POSIXlt' ) {
                  value <- rPOSIXlt2cas(values[[col]])
               } else {
                  value <- rPOSIXct2cas(values[[col]])
               }
               tryCatch({
                  sw_databuffer$setInt64FromString(row-1, vars[[col]][['offset']],
                                                   format(value, scientific=FALSE))
               }, error=function (e) {
                  sw_databuffer$setInt64(row-1, vars[[col]][['offset']], as.integer(value))
               })
               swat::errorcheck(sw_databuffer)
            }
            else
            {
               sw_databuffer$setDouble(row-1, vars[[col]][['offset']], as.numeric(values[[col]]))
               swat::errorcheck(sw_databuffer)
            }
         }
      },

      send = function( connection, nrecs ) {
         rc <- sw_databuffer$send( connection$sw_connection, nrecs )
         swat::errorcheck(sw_databuffer)
      },

      finish = function( connection ) {
         finished <<- TRUE
         out <- sw_databuffer$send( connection$sw_connection, 0 )
         swat::errorcheck(sw_databuffer)
         return( out )
      },

      getrow = function( row ) {
         if ( row <= nrows ) {
            return (as.list(data[row,]))
         }
         return (NULL)
      },

      call = function( request, connection ) {
         if ( finished )
         {
            stop('The data message handler has already been used.')
         }

         inputrow <- 0
         row <- 0

         while ( TRUE )
         {
            written <- FALSE

            # populate buffer
            for ( row in 1:nbuffrows )
            {
               inputrow <- inputrow + 1
               values <- .self$getrow(inputrow)
               if ( is.null(values) )
               {
                  row <- row - 1
                  break
               }
               .self$write(row, values)
               written <- TRUE
            }

            # send it
            if ( written )
            {
               .self$send(connection, row)
               res <- connection$getone()
               if ( class(res) == 'CASRequest' )
               {
                  next
               }
               else if ( class(res) == 'CASResponse' )
               {
                  if ( res$disposition$severity <= 1 )
                  {
                     messages <- res$messages
                     while ( class(res) == 'CASResponse' )
                     {
                        res <- connection$getone()
                        messages <- c(messages, res$messages)
                        if ( res$disposition$severity > 1 )
                        {
                           res$messages <- messages
                           break
                        }
                     }
                     if ( class(res) == 'CASRequest' )
                     {
                        next
                     }
                  }
               }
            }
            else
            {
               break
            }

            # If we failed for some reason, return the last response
            if ( class(res) == 'CASResponse' && res$disposition$severity > 1 )
            {
               return (res)
            }
         }

         # end it
         .self$finish(connection)
         return (connection$getone())
      }
   )
)

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
#'
#' @return A CAS object.
#' @export
#' @rawRd % Copyright SAS Institute
#'
#' @examples
#' \dontrun{
#' # Use binary communication and credentals from the default authinfo location.
#' s <- CAS('cloud.example.com', 5570) 
#'
#' # Use HTTPS and credentials from the default authinfo location.
#' s <- CAS('cloud.example.com', 8777, protocol='https')
#'
#' # Use binary or HTTP communication and credentials from an alternative authinfo.
#' s <- CAS('cloud.example.com', 5570, protocol='auto', authinfo=~/alt.txt)
#'
#' # Use binary or HTTP communcation and specify credentials.
#' s <- CAS('cloud.example.com', 8777, protocol='auto', username="sasdemo"
#'   password="!s3cret")
#' }
CAS <- setRefClass(

   Class = 'CAS',

   fields = list(
      sw_connection = 'ANY',
      sw_error = 'ANY',
      soptions = 'character',
      hostname = 'character',
      port = 'numeric',
      protocol = 'character',
      username = 'character',
      session = 'character'
   ),

   methods = list(
      initialize = function( hostname=NULL, port=NULL, username='', password='', ... ) {
         prototype <- NULL
         protocol <<- 'auto'
         options <- list(...)

         if ( is.null(hostname) )
         {
            if ( Sys.getenv('CASHOST') != '' )
               hostname <<- Sys.getenv('CASHOST')
            else
               hostname <<- 'localhost'
         }
         else
         {
            hostname <<- hostname
         }

         if ( is.null(port) )
         {
            if ( Sys.getenv('CASPORT') != '' )
               port <<- as.integer(Sys.getenv('CASPORT'))
            else
               port <<- 5570
         }
         else
         {
            port <<- port
         }

         if ( Sys.getenv('CASPROTOCOL') != '' )
             protocol <<- Sys.getenv('CASPROTOCOL')

         soptions <<- ''

         if ( !is.null(options) && 'locale' %in% names(options) )
         {
            soptions <<- gsub('^\\s+|\\s+$', '', paste(soptions, paste('locale=', options$locale, sep=''), sep=' '))
         }

         if ( !is.null(options) && 'session' %in% names(options) )
         {
            soptions <<- gsub('^\\s+|\\s+$', '', paste(soptions, paste('session=', options$session, sep=''), sep=' '))
         }

         if ( !is.null(options) && 'protocol' %in% names(options) )
         {
            soptions <<- gsub('^\\s+|\\s+$', '', paste(soptions, paste('protocol=', options$protocol, sep=''), sep=' '))
            protocol <<- options$protocol
         }

         if ( !is.null(options) && 'authinfo' %in% names(options) && password == '' )
         {
            if ( class(options$authinfo) == 'character' )
            {
               password <- paste('authinfo={', options$authinfo, '}', sep='')
            }
            else if ( class(options$authinfo) == 'list' )
            {
               password <- ''
               for ( i in 1:length(options$authinfo) )
               {
                  password <- paste(password, '{', options$authinfo[[i]], '}', sep='')
               }
               password <- paste('authinfo={', password, '}', sep='')
            }
         }

         if ( !is.null(options) && 'prototype' %in% names(options) )
         {
            prototype <- options$prototype
         }

         if ( is.null(.self$protocol) )
             protocol <<- 'auto'

         if ( .self$protocol == 'auto' )
         {
            for ( i in 1:length(.self$hostname) )
            {
               url <- httr::parse_url(.self$hostname[[i]])
               if ( is.null(url$hostname) )
                   url$hostname <- .self$hostname[[i]]
               if ( is.null(url$port) )
                   url$port <- port
               if ( is.null(url$scheme) )
                   url$scheme <- 'auto'

               if ( url$scheme != 'auto' )
               {
                   protocol <<- url$scheme
                   break
               }

               tryCatch({
                    httr::GET(paste('http://', url$hostname, ':', url$port, '/cas', sep=''))
                    protocol <<- 'http'
                    hostname <<- gsub('^auto', 'http', .self$hostname, perl=TRUE)
                    port <<- as.integer(url$port)
               }, error=function (e) { })

               if ( .self$protocol == 'http' )
                  break
            }

            if ( .self$protocol == 'auto' )
               protocol <<- 'cas'
         }

         if ( .self$protocol == 'http' || .self$protocol == 'https' )
         {
            sw_error <<- REST_CASError(soptions)
            CASConnection <- REST_CASConnection
         }
         else
         {
            tryCatch({
               sw_error <<- SW_CASError(soptions)
            }, error = function (e) {
               stop(paste('Could not create object from the extension module.\n',
                          'You may be attempting to create a binary CAS connection',
                          'on a platform\n that only supports the CAS REST interface.'))
            })
            CASConnection <- SW_CASConnection
            for ( i in 1:length(.self$hostname) )
            {
               url <- httr::parse_url(.self$hostname[[i]])
               if ( is.null(url$hostname) )
                   url$hostname <- .self$hostname[[i]]
               if ( is.null(url$port) )
                   url$port <- port
               hostname[[i]] <<- url$hostname
               port <<- as.integer(url$port)
            }
         }

         if ( is.null(prototype) )
         {
            conn <- CASConnection(.self$hostname, .self$port, username, password, soptions, sw_error)
            swat::errorcheck(sw_error)
            sw_connection <<- conn

            # This should always be disabled.  Messages are printed by CASResponse.
            sw_connection$setBooleanOption('print_messages', FALSE)
         }
         else
         {
            sw_connection <<- prototype$sw_connection$copy()
            swat::errorcheck(prototype$sw_connection)
         }

         message("NOTE: Connecting to CAS and generating CAS action functions for loaded")
         message("      action sets...")
         hostname <<- sw_connection$getHostname()
         swat::errorcheck(sw_connection)
         port <<- sw_connection$getPort()
         swat::errorcheck(sw_connection)
         username <<- sw_connection$getUsername()
         swat::errorcheck(sw_connection)
         session <<- sw_connection$getSession()
         swat::errorcheck(sw_connection)
         soptions <<- sw_connection$getSOptions()
         swat::errorcheck(sw_connection)
         callSuper(sw_connection=.self$sw_connection, sw_error=.self$sw_error,
                   soptions=.self$soptions, hostname=.self$hostname, port=.self$port,
                   username=.self$username, session=.self$session)

         ml <- getOption('cas.message.level')
         options(cas.message.level='error')
         actsets = listActionSets(.self)
         if (!as.logical(getOption('cas.gen.function.sig'))) {
            message("NOTE: To generate the functions with signatures (for tab completion), set ")
            message("      options(cas.gen.function.sig=TRUE).")
         }
         for (i in 1:length(actsets$actionset))
            {
            if (actsets$loaded[i] == 1)
               gen.functions(.self, actsets$actionset[i])
            }
         options(cas.message.level=ml)

         .self
      },

      close = function() {
         rc <- sw_connection$close()
         swat::errorcheck(sw_connection)
         return(rc)
      },

      copy = function() {
         return (CAS$new(hostname='', port=0, prototype=.self))
      },

      fork = function(num=2) {
         output = c(.self)
         for ( i in 1:(num-1) )
         {
            output <- c(output, .self$copy())
         }
         return (output)
      },

      show = function() {
         cat('CAS(hostname=', hostname, ', port=', port, ', username=', username, ', session=', session, ', protocol=', protocol, ')\n', sep='')
      },

      enableDataMessages = function() {
         rc <- sw_connection$enableDataMessages()
         swat::errorcheck(sw_connection)
         return(rc)
      },

      disableDataMessages = function() {
         rc <- sw_connection$enableDataMessages()
         swat::errorcheck(sw_connection)
         return(rc)
      },

      getone = function(...) {
         args <- list(...)
         output <- NULL
         datamsghandler <- NULL
         if ( !is.null(args$datamsghandler) ) {
            .self$enableDataMessages()
            datamsghandler <- args$datamsghandler
         }
         sw_message <- sw_connection$receive()
         swat::errorcheck(sw_connection)
         if ( !sw_message$isNULL() )
         {
            t <- sw_message$getType()
            if ( t == 'response' )
            {
               sw_response <- sw_message$toResponse(sw_connection)
               swat::errorcheck(sw_message)
               if ( !sw_response$isNULL() )
               {
                  output <- CASResponse$new(sw_response=sw_response)
               }
            }
            else if ( t == 'request' && !is.null(datamsghandler) )
            {
               sw_request <- sw_message$toRequest(sw_connection)
               swat::errorcheck(sw_message)
               if ( !sw_request$isNULL() )
               {
                  output <- datamsghandler$call(CASRequest$new(sw_request=sw_request), .self)
               }
            }
            else if ( t == 'request' )
            {
               sw_request <- sw_message$toRequest(sw_connection)
               swat::errorcheck(sw_message)
               if ( !sw_request$isNULL() )
               {
                  output <- CASRequest$new(sw_request=sw_request)
               }
            }
         }
         if ( !is.null(datamsghandler) ) {
            .self$disableDataMessages()
         }
         return (output)
      },

      invoke = function(actn, ...) {
         # Make sure options are updated
         args <- list(...)
         if ( class(.self$sw_connection) == 'REST_CASConnection' ) {
             sw_connection$invoke(actn, args)
         } else {
             if ( grepl('nil', capture.output(slot(sw_connection, 'ref'))) ) {
                 stop('The connection object is invalid.  Please create a new connection.')
             }
             sw_connection$setBooleanOption('trace_actions', as.logical(getOption('cas.trace.actions')))
             sw_connection$setBooleanOption('trace_ui_actions', as.logical(getOption('cas.trace.ui.actions')))
             sw_connection$invoke(actn, r2cas(.self$soptions, .self$sw_error, args))
         }
         .self
      },

      retrieve = function(actn, ...) {
         args <- list(...)
         datamsghandler <- NULL
         if ( !is.null(args$datamsghandler) ) {
            datamsghandler <- args$datamsghandler
         }
         repeat {
            .self$invoke(actn, ...)
            output <- list()
            results <- list()
            messages <- list()
            events <- list()
            idx <- 1
            while ( TRUE ) {
               nextresp <- getnext(.self, datamsghandler=datamsghandler)
               if ( is.null(nextresp$response) ) break
   
               while ( TRUE ) {
                  result <- getnext(nextresp$response)
                  if ( is.null(result) || length(result) < 1) break
                  for ( i in 1:length(result) )
                  {
                     key <- names(result[i])
                     if ( is.null(key) || nchar(key) == 0 )
                     {
                        results[[idx]] <- result[[i]]
                        idx <- idx + 1
                     }
                     else if ( substr(key, 1, 1) == '$' )
                     {
                         events[[key]] <- result[[i]]
                     }
                     else
                     {
                        results[[key]] <- result[[i]]
                     }
                  }
               }

               messages <- c(messages, nextresp$response$messages)

               output[['performance']] <- nextresp$response$performance
               output[['disposition']] <- nextresp$response$disposition
            }
            if ( output[['disposition']][['statusCode']] != RETRY_ACTION_CODE ) {
                break
            }
         }
         output[['messages']] <- messages
         output[['results']] <- results
         output[['events']] <- events
         return (output)
      },

      upload = function(data, ...) {
         filename <- data
         args <- list(...)
         tmp <- ''

         if ( class(data) == 'character' )
         {
            if ( grepl('^(https?|ftps?|file)://', data, perl=TRUE) )
            {
                tmp <- tempfile()
                filename <- tmp
                name <- basename(data)
                ext <- tools::file_ext(data)
                download.file(data, destfile=tmp, method='auto', quiet=TRUE)
            }
            else
            {
                if ( !file.exists(data) )
                {
                    stop(paste('The file', data, 'does not exist.'))
                }
                ext <- tools::file_ext(data)
                name <- tools::file_path_sans_ext(basename(data))
            }
         }
         else
         {
            ext <- 'csv'
            tmp <- tempfile()
            filename <- tmp
            name <- basename(tmp)
            write.csv(data, file=tmp, row.names=FALSE, na='', fileEncoding='UTF-8')
         }

         if ( is.null(args) ) args = list()

         if ( !('casout' %in% names(args)) && !('casOut' %in% names(args)) )
         {
            args[['casout']] <- name
         }

         if ( !('importoptions' %in% names(args)) && !('importOptions' %in% names(args)) )
         {
            if ( ext == 'sav' ) ext <- 'spss'
            else if ( ext == 'xlsx' ) ext <- 'excel'
            else if ( ext == 'sashdat' ) ext <- 'hdat'
            else if ( ext == 'sas7bdat' ) ext <- 'basesas'
            args[['importoptions']] <- list(filetype=ext)
         }

         if ( class(.self$sw_connection) == 'REST_CASConnection' ) {
             response <- sw_connection$upload(filename, args)
         } else {
             # The upload method may print messages.  This is the only way to see them.
             sw_connection$setBooleanOption('print_messages', getOption('cas.print.messages'))
             response <- sw_connection$upload(filename, r2cas(.self$soptions, .self$sw_error, args))
             sw_connection$setBooleanOption('print_messages', FALSE)
         }

         if ( tmp != '' ) unlink(tmp)

         output <- list()
         results <- list()
         messages <- list()
         idx <- 1
         if ( !is.null(response) )
         {
            response <- CASResponse(response)
            while ( TRUE )
            {
               result <- getnext(response)
               if ( is.null(result) ) break
               for ( i in 1:length(result) )
               {
                  key <- names(result[i])
                  if ( is.null(key) || nchar(key) == 0 )
                  {
                     results[[idx]] <- result[[i]]
                     idx <- idx + 1
                  }
                  else
                  {
                     results[[key]] <- result[[i]]
                  }
               }
            }

            messages <- c(messages, response$messages)

            output[['performance']] <- response$performance
            output[['disposition']] <- response$disposition
         }
         output[['messages']] <- messages
         output[['results']] <- results
         return (output)
      }
   )
)

CASEventWatcher <- setRefClass(

   Class = 'CASEventWatcher',

   fields = list(
      sw_watcher = 'ANY',
      connections = 'list',
      datamsghandler = 'ANY'
   ),

   methods = list(
      initialize = function( connections, ... ) {
         if (class(connections) == "list") {
            connections <<- connections
         }
         else {
            connections <<- list(connections)
         }

         rest <- 0
         binary <- 0
         for ( i in 1:length(connections) )
         {
             if ( grepl('^REST_', class(connections[[i]]$sw_connection), perl=TRUE) )
                 rest <- rest + 1
             else
                 binary <- binary + 1
         }
         if ( rest > 0 && binary > 0 )
             stop('REST and binary connections can not be mixed')

         options = list(...)
         if ( !is.null(options) && 'timeout' %in% names(options) ) {
            timeout <- options$timeout;
         }
         else {
            timeout <- 0;
         }
         if ( !is.null(options) && 'datamsghandler' %in% names(options) ) {
            datamsghandler <<- options$datamsghandler;
         }
         else {
            datamsghandler <<- NULL;
         }

         if ( binary )
         {
            sw_watcher <<- SW_CASConnectionEventWatcher(length(connections), timeout,
                                 connections[[1]]$soptions, connections[[1]]$sw_error)
         }
         else
         {
            sw_watcher <<- REST_CASConnectionEventWatcher(length(connections), timeout,
                                   connections[[1]]$soptions, connections[[1]]$sw_error)
         }
         swat::errorcheck(connections[[1]]$sw_error)

         for ( i in 1:length(connections) ) {
            sw_watcher$addConnection(connections[[i]]$sw_connection)
            swat::errorcheck(sw_watcher)
         }

         callSuper(sw_watcher=.self$sw_watcher, connections=.self$connections,
                   datamsghandler=.self$datamsghandler)
         .self
      }
   )
)

getnext <- function(...) {
   args <- list(...)
   connections <- args[unlist(lapply(args, function(x) class(x) == 'CAS'))]
   responses <- args[unlist(lapply(args, function(x) class(x) == 'CASResponse'))]
   watchers <- args[unlist(lapply(args, function(x) class(x) == 'CASEventWatcher'))]
   kwargs <- args[unlist(lapply(args, function(x) class(x) != 'CAS'))]

   # get next result from response
   if ( length(responses) > 0 ) {
      sw_result <- responses[[1]]$sw_response$getNextResult()
      swat::errorcheck(responses[[1]]$sw_response)
      if ( sw_result$isNULL() )
      {
         return (NULL)
      }
      output <- list()
      key <- sw_result$getKey()
      swat::errorcheck(sw_result)
      if ( is.null(key) || nchar(key) == 0 )
      {
         val = cas2r(sw_result)
         if ( !is.null(val) )
         {
            if (length(val) > length(output[1]))
               output[[1]] <- val
            else
               output[1] <- val
         }
      }
      else
      {
         val = cas2r(sw_result)
         if ( !is.null(val) )
         {
             output[[key]] <- val
         }
      }
      return (output)
   }

   # handle multiple connections
   else if ( length(watchers) > 0 ) {
      watcher = watchers[[1]]
      while ( TRUE ) {
         idx <- watcher$sw_watcher$wait()
         swat::errorcheck(watcher$sw_watcher)

         # finished
         if ( idx == -2 ) {
            break
         }

         # timeout
         # TODO: Return something else so they know it's a timeout
         if ( idx == -1 ) {
            return (NULL,NULL)
         }

         # TODO: merge datamsghandler from watcher
         # TODO: return response and connection
         return (list(response=do.call(watcher$connections[[idx+1]]$getone,kwargs),
                      connection=watcher$connections[[idx+1]]))
      }
   }

   # get next response from connection
   else if ( length(connections) > 0 ) {
      return (list(response=do.call(connections[[1]]$getone,kwargs),
                    connection=connections[[1]]))
   }

   return (NULL)
}

nonull.string <- function(str) {
    if ( is.null(str) ) {
        return( '' )
    }
    return( str )
}

cas2r <- function(sw_value) {
   t <- sw_value$getType()
   swat::errorcheck(sw_value)
   if ( t == "nil" ) {
      return (NULL)
   } else if ( t == "int32" ) {
      out <- sw_value$getInt32()
      swat::errorcheck(sw_value)
      return(out)
   } else if ( t == "int64" ) {
      out <- sw_value$getInt64AsString()
      swat::errorcheck(sw_value)
      return(swat.as.integer64(out))
   } else if ( t == "double" ) {
      out <- sw_value$getDouble()
      swat::errorcheck(sw_value)
      return(out)
   } else if ( t == "string" ) {
      out <- sw_value$getString()
      if ( is.null(out) ) { out <- "" }
      swat::errorcheck(sw_value)
      return(out)
   } else if ( t == "boolean" ) {
      out <- sw_value$getBoolean()
      swat::errorcheck(sw_value)
      return(out)
   } else if ( t == "date" ) {
      out <- sw_value$getDate()
      swat::errorcheck(sw_value)
      return(cas2rDate(out))
   } else if ( t == "time" ) {
      out <- sw_value$getTimeAsString()
      swat::errorcheck(sw_value)
      return(cas2rPOSIXct(out))
   } else if ( t == "datetime" ) {
      out <- sw_value$getDateTimeAsString()
      swat::errorcheck(sw_value)
      return(cas2rPOSIXct(out))
   } else if ( t == "table" )  {
      sw_table <- sw_value$getTable()
      swat::errorcheck(sw_value)
      nCols <- sw_table$getNColumns()
      swat::errorcheck(sw_table)
      nRows <- sw_table$getNRows()
      swat::errorcheck(sw_table)
      #print(sprintf("%s has %d rows and %d columns", sw_table$getName(), nRows, nCols))
      table <- NULL

      name <- nonull.string(sw_table$getName())
      swat::errorcheck(sw_table)
      label <- nonull.string(sw_table$getLabel())
      swat::errorcheck(sw_table)
      title <- nonull.string(sw_table$getTitle())
      swat::errorcheck(sw_table)

      # Get table extended attributes
      if ( class(sw_table) == 'REST_CASTable' )
      {
         attrs <- sw_table$getAttributes()
      }
      else
      {
         attrs <- list()
         while ( TRUE )
         {
            key <- sw_table$getNextAttributeKey()
            swat::errorcheck(sw_table)
            if ( is.null(key) ) break;
            typ = sw_table$getAttributeType(key)
            swat::errorcheck(sw_table)

            if ( typ == 'int32' )
            {
               attrs[[key]] <- sw_table$getInt32Attribute(key)
               swat::errorcheck(sw_table)
            }
            else if ( typ == 'int64' )
            {
               attrs[[key]] <- sw_table$getInt64AttributeAsString(key)
               swat::errorcheck(sw_table)
               attrs[[key]] <- swat.as.integer64(attrs[[key]])
            }
            else if ( typ == 'double' )
            {
               attrs[[key]] <- sw_table$getDoubleAttribute(key)
               swat::errorcheck(sw_table)
            }
            else if ( typ == 'string' )
            {
               attrs[[key]] <- nonull.string(sw_table$getStringAttribute(key))
               swat::errorcheck(sw_table)
            }
            else if ( typ == 'date' )
            {
               attrs[[key]] <- sw_table$getInt32Attribute(key)
               swat::errorcheck(sw_table)
               attrs[[key]] <- cas2rDate(attrs[[key]])
            }
            else if ( typ == 'time' )
            {
               attrs[[key]] <- sw_table$getInt64AttributeAsString(key)
               swat::errorcheck(sw_table)
               attrs[[key]] <- cas2rPOSIXct(attrs[[key]])
            }
            else if ( typ == 'datetime' )
            {
               attrs[[key]] <- sw_table$getInt64AttributeAsString(key)
               swat::errorcheck(sw_table)
               attrs[[key]] <- cas2rPOSIXct(attrs[[key]])
            }
            else if ( typ == 'int32-array' )
            {
               nitems <- sw_table$getAttributeNItems()
               swat::errorcheck(sw_table)
               value <- list()
               for ( i in 1:nitems )
               {
                  value[[i]] <- sw_table$getInt32ArrayAttributeItem(key, i-1)
                  swat::errorcheck(sw_table)
               }
               attrs[[key]] <- value
            }
            else if ( typ == 'int64-array' )
            {
               nitems <- sw_table$getAttributeNItems()
               swat::errorcheck(sw_table)
               value <- list()
               for ( i in 1:nitems )
               {
                  value[[i]] <- sw_table$getInt64ArrayAttributeItemAsString(key, i-1)
                  swat::errorcheck(sw_table)
                  value[[i]] <- swat.as.integer64(value[[i]])
               }
               attrs[[key]] <- value
            }
            else if ( typ == 'double-array' )
            {
               nitems <- sw_table$getAttributeNItems()
               swat::errorcheck(sw_table)
               value <- list()
               for ( i in 1:nitems )
               {
                  value[[i]] <- sw_table$getDoubleArrayAttributeItem(key, i-1)
                  swat::errorcheck(sw_table)
               }
               attrs[[key]] <- value
            }
         }
      }

      col.labels <- c()
      col.formats <- c()
      col.types <- c()
      col.widths <- c()
      col.sizes <- c()

      if (nCols > 0)
      {
         for ( col in 0:(nCols-1) )
         {
             col.labels <- c(col.labels, nonull.string(sw_table$getColumnLabel(col)))
             swat::errorcheck(sw_table)
             col.formats <- c(col.labels, nonull.string(sw_table$getColumnFormat(col)))
             swat::errorcheck(sw_table)
             col.widths <- c(col.widths, sw_table$getColumnWidth(col))
             swat::errorcheck(sw_table)
             col.types <- c(col.types, sw_table$getColumnType(col))
             swat::errorcheck(sw_table)
             col.sizes <- c(col.sizes, list(1, sw_table$getColumnArrayNItems(col)))
             swat::errorcheck(sw_table)
         }
      }

      # Get column extended attributes
      if ( class(sw_table) == 'REST_CASTable' )
      {
         col.attrs <- sw_table$getColumnAttributes()
      }
      else
      {
         # Get column extended attributes
         col.attrs <- c()
         if (nCols > 0)
         {
            for ( col in 0:(nCols-1) )
            {
               info <- list()
               col.attrs <- c(col.attrs, info)

               while ( TRUE )
               {
                  key <- sw_table$getNextColumnAttributeKey(col)
                  swat::errorcheck(sw_table)
                  if ( is.null(key) ) break;
                  typ <- sw_table$getColumnAttributeType(col, key)
                  swat::errorcheck(sw_table)

                  if ( typ == 'double' )
                  {
                     info[[key]] <- sw_table$getColumnDoubleAttribute(col, key)
                     swat::errorcheck(sw_table)
                  }
                  else if ( typ == 'int32' )
                  {
                     info[[key]] <- sw_table$getColumnInt32Attribute(col, key)
                     swat::errorcheck(sw_table)
                  }
                  else if ( typ == 'int64' )
                  {
                     info[[key]] <- sw_table$getColumnInt64AttributeAsString(col, key)
                     swat::errorcheck(sw_table)
                     info[[key]] <- swat.as.integer64(info[[key]])
                  }
                  else if ( typ == 'string' )
                  {
                     info[[key]] <- nonull.string(sw_table$getColumnStringAttribute(col, key))
                     swat::errorcheck(sw_table)
                  }
                  else if ( typ == 'date' )
                  {
                     info[[key]] <- sw_table$getColumnInt32Attribute(col, key)
                     swat::errorcheck(sw_table)
                     info[[key]] <- cas2rDate(info[[key]])
                  }
                  else if ( typ == 'time' )
                  {
                     info[[key]] <- sw_table$getColumnInt64AttributeAsString(col, key)
                     swat::errorcheck(sw_table)
                     info[[key]] <- cas2rPOSIXct(info[[key]])
                  }
                  else if ( typ == 'datetime' )
                  {
                     info[[key]] <- sw_table$getColumnInt64AttributeAsString(col, key)
                     swat::errorcheck(sw_table)
                     info[[key]] <- cas2rPOSIXct(info[[key]])
                  }
                  else if ( typ == 'int32-array' )
                  {
                     nitems <- sw_table$getColumnAttributeNItems(col, key)
                     swat::errorcheck(sw_table)
                     value <- list()
                     for ( i in 1:nitems )
                     {
                        value[[i]] <- sw_table$getColumnInt32ArrayAttributeItem(col, key, i-1)
                        swat::errorcheck(sw_table)
                     }
                     info[[key]] <- value
                  }
                  else if ( typ == 'int64-array' )
                  {
                     nitems <- sw_table$getColumnAttributeNItems(col, key)
                     swat::errorcheck(sw_table)
                     value <- list()
                     for ( i in 1:nitems )
                     {
                        value[[i]] <- sw_table$getColumnInt64ArrayAttributeItemAsString(col, key, i-1)
                        swat::errorcheck(sw_table)
                        value[[i]] <- swat.as.integer64(value[[i]])
                     }
                     info[[key]] <- value
                  }
                  else if ( typ == 'double-array' )
                  {
                     nitems <- sw_table$getColumnAttributeNItems(col, key)
                     swat::errorcheck(sw_table)
                     value <- list()
                     for ( i in 1:nitems )
                     {
                        value[[i]] <- sw_table$getColumnDoubleArrayAttributeItem(col, key, i-1)
                        swat::errorcheck(sw_table)
                     }
                     info[[key]] <- value
                  }
               }
            }
         }
      }

      add_column <- function (table, newname, newcol)
      {
          if ( is.null(table) ) {
              table <- data.frame(newcol, stringsAsFactors=FALSE)
              names(table) <- newname
          } else {
              table[newname] <- list(newcol)
          }
          return( table )
      }

      add_bygroup_columns <- function ( table )
      {
          if ( getOption('cas.bygroup.mode') == 'none' )
          {
              return( table )
          }

          grpnum <- 1
          grpvar_lbl <- paste('ByVar', grpnum, sep='')
          grpval_lbl <- paste('ByVar', grpnum, 'Value', sep='')
          grpvalf_lbl <- paste('ByVar', grpnum, 'ValueFormatted', sep='')

          while ( !is.null(attrs[[grpvar_lbl]]) )
          {
             varname <- attrs[[grpvar_lbl]]
             rawval <- attrs[[grpval_lbl]]
             fmtval <- attrs[[grpvalf_lbl]]

             if ( getOption('cas.bygroup.mode') == 'raw' )
             {
                 table <- add_column(table, varname, rep(rawval, nRows))
             }
             else if ( getOption('cas.bygroup.mode') == 'formatted' )
             {
                 table <- add_column(table, varname, rep(fmtval, nRows))
             }
             else if ( getOption('cas.bygroup.mode') == 'both' )
             {
                 table <- add_column(table, varname, rep(rawval, nRows))
                 table <- add_column(table,
                                     paste(varname,
                                           getOption('cas.bygroup.dup.suffix'), sep=''),
                                     rep(fmtval, nRows))
             } 
             else
             {
                 stop(paste('Unrecognized value for cas.bygroup.mode:',
                            getOption('cas.bygroup.mode')))
             }

             grpnum <- grpnum + 1
             grpvar_lbl <- paste('ByVar', grpnum, sep='')
             grpval_lbl <- paste('ByVar', grpnum, 'Value', sep='')
             grpvalf_lbl <- paste('ByVar', grpnum, 'ValueFormatted', sep='')
          }

          return( table )
      }

      int32_missval <- -2147483648
      int64_missval <- '-9223372036854775808'
      setMissing <- function (value, missval)
      {
          if ( is.na(value) || is.nan(value) ) {
              return( NA )
          }
          if ( value == missval ) {
              return( NA )
          }
          return( value )
      }

      if ( nCols > 0 )
      {
         columns <- list()
         colnames <- c()

         table <- add_bygroup_columns(table)

         for (col in 0 : (nCols - 1))
         {
            #print(sprintf("Processing column %d", col))
            column <- c()
            t <- sw_table$getColumnType(col)
            swat::errorcheck(sw_table)
            len <- sw_table$getColumnArrayNItems(col)
            swat::errorcheck(sw_table)
            name <- sw_table$getColumnName(col)
            swat::errorcheck(sw_table)

            if (nRows == 0)
            {
               table <- add_column(table, name, character())
            }
            else if (t == 'int32')
            {
               for (row in 0:(nRows-1)) {
                  out <- sw_table$getInt32Value(row, col)
                  swat::errorcheck(sw_table)
                  if ( identical(out, numeric(0)) )
                      out <- 0
                  column <- c(column, setMissing(out, int32_missval))
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'int32-array')
            {
               for (elem in 0:(len-1))
               {
                  column <- c()
                  for (row in 0:(nRows-1))
                  {
                     out <- sw_table$getInt32ArrayValue(row, col, elem)
                     swat::errorcheck(sw_table)
                     if ( identical(out, numeric(0)) )
                         out <- 0
                     column <- c(column, setMissing(out, int32_missval))
                  }
                  table <- add_column(table, paste(name, elem+1, sep=''), column)
               }
            }
            else if (t == 'int64')
            {
               column <- c()
               for (row in 0:(nRows-1)) {
                  out <- sw_table$getInt64ValueAsString(row, col)
                  swat::errorcheck(sw_table)
                  if ( identical(out, numeric(0)) )
                      out <- 0
                  column <- c(column, swat.as.integer64(setMissing(out, int64_missval)))
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'int64-array')
            {
               for (elem in 0:(len-1))
               {
                  column <- c()
                  for (row in 0:(nRows-1)) {
                     out <- sw_table$getInt64ArrayValueAsString(row, col, elem)
                     swat::errorcheck(sw_table)
                     if ( identical(out, numeric(0)) )
                         out <- 0
                     column <- c(column, swat.as.integer64(setMissing(out, int64_missval)))
                  }
                  table <- add_column(table, paste(name, elem+1, sep=''), column)
               }
            }
            else if (t == 'double')
            {
               for (row in 0:(nRows-1))
               {
                  out <- sw_table$getDoubleValue(row, col)
                  swat::errorcheck(sw_table)
                  if ( identical(out, numeric(0)) )
                      out <- 0
                  column <- c(column, out)
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'double-array')
            {
               for (elem in 0:(len-1))
               {
                  column <- c()
                  for (row in 0:(nRows-1))
                  {
                     out <- sw_table$getDoubleArrayValue(row, col, elem)
                     swat::errorcheck(sw_table)
                     if ( identical(out, numeric(0)) )
                         out <- 0
                     column <- c(column, out)
                  }
                  table <- add_column(table, paste(name, elem+1, sep=''), column)
               }
            }
            else if (t == 'char')
            {
               for (row in 0:(nRows-1)) {
                  strval <- sw_table$getStringValue(row, col)
                  swat::errorcheck(sw_table)
                  if ( is.null(strval) ) { strval <- "" }
                  column <- c(column, strval)
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'varchar')
            {
               for (row in 0:(nRows-1)) {
                  strval <- sw_table$getStringValue(row, col)
                  swat::errorcheck(sw_table)
                  if ( is.null(strval) ) { strval <- "" }
                  column <- c(column, strval)
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'datetime')
            {
               column <- list()
               for (row in 0:(nRows-1)) {
                  value <- setMissing(sw_table$getDatetimeValueAsString(row, col), int64_missval)
                  swat::errorcheck(sw_table)
                  if ( is.na(value) ) {
                     column[[length(column) + 1]] <- value
                  } else {
                     column[[length(column) + 1]] <- cas2rPOSIXct(value)
                  }
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'date')
            {
               column <- list()
               for (row in 0:(nRows-1)) {
                  value <- setMissing(sw_table$getDateValue(row, col), int32_missval)
                  swat::errorcheck(sw_table)
                  if ( is.na(value) ) {
                     column[[length(column) + 1]] <- value
                  } else {
                     column[[length(column) + 1]] <- cas2rDate(value)
                  }
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'time')
            {
               column <- list()
               for (row in 0:(nRows-1)) {
                  value <- setMissing(sw_table$getTimeValueAsString(row, col), int64_missval)
                  swat::errorcheck(sw_table)
                  if ( is.na(value) ) {
                     column[[length(column) + 1]] <- value
                  } else {
                     column[[length(column) + 1]] <- cas2rPOSIXct(value)
                  }
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'binary')
            {
               for (row in 0:(nRows-1)) {
                  strval <- sw_table$getBinaryBase64Value(row, col)
                  swat::errorcheck(sw_table)
                  if ( is.null(strval) ) { strval <- "" }
                  column <- c(column, strval)
               }
               table <- add_column(table, name, column)
            }
            else if (t == 'varbinary')
            {
               for (row in 0:(nRows-1)) {
                  strval <- sw_table$getBinaryBase64Value(row, col)
                  swat::errorcheck(sw_table)
                  if ( is.null(strval) ) { strval <- "" }
                  column <- c(column, strval)
               }
               table <- add_column(table, name, column)
            }
            else
            {
               for (row in 0:(nRows-1)) {
                  column <- c(column, NULL)
               }
               table <- add_column(table, name, column)
            }
         }
      }

      # TODO: name, label, attrs and colinfo needs to be added to the
      #       output data.frame
      return( as.casDataFrame(table, name=name, label=label, title=title, attrs=attrs,
                                     col.labels=col.labels, col.formats=col.formats,
                                     col.attrs=col.attrs, col.sizes=col.sizes,
                                     col.types=col.types, col.widths=col.widths) )
   }
   else if ( t == "list" )
      {
      len <- sw_value$getListNItems()
      swat::errorcheck(sw_value)
      haskeys <- sw_value$hasKeys()
      swat::errorcheck(sw_value)
      output <- list()

      if (len < 1)
         return (NULL)

      if ( haskeys == 0 )
         {
         idx <- 1
         for ( i in 1:len )
            {
            sw_item <- sw_value$getListItem(i-1)
            swat::errorcheck(sw_value)
            val = cas2r(sw_item)

            if (!is.null(val))
               {
               output[[idx]] <- val
               idx <- idx + 1
               }
            }
         }
      else
         {
         for ( i in 0:(len-1) )
            {
            sw_item <- sw_value$getListItem(i)
            swat::errorcheck(sw_value)
            key <- sw_item$getKey()
            swat::errorcheck(sw_item)
            val = cas2r(sw_item)
            if (!is.null(val))
               output[[key]] <- val
            }
         }
      return (output)
      }
   else
      {
      return ("Invalid type")
      }
}

setListValue <- function(sw_values, i, key, value) {
   if ( key == 'datamsghandler' )
   {
      return (i)
   }
   t <- class(value)
   if (t == "list" || length(value) > 1)
   {
      sw_sublist <- sw_values$createListAt(i, key, length(value))
      swat::errorcheck(sw_values)
      for ( j in 1:length(value) )
      {
         if ( is.null(names(value[j])) || nchar(names(value[j])) == 0 )
         {
            setListValue(sw_sublist, j-1, '', value[[j]])
         }
         else
         {
            setListValue(sw_sublist, j-1, names(value[j]), value[[j]])
         }
      }
      return (i + 1)
   }
   else if (t == "logical")
   {
      sw_values$setBoolean(i, key, value[[1]])
      swat::errorcheck(sw_values)
      return (i + 1)
   }
   else if (t == "integer")
   {
      sw_values$setInt64(i, key, value[[1]])
      swat::errorcheck(sw_values)
      return (i + 1)
   }
   else if (t == "numeric")
   {
      sw_values$setDouble(i, key, value[[1]])
      swat::errorcheck(sw_values)
      return (i + 1)
   }
   else if (t == "character" || t == "factor")
   {
      sw_values$setString(i, key, value[[1]])
      swat::errorcheck(sw_values)
      return (i + 1)
   }
   else
   {
      sw_values$setNil(i, key)
      swat::errorcheck(sw_values)
      return (i + 1)
   }
   return (i)
}

r2cas <- function(soptions, sw_error, a) {
   len <- length(a)
   if ( !is.null(a$datamsghandler) )
   {
      len <- len - 1
   }
   sw_values <- SW_CASValueList(len, soptions, sw_error)
   if ( len > 0 )
   {
      i <- 0
      for ( j in 1:length(a) )
      {
         if ( is.null(names(a[j])) || nchar(names(a[j])) == 0 )
         {
            i <- setListValue(sw_values, i, '', a[[j]])
         }
         else
         {
            i <- setListValue(sw_values, i, names(a[j]), a[[j]])
         }
      }
   }
   return(sw_values)
}

casvaluelist2r <- function(sw_values, len) {
   num <- 1
   output <- list()
   for ( i in 0:(len-1) )
   {
       sw_item <- sw_values$getItem(i)
       swat::errorcheck(sw_values)
       key <- sw_item$getKey()
       swat::errorcheck(sw_item)
       if ( is.null(key) || nchar(key) == 0 )
       {
          output[[num]] <- cas2r(sw_item)
          num <- num + 1
       }
       else
       {
          output[[key]] <- cas2r(sw_item)
       }
   }
   return (output)
}
