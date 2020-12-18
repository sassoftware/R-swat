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


.casdt_total_seconds <- function(cts) {
  if (class(cts) == "character" && grepl("^-?\\d{9}\\d*$", cts, perl = TRUE)) {
    cts <- as.numeric(gsub("^(-?\\d+)(\\d{6})$", "\\1.\\2", cts, perl = TRUE))
  } else {
    cts <- as.numeric(cts) / 1000000
  }
  return(cts)
}

#
# Convert CAS datetimes to R / SAS datetimes
#

#' Convert CAS timestamp to POSIXlt
#'
#' @param cts Numeric representing a CAS timestamp
#'
#' @return POSIXlt object
#'
#' @export
CASdt.as.POSIXlt <- function(cts) {
  return(as.POSIXlt(cas.as.POSIXct(cts)))
}

#' Convert CAS timestamp to POSIXct
#'
#' @param cts Numeric representing a CAS timestamp
#'
#' @return POSIXct object
#'
#' @export
CASdt.as.POSIXct <- function(cts) {
  return(as.POSIXct(.casdt_total_seconds(cts), origin = "1960-01-01", tz = "UTC"))
}

#' Convert a CAS date to Date
#'
#' @param cdt Numeric representing a CAS date
#'
#' @return Date
#'
#' @export
CASd.as.Date <- function(cdt) {
  return(as.Date(as.POSIXct(as.numeric(cdt) * 60 * 60 * 24, origin = "1960-01-01", tz = "UTC")))
}

#' Convert a CAS time to POSIXct
#'
#' @param ctm Numeric representing a CAS time
#'
#' @return POSIXct object
#'
#' @export
CASt.as.POSIXct <- function(ctm) {
  return(as.POSIXct(.casdt_total_seconds(ctm), origin = "1970-01-01", tz = "UTC"))
}

#' Convert a CAS time to POSIXlt
#'
#' @param ctm Numeric representing a CAS time
#'
#' @return POSIXlt object
#'
#' @export
CASt.as.POSIXlt <- function(ctm) {
  return(as.POSIXlt(CASt.as.POSIXct(ctm)))
}

#' Convert a POSIXlt to a CAS timestamp
#'
#' @param rplt POSIXlt object
#'
#' @return Numeric representing a CAS timestamp
#'
#' @export
POSIXlt.as.CASdt <- function(rplt) {
  return((as.numeric(rplt) + 3653 * 24 * 60 * 60) * 1000000)
}

#' Convert a POSIXct to a CAS timestamp
#'
#' @param rpct POSIXct object
#'
#' @return Numeric representing a CAS timestamp
#'
#' @export
POSIXct.as.CASdt <- function(rpct) {
  return((as.numeric(rpct) + 3653 * 24 * 60 * 60) * 1000000)
}

#' Convert a Date to a CAS date
#'
#' @param rdt Date object
#'
#' @return Numeric representing a CAS date
#'
#' @export
Date.as.CASd <- function(rdt) {
  return(as.numeric(rdt) + 3653)
}

#' Convert a CAS timestamp to a SAS timestamp
#'
#' @param cts Numeric representing a CAS timestamp
#'
#' @return Numeric representing a SAS timestamp
#'
#' @export
CASdt.as.SASdt <- function(cts) {
  return(.casdt_total_seconds(cts))
}

#' Convert a CAS date to a SAS date
#'
#' @param cdt Numeric representing a CAS date
#'
#' @return Numeric representing a SAS date
#'
#' @export
CASd.as.SASd <- function(cdt) {
  return(cdt)
}

#' Convert a CAS time to a SAS time
#'
#' @param ctm Numeric representing a CAS time
#'
#' @return Numeric representing a SAS time
#'
#' @export
CASt.as.SASt <- function(ctm) {
  return(.casdt_total_seconds(ctm))
}

#
# Convert SAS datetimes to R / SAS datetimes
#

#' Convert a SAS timestamp to a POSIXlt
#'
#' @param sts Numeric representing a SAS timestamp
#'
#' @return POSIXlt object
#'
#' @export
SASdt.as.POSIXlt <- function(sts) {
  return(as.POSIXlt(sas.as.POSIXct(sts)))
}

#' Convert a SAS timestamp to a POSIXct
#'
#' @param sts Numeric representing a SAS timestamp
#'
#' @return POSIXct object
#'
#' @export
SASdt.as.POSIXct <- function(sts) {
  return(as.POSIXct(as.numeric(sts), origin = "1960-01-01", tz = "UTC"))
}

#' Convert a SAS date to a Date
#' 
#' @param sdt Numeric representing a SAS date
#'
#' @return Date object
#'
#' @export
SASd.as.Date <- function(sdt) {
  return(as.Date(as.POSIXct(as.numeric(sdt) * 60 * 60 * 24, origin = "1960-01-01", tz = "UTC")))
}

#' Convert a SAS time to a POSIXct
#'
#' @param stm Numeric representing a SAS time
#'
#' @return POSIXct object
#'
#' @export
SASt.as.POSIXct <- function(stm) {
  return(as.POSIXct(as.numeric(stm), origin = "1970-01-01", tz = "UTC"))
}

#' Convert a SAS timem to a POSIXlt
#'
#' @param stm Numeric representing a SAS time
#'
#' @return POSIXlt object
#'
#' @export
SASt.as.POSIXlt <- function(stm) {
  return(as.POSIXlt(SASt.as.POSIXct(stm)))
}

#' Convert a POSIXlt to a SAS timestamp
#'
#' @param rplt POSIXlt object
#'
#' @return Numeric representing a SAS timestamp
#'
#' @export
POSIXlt.as.SASdt <- function(rplt) {
  return(as.numeric(rplt) + 3653 * 24 * 60 * 60)
}

#' Convert a POSIXct to a SAS timestamp
#'
#' @param rpct POSIXct object
#'
#' @return Numeric representing a SAS timestamp
#'
#' @export
POSIXct.as.SASdt <- function(rpct) {
  return(as.numeric(rpct) + 3653 * 24 * 60 * 60)
}

#' Convert a Date to a SAS date
#'
#' @param rdt Date object
#'
#' @return Numeric representing a SAS date
#'
#' @export
Date.as.SASd <- function(rdt) {
  return(as.numeric(rdt) + 3653)
}

#' Convert a SAS timestamp to a CAS timestamp
#'
#' @param sts Numeric representing a SAS timestamp
#'
#' @return Numeric representing a CAS timestamp
#'
#' @export
SASdt.as.CASdt <- function(sts) {
  return(as.numeric(sts) * 1000000)
}

#' Convert a SAS date to a CAS date
#'
#' @param sdt Numeric representing a SAS date
#'
#' @return Numeric representing a CAS date
#'
#' @export
SASd.as.CASd <- function(sdt) {
  return(sdt)
}

#' Convert a SAS time to a CAS time
#'
#' @param stm Numeric representing a SAS time
#'
#' @return Numeric representing a CAS time
#'
#' @export
SASt.as.CASt <- function(stm) {
  return(as.numeric(stm) * 1000000)
}
