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




.casdt.total.seconds <- function ( cts )
{
   if ( class(cts) == 'character' && grepl('^-?\\d{9}\\d*$', cts, perl=TRUE) )
      cts <- as.numeric(gsub('^(-?\\d+)(\\d{6})$', '\\1.\\2', cts, perl=TRUE))
   else
      cts <- as.numeric(cts) / 1000000
   return( cts )
}

#
# Convert CAS datetimes to R / SAS datetimes
#

cas2rPOSIXlt <- function ( cts )
{
   return( as.POSIXlt(cas2rPOSIXct(cts)) )
}

cas2rPOSIXct <- function ( cts )
{
   return( as.POSIXct(.casdt.total.seconds(cts), origin='1960-01-01', tz='UTC') )
}

cas2rDate <- function ( cdt )
{
   return( as.Date(as.POSIXct(as.numeric(cdt) * 60 * 60 * 24, origin='1960-01-01', tz='UTC')) )
}

casTime2rPOSIXct <- function ( ctm )
{
   return( as.POSIXct(.casdt.total.seconds(ctm), origin='1970-01-01', tz='UTC') )
}

casTime2rPOSIXlt <- function ( ctm )
{
   return( as.POSIXlt(casTime2rPOSIXct(ctm)) )
}

rPOSIXlt2cas <- function ( rplt )
{
   return( (as.numeric(rplt) + 3653 * 24 * 60 * 60 ) * 1000000 )
}

rPOSIXct2cas <- function ( rpct )
{
   return( (as.numeric(rpct) + 3653 * 24 * 60 * 60 ) * 1000000 )
}

rDate2cas <- function ( rdt )
{
    return( as.numeric(rdt) + 3653 )
}

cas2sasDateTime <- function ( cts )
{
   return( .casdt.total.seconds(cts) )
}

cas2sasDate <- function ( cdt )
{
   return( cdt )
}

cas2sasTime <- function ( ctm )
{
   return( .casdt.total.seconds(ctm) )
}

#
# Convert SAS datetimes to R / SAS datetimes
#

sas2rPOSIXlt <- function ( sts )
{
   return( as.POSIXlt(sas2rPOSIXct(sts)) )
}

sas2rPOSIXct <- function ( sts )
{
   return( as.POSIXct(as.numeric(sts), origin='1960-01-01', tz='UTC') )
}

sas2rDate <- function ( sdt )
{
   return( as.Date(as.POSIXct(as.numeric(sdt) * 60 * 60 * 24, origin='1960-01-01', tz='UTC')) )
}

sasTime2rPOSIXct <- function ( stm )
{
   return( as.POSIXct(as.numeric(stm), origin='1970-01-01', tz='UTC') )
}

sasTime2rPOSIXlt <- function ( stm )
{
   return( as.POSIXlt(sasTime2rPOSIXct(stm)) )
}

rPOSIXlt2sas <- function ( rplt )
{
   return( as.numeric(rplt) + 3653 * 24 * 60 * 60 )
}

rPOSIXct2sas <- function ( rpct )
{
   return( as.numeric(rpct) + 3653 * 24 * 60 * 60 )
}

rDate2sas <- function ( rdt )
{
    return( as.numeric(rdt) + 3653 )
}

sas2casDateTime <- function ( sts )
{
   return( as.numeric(sts) * 1000000 )
}

sas2casDate <- function ( sdt )
{
   return( sdt )
}

sas2casTime <- function ( stm )
{
   return( as.numeric(stm) * 1000000 )
}
