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

library(swat)
library(testthat)

options(cas.gen.function.sig=FALSE, cas.print.messages=FALSE)

#
# Helper package to seed data for tests
#

#
# Setup connection parameters
#

# Fails in Jenkins due to older version of R not supporting withr:defer
# Uncomment when running tests outside of Jenkins
#withr::defer(swat::cas.terminate(caz), teardown_env())

info <- swat::.getConnectionInfo(NULL, NULL, NULL, NULL, NULL, NULL)
HOSTNAME <- info$hostname
PORT <- info$port
USERNAME <- info$username
PASSWORD <- info$password
PROTOCOL <- info$protocol
PATH <- ''

if ( grepl('^https?:', HOSTNAME) ) {
    PURL <- httr::parse_url(HOSTNAME)
    PATH <- PURL$path
    HOSTNAME <- PURL$hostname
}
if ( HOSTNAME == '' ) { HOSTNAME = NULL }
if ( PORT == '' ) { PORT <- NULL }
if ( !is.null(PORT) ) { PORT <- as.numeric(PORT) }

# Check for .casrc file
if ( is.null(HOSTNAME) ) 
{
  casrc <- '.casrc'
  if ( !file.exists('.casrc') )
    casrc <- file.path(path.expand('~'), '.casrc')
  
  if ( file.exists(casrc) ) 
  {
    casrc <- readLines(casrc)
    for (i in 1:length(casrc))
    {
      if ( grepl('^\\s*cashost\\s*=', casrc[[i]], perl=TRUE) )
        HOSTNAME <- gsub('(^\\s*cashost\\s*=\\s*["\']|["\']\\s*$)', '', casrc[[i]])
      else if ( grepl('^\\s*casport\\s*=', casrc[[i]], perl=TRUE) )
        PORT <- as.numeric(gsub('(^\\s*casport\\s*=\\s*)', '', casrc[[i]]))
      else if ( grepl('^\\s*casprotocol\\s*=', casrc[[i]], perl=TRUE) )
        PROTOCOL <- gsub('(^\\s*casprotocol\\s*=\\s*["\']|["\']\\s*$)', '', casrc[[i]])
    }
  }
}

message(cat('NOTE: Using HOSTNAME=', HOSTNAME, ' PORT=', PORT, ' PROTOCOL=', PROTOCOL, ' PATH=', PATH, sep=''))

# Create CAS connection
caz <- swat::CAS(HOSTNAME, PORT, USERNAME, PASSWORD, protocol=PROTOCOL, path=PATH)

#
# Create test data
#

n1 <- c(2, 3, 5, 5, 5, 598)
n2 <- c(5, 6, 7, 7, 8, 120)
n3 <- c(12, 13, 15, 15, 8, 198)
n4 <- c(15, 16, 17,  15, 8, 1120)
n5 = c(1.2, 1.3, 1.5, 1.5, 0.8, 19.8)
s <- c("aa", "bb", "cc", "dd", "dd", "dd")
d <- c('12/31/09', '09/08/78', '12/25/00','04/01/16', '06/17/80', '03/08/02')
d <- as.Date(d, "%m/%d/%y")
df <- data.frame(n1, n2, n3, n4, s, d)
df_ <- data.frame(n1, n2, n3, n4, n5, s, d)

# Create data frame with missing values and invalid SAS column names
n1.1 <- c(2, 3, 5, NA, 5, 598)
n2.1 <- c(5, 6, 7, 7, NA, 120)
n3.1 <- c(12, 13, 15, 15, 13, 198)
n4.1 <- c(NA, 16, 17,  15, 8, 1120)
n5.1 = c(1.2, 1.3, 1.5, 1.5, 0.8, NA)
s.1 <- c("aa", "bb", "cc", "dd", "dd", "dd")
s0.1 <- c("aa", "bb", "cc", "dd", "dd", " ")
d.1 <- c('12/31/09', '09/08/78', '12/25/00','04/01/16', NA, '03/08/02')
d.1 <- as.Date(d, "%m/%d/%y")
df0.1 <- data.frame(n1.1, n2.1, n3.1, n4.1, s.1, d.1)
df0_.1 <- data.frame(n1.1, n2.1, n3.1, n4.1, s0.1, d.1)

# Create data frame with missing values
n1 <- c(2, 3, 5, NA, 5, 598)
n2 <- c(5, 6, 7, 7, NA, 120)
n3 <- c(12, 13, 15, 15, 13, 198)
n4 <- c(NA, 16, 17,  15, 8, 1120)
n5 = c(1.2, 1.3, 1.5, 1.5, 0.8, NA)
s <- c("aa", "bb", "cc", "dd", "dd", "dd")
s0 <- c("aa", "bb", "cc", "dd", "dd", " ")
d <- c('12/31/09', '09/08/78', '12/25/00','04/01/16', NA, '03/08/02')
d <- as.Date(d, "%m/%d/%y")
df0 <- data.frame(n1, n2, n3, n4, s, d)
df0_ <- data.frame(n1, n2, n3, n4, n5, s0, d)

# data.frame with only numeric data
dfn <- data.frame(n1, n2, n3, n4)

df1 <- data.frame(matrix(rnorm(20), 10))
df2 <- data.frame(matrix(rnorm(20), 10))
df3 <- data.frame(matrix(rnorm(20), 10))
names(df3) <- c("V3", "V4")

# Load data to CAS
ct1 <- as.casTable(caz, df1, casOut=list(replace=TRUE))
ct2 <- as.casTable(caz, df2, casOut=list(replace=TRUE))
ct3 <- as.casTable(caz, df3, casOut=list(replace=TRUE))

ct0.1 <- as.casTable(caz, df0.1, casOut=list(replace=TRUE))
ct0 <- as.casTable(caz, df0, casOut=list(replace=TRUE))
ct  <- as.casTable(caz, df, casOut=list(replace=TRUE))
i2 <- as.casTable(caz, iris, casOut=list(replace=TRUE))
df.ct <- as.casTable(caz, df_, casOut=list(replace=TRUE))
df0.ct <- as.casTable(caz, df0_, casOut=list(replace=TRUE))
df0_1.ct <- as.casTable(caz, df0_.1, casOut=list(replace=TRUE))

ctn  <- as.casTable(caz, dfn, casOut=list(replace=TRUE))

ct_cmp <- as.casTable(caz, df, casOut = list(name='ct_cmp', replace=TRUE))
ct_cmp["cv1"] <- ct_cmp$n1 + 0
ct_cmp["cv2"] <- ct_cmp$n2 + 0
ct_cmp["cv3"] <- ct_cmp$n3 + 0
df_cmp <- to.data.frame(to.casDataFrame(ct_cmp))



titanic <- read.csv('http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv')
t <- as.casTable(caz, titanic, casOut=list(replace=TRUE))

titanic.csv <- tempfile(pattern='titanic_', fileext='.csv')
write.csv(titanic, file = titanic.csv)
mtcars.ct = as.casTable(caz, mtcars, casOut=list(replace=TRUE))
