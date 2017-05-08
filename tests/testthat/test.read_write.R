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



context("test.read_write.R")

# Read CSV files
test_that("Read CSV files", {
  
  from_df  <- as.casTable(caz, titanic, casOut = 'from_df')
  from_csv <- cas.read.csv(caz, file = 'titanic.csv', casOut = list(name='from_csv'), row.names = 1)
  expect_equivalent(head(from_df), head(from_csv))
  expect_error(read.csv.cas(caz, file = '', tablename = 'from_csv'))
  expect_error(read.csv.cas(caz))
  c1 <- cas.read.csv(caz, "../data/class.csv")
  c2 <- cas.read.csv(caz, "../data/class.csv", casOut=list(name="class2", replace=TRUE))
  expect_equivalent(c1, c2)
  # tests for issue 2
  c3 <- cas.read.csv(caz, "../data/class.csv", casOut="class3")
})

# Read table files
test_that("Read Table files", {
  
  from_df  <- as.casTable(caz, titanic, casOut = 'from_df')
  from_csv <- cas.read.csv(caz, file = 'titanic.csv', casOut = list(name='from_csv', replace=TRUE), row.names = 1)
  expect_equivalent(from_df, from_csv)
  expect_error(read.csv.cas(caz, file = '', tablename = 'from_csv'))
  expect_error(read.csv.cas(caz, file = '', casOut = list(name = 'from_csv')))
  expect_error(cas.read.csv(caz, file = '', casOut = 'from_csv'))
  expect_error(read.csv.cas(caz))
  
  nastr=c("NA", "NaN")
  import_DF0_skipnul <- read.table("../data/MissingVals.txt", row.names = NULL, header=TRUE, na.strings = nastr)
  import_DF0_skipnul.cas <- cas.read.table(caz, "../data/MissingVals.txt", row.names = NULL, header=TRUE, na.strings = nastr)
  expect_equivalent(as.casTable(caz, import_DF0_skipnul), import_DF0_skipnul.cas)
})


# Read Excel Files
test_that("Read xlsx files", {
  tryCatch({
    library(xlsx)
  }, error=function (e) {
    skip('The xlsx package can not be loaded.')
  })
  from_xlsx <- cas.read.xlsx(caz, file = '../data/excelTest.xlsx', sheetName = 'Sheet1')
  expect_that(from_xlsx, is_a("CASTable"))
})

# Generic Read Function
test_that("Generic Read Function", {
  
  from_csv <- cas.read.csv(caz, file = 'titanic.csv', casOut = list(name= 'from_csv', replace=TRUE), header = TRUE)
  from_generic_csv <- cas.read.table(caz, file = 'titanic.csv', casOut = list(name = 'from_generic_csv', replace=TRUE), header = TRUE, sep = ",")
  expect_equivalent(from_generic_csv, from_csv)
  
  # test for issue 2
  from_generic_csv2 <- cas.read.table(caz, file = 'titanic.csv', casOut = 'from_generic_csv2', header = TRUE, sep = ",")
  expect_equivalent(from_generic_csv2, from_csv)
})

# Write CSV files
test_that("Write CSV files", {
  
  # write out the contents of t to a file.
  cas.write.csv(t, "cas.write.csv")
  expect_true(file.exists("cas.write.csv"))
  
  # read in the csv file and compare it to the standard
  # This is currently not working as expected.
  #expect_output_file(cat(readLines("cas.write.csv")), "titanic.csv")
})

# Download an in-memory table from the CAS server
test_that("Test that an in-memory can be downloaded and used within R", {
  
  cas.write.table(ct, "cas.write.txt")
  expect_true(file.exists("cas.write.txt"))
  
  #Additional tests should be written to cover options.
  
  # Cleanup
  file.remove("cas.write.txt")
})

# Read sas7bdat files
test_that("Read sas7bdat files", {
  t1 <- cas.read.sas7bdat(caz, "../data/census2.sas7bdat")
  t2 <- cas.read.sas7bdat(caz, "../data/census2.sas7bdat", casOut = list(name = 't2', replace=TRUE))
  expect_equivalent(t1, t2)
  expect_that(t2, is_a("CASTable"))
  
  # test for issue 2
  st3 <- cas.read.sas7bdat(caz, "../data/census2.sas7bdat", casOut = 'st3')
  expect_equivalent(t1, st3)
})


# Read jmp files
test_that("Read jmp files", {
  t1 <- cas.read.jmp(caz, "../data/class.jmp", casOut = list(name = 'class', replace=TRUE))
  t2 <- cas.read.jmp(caz, "../data/class.jmp", casOut = list(name = 't2', replace=TRUE))
  expect_equivalent(t1, t2)
  expect_that(t2, is_a("CASTable"))
  
  # test for issue 2
  jt3 <- cas.read.jmp(caz, "../data/class.jmp", casOut = 'jt3')
  expect_equivalent(t1, jt3)
})

# Read RDS files
test_that("Read RDS files", {
  from_rds <-cas.readRDS(caz, "../data/class.rds", casOut = list(name = 'from_rds'))
  from_csv <-cas.read.csv(caz, "../data/class.csv", casOut = list(name = 'from_csv', replace=TRUE))
  expect_equivalent(from_rds, from_csv)
  expect_that(from_rds, is_a("CASTable"))
})

# Write RDS Files
test_that("Write RDS files", {
  from_rds <-cas.readRDS(caz, "../data/class.rds", casOut = list(name = 'from_rds', casOut=list(replace=TRUE)))
  cas.saveRDS(from_rds, file="casSaveRDS.rds")
  cas.saveRDS(from_rds)
  expect_silent(cas.saveRDS(from_rds))
})


# Replace in-memory tables
test_that("Replace in-memory table (titanic)", {
  from_csv <- cas.read.csv(caz, file = 'titanic.csv', casOut = list(name = 'from_csv'))
  replace_it <-  cas.read.csv(caz, file = 'titanic.csv', casOut = list(name = 'from_csv', replace=TRUE))
  # NOTE: This check only works in the REST interface!!!
  #       This needs to be checked some other way.
  #  expect_match(caz$sw_connection$results_$log, 
  #    "made the uploaded file available"
  #  )
})



# Read txt file
test_that("Read txt files", {
  
  import_effort_txt <- read.table("../data/effort.txt", header=TRUE)
  import_effort_txt.cas <- cas.read.table(caz, "../data/effort.txt", header = TRUE)
  expect_that(import_effort_txt.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_txt), dim(import_effort_txt.cas))
  skip("no support for BIGINT")
  expect_equivalent(summary(import_effort_txt), summary(import_effort_txt.cas))
})


# Read txt file
test_that("Read txt files", {
  import_effort_txt <- read.table("../data/effort.txt", header=TRUE)
  import_effort_txt.cas <- cas.read.table(caz, "../data/effort.txt", header=TRUE)
  expect_that(import_effort_txt.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_txt), dim(import_effort_txt.cas))
})

# read .txt file with na.strings
test_that("Read web dat file", {
  nastr=c("NA", "NaN")
  import_DF0_skipnul <- read.table("../data/MissingVals.txt", 
                                   row.names = NULL, header=TRUE, na.strings = nastr)
  import_DF0_skipnul.cas <- cas.read.table(caz, "../data/MissingVals.txt", 
                                           row.names = NULL, header=TRUE, na.strings = nastr)
  import_DF0_skipnul.rdf<-as.data.frame(import_DF0_skipnul)
  import_DF0_skipnul.cdf <-to.casDataFrame(import_DF0_skipnul.cas)
  import_DF0_skipnul.cas2rdf=to.data.frame(import_DF0_skipnul.cdf)
  
  expect_equivalent(import_DF0_skipnul.cas2rdf[1:4], import_DF0_skipnul.rdf[1:4])
  expect_true("" %in% import_DF0_skipnul.cas2rdf$l)
})



# Do not replace in-memory tables, when requested
test_that("Do not replace an in-memory table (titanic)", {
  from_csv <-
    cas.read.csv(caz, file = 'titanic.csv', casOut = list(name = 'from_csv'))
  do_not_replace_it <-
    cas.read.csv(caz, file = 'titanic.csv', casOut = list(name = 'from_csv',
                 replace = FALSE))
# NOTE: This check only works in the REST interface!!!
#       This needs to be checked some other way.
#  expect_match(caz$sw_connection$results_$log, "already exists")
})


# Read comma separated csv files
test_that("Read comma separated csv files", {
  import_HR_csv <- read.csv("../data/HR_comma_sep.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
  import_HR_csv.cas <- cas.read.csv(caz, "../data/HR_comma_sep.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
  write.csv2(import_HR_csv, "../data/HR_semicolon_sep.csv")
  expect_that(import_HR_csv.cas, is_a("CASTable"))
  expect_equivalent(dim(import_HR_csv), dim(import_HR_csv.cas))
})


# Read comma separated csv files
test_that("Read comma separated csv files", {
  import_HR_csv <- read.csv("../data/HR_comma_sep.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
  import_HR_csv.cas <- cas.read.csv(caz, "../data/HR_comma_sep.csv", header = TRUE, sep = ",", quote = "\"", dec = ".")
  write.csv2(import_HR_csv, "../data/HR_semicolon_sep.csv")
  expect_that(import_HR_csv.cas, is_a("CASTable"))
  expect_equivalent(dim(import_HR_csv), dim(import_HR_csv.cas))
})


# read dml file with skip and col.names
colnames2=c("var1 ", "var 2", "var 3", "var 4")

test_that("read dml file with skip and col.names", {
  import_effort_chknames <- read.table("../data/effort.txt", skip= 1, col.names=colnames2, check.names = FALSE)
  import_effort_chknames.cas <- cas.read.table(caz, "../data/effort.txt", skip= 1, col.names=colnames2, check.names = FALSE)
  expect_that(import_effort_chknames.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_chknames), dim(import_effort_chknames.cas))
})


# read dml file with skip, col.names and check.names
colnames=c("var1 ", "var 2", "var 3", "var 4", "var 5")

test_that("read dml file with skip, col.names and check.names", {
  import_class_colnames <- read.table("../data/class.dlm", skip= 1, col.names=colnames)
  import_class_colnames.cas <- cas.read.table(caz, "../data/class.dlm", skip= 1, col.names=colnames)
  expect_that(import_class_colnames.cas, is_a("CASTable"))
  expect_equivalent(dim(import_class_colnames), dim(import_class_colnames.cas))
})


# read .dlm file with nrows
test_that("read .dlm file with nrows", {
  import_class_nrows <- read.table("../data/class.dlm", nrows=10, header=TRUE)
  import_class_nrows.cas <- cas.read.table(caz, "../data/class.dlm",  nrows=10, header=TRUE, casOut = list(replace=TRUE))
  expect_that(import_class_nrows.cas, is_a("CASTable"))
  expect_equivalent(dim(import_class_nrows), dim(import_class_nrows.cas))
})


# read xlsx file with keepFormulas
test_that("read xlsx file with keepFormulas", {
  import_excel_formulas <- read.xlsx(file="../data/excelTest.xlsx", sheetName="Sheet1", keepFormulas=TRUE)
  import_excel_formulas.cas <- cas.read.xlsx(caz, file="../data/excelTest.xlsx", sheetName="Sheet1", keepFormulas=TRUE, casOut=list(replace=TRUE))
  expect_that(import_excel_formulas.cas, is_a("CASTable"))
  expect_equivalent(dim(import_excel_formulas), dim(import_excel_formulas.cas))
})


#read xlsx file with as.data.frame
test_that("read xlsx file with as.data.frame", {
  import_excel_list <- read.xlsx(file="../data/excelTest.xlsx", sheetName="Sheet1", as.data.frame= FALSE)
  import_excel_list.cas <- cas.read.xlsx(caz, file="../data/excelTest.xlsx", sheetName="Sheet1", as.data.frame= FALSE, casOut=list(replace=TRUE))
  expect_that(import_excel_list.cas, is_a("CASTable"))
  expect_equivalent(length(import_excel_list), length(import_excel_list.cas))
})


# write xlsx files with sheet name
test_that("write xlsx files with sheet name", {
  import_effort_txt.cas <- cas.read.table(caz, "../data/effort.txt", header=TRUE)
  cas.write.xlsx(import_effort_txt.cas, "../data/effort_CAS.xlsx", sheetName = "Effort data", col.names = FALSE)
  expect_true(file.exists("../data/effort_CAS.xlsx"))
})


# read xlsx file with start/endRow
test_that("read xlsx file with start/endRow", {
  testFile <- "../data/effort.xlsx"
  
  # IF the testFile does not exist, create it.
  if(!file.exists(testFile)){
    # Create xlsx file from txt file.
    write.xlsx(read.table("../data/effort.txt", header = TRUE), testFile, "Effort data")
  }
  
  import_excel_row <- read.xlsx(file=testFile, sheetName="Effort data", header=FALSE, startRow=3, endRow= 14)
  import_excel_row.cas <- cas.read.xlsx(caz, file=testFile, sheetName="Effort data", header=FALSE, startRow=3, endRow= 14, casOut = list(replace=TRUE))
  expect_that(import_excel_row.cas, is_a("CASTable"))
  expect_equivalent(dim(import_excel_row), dim(import_excel_row.cas))
})


# read xlsx file with colIndex
test_that("read xlsx file with colIndex", {
  import_excelTest_colsubset <- read.xlsx("../data/excelTest.xlsx", 1, colIndex = 2)
  import_excelTest_colsubset.cas <- cas.read.xlsx(caz, "../data/excelTest.xlsx", 1, colIndex = 2, casOut=list(replace=TRUE))
  expect_that(import_excelTest_colsubset.cas, is_a("CASTable"))
  expect_equivalent(dim(import_excelTest_colsubset), dim(import_excelTest_colsubset.cas))
})


# Read semicolon separated csv files with comma as decimal
test_that("Read semicolon separated csv files with comma as decimal", {
  import_HRsemi_csv <- read.csv("../data/HR_semicolon_sep.csv", header = TRUE, sep = ";", dec = ",")
  
  import_HRsemi_csv.cas <- cas.read.csv(caz, "../data/HR_semicolon_sep.csv", header = TRUE, sep = ";", dec = ",")
  expect_that(import_HRsemi_csv.cas, is_a("CASTable"))
  
  import_HRsemi_csv2.cas <- cas.read.table(caz, "../data/HR_semicolon_sep.csv", header = TRUE, sep = ";", dec = ",", casOut=list(replace=TRUE))
  expect_that(import_HRsemi_csv2.cas, is_a("CASTable"))
  
  expect_equivalent(dim(import_HRsemi_csv), dim(import_HRsemi_csv.cas))
  expect_equivalent(import_HRsemi_csv.cas, import_HRsemi_csv2.cas)
})


# Read web dat file
test_that("Read web dat file", {
  import_effort_dat <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat", header=TRUE)
  import_effort_dat.cas <- cas.read.table(caz, "http://data.princeton.edu/wws509/datasets/effort.dat", header=TRUE, casOut = list(replace=TRUE))
  expect_that(import_effort_dat.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_dat), dim(import_effort_dat.cas))
})


test_that("Tear Down", {
    file.remove("cas.write.csv", "titanic.csv", "from_rds.rds", "casSaveRDS.rds")
})
