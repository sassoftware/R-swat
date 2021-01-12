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

options(cas.print.messages=FALSE)


context("test.read_write.R")

verify_xlsx_pkg <- function() {
  tryCatch({ 
    library(xlsx) 
  }, error=function (e) { 
    skip('The xlsx package can not be loaded.') 
  })
}

test.data <- function(data) {
  return(file.path(find.package('swat')[[1]], 'tests', 'data', data))
}

# Read CSV files
test_that("Read CSV files", {
  class.csv <- test.data('class.csv')
  
  from_df  <- as.casTable(caz, titanic, casOut=list(name='from_df', replace=TRUE))
  from_csv <- cas.read.csv(caz, file = titanic.csv, casOut = list(name='from_csv'), row.names = 1)
  expect_equivalent(head(from_df), head(from_csv))
  expect_error(read.csv.cas(caz, file = '', tablename = 'from_csv'))
  expect_error(read.csv.cas(caz))
  c1 <- cas.read.csv(caz, class.csv)
  c2 <- cas.read.csv(caz, class.csv, casOut=list(name="class2", replace=TRUE))
  expect_equivalent(c1, c2)
  # tests for issue 2
  c3 <- cas.read.csv(caz, class.csv, casOut="class3")
})

# Read table files
test_that("Read Table files", {
  missing_vals.txt <- test.data('missing_vals.txt')
  
  from_df  <- as.casTable(caz, titanic, casOut=list(name='from_df', replace=TRUE))
  from_csv <- cas.read.csv(caz, file=titanic.csv, casOut = list(name='from_csv', replace=TRUE), row.names = 1)
  expect_equivalent(from_df, from_csv)
  expect_error(read.csv.cas(caz, file = '', tablename = 'from_csv'))
  expect_error(read.csv.cas(caz, file = '', casOut = list(name = 'from_csv')))
  expect_error(cas.read.csv(caz, file = '', casOut = 'from_csv'))
  expect_error(read.csv.cas(caz))
  
  nastr=c("NA", "NaN")
  import_DF0_skipnul <- read.table(missing_vals.txt, row.names = NULL, header=TRUE, na.strings = nastr)
  import_DF0_skipnul.cas <- cas.read.table(caz, missing_vals.txt, row.names = NULL, header=TRUE, na.strings = nastr)
  expect_equivalent(as.casTable(caz, import_DF0_skipnul, casOut=list(replace=TRUE)), import_DF0_skipnul.cas)
})


# Read Excel Files
test_that("Read xlsx files", {
  verify_xlsx_pkg()

  excel_test.xlsx <- test.data('excel_test.xlsx')

  from_xlsx <- cas.read.xlsx(caz, file = excel_test.xlsx, sheetName = 'Sheet1')
  expect_that(from_xlsx, is_a("CASTable"))
})

# Generic Read Function
test_that("Generic Read Function", {
  from_csv <- cas.read.csv(caz, file = titanic.csv, casOut = list(name= 'from_csv', replace=TRUE), header = TRUE)
  from_generic_csv <- cas.read.table(caz, file = titanic.csv, casOut = list(name = 'from_generic_csv', replace=TRUE), header = TRUE, sep = ",")
  expect_equivalent(from_generic_csv, from_csv)
  
  # test for issue 2
  from_generic_csv2 <- cas.read.table(caz, file = titanic.csv, casOut = 'from_generic_csv2', header = TRUE, sep = ",")
  expect_equivalent(from_generic_csv2, from_csv)
})

# Write CSV files
test_that("Write CSV files", {
  cas.write.csv <- file.path(tempdir(), 'cas.write.csv')
  
  # write out the contents of t to a file.
  cas.write.csv(t, cas.write.csv)
  expect_true(file.exists(cas.write.csv))
  
  # read in the csv file and compare it to the standard
  # This is currently not working as expected.
  #expect_output_file(cat(readLines(cas.write.csv)), titanic.csv)
})

# Download an in-memory table from the CAS server
test_that("Test that an in-memory can be downloaded and used within R", {
  cas.write.txt <- file.path(tempdir(), 'cas.write.txt')
  
  cas.write.table(ct, cas.write.txt)
  expect_true(file.exists(cas.write.txt))
})

# Read sas7bdat files
test_that("Read sas7bdat files", {
  census2.sas7bdat <- test.data('census2.sas7bdat')

  t1 <- cas.read.sas7bdat(caz, census2.sas7bdat)
  t2 <- cas.read.sas7bdat(caz, census2.sas7bdat, casOut = list(name = 't2', replace=TRUE))
  expect_equivalent(t1, t2)
  expect_that(t2, is_a("CASTable"))
  
  # test for issue 2
  st3 <- cas.read.sas7bdat(caz, census2.sas7bdat, casOut = 'st3')
  expect_equivalent(t1, st3)
})


# Read jmp files
test_that("Read jmp files", {
  class.jmp <- test.data('class.jmp')

  t1 <- cas.read.jmp(caz, class.jmp, casOut = list(name = 'class', replace=TRUE))
  t2 <- cas.read.jmp(caz, class.jmp, casOut = list(name = 't2', replace=TRUE))
  expect_equivalent(t1, t2)
  expect_that(t2, is_a("CASTable"))
  
  # test for issue 2
  jt3 <- cas.read.jmp(caz, class.jmp, casOut = 'jt3')
  expect_equivalent(t1, jt3)
})

# Read RDS files
test_that("Read RDS files", {
  class.rds <- test.data('class.rds')
  class.csv <- test.data('class.csv')

  from_rds <- cas.readRDS(caz, class.rds, casOut = list(name = 'from_rds'))
  from_csv <- cas.read.csv(caz, class.csv, casOut = list(name = 'from_csv', replace=TRUE))
  expect_equivalent(from_rds, from_csv)
  expect_that(from_rds, is_a("CASTable"))
})

# Write RDS Files
test_that("Write RDS files", {
  class.rds <- test.data('class.rds')

  from_rds <- cas.readRDS(caz, class.rds, casOut=list(name='from_rds', replace=TRUE))
  cas.saveRDS(from_rds, file="casSaveRDS.rds")
  cas.saveRDS(from_rds)
  expect_silent(cas.saveRDS(from_rds))
})


# Replace in-memory tables
test_that("Replace in-memory table (titanic)", {
  from_csv <- cas.read.csv(caz, file = titanic.csv, casOut = list(name = 'from_csv', replace=TRUE))

  ti <- cas.table.tableInfo(caz, table='from_csv')$TableInfo
  create_time <- ti[ ti$Name=='FROM_CSV', ]$CreateTime
  expect_true(create_time > 0)

  replace_it <-  cas.read.csv(caz, file = titanic.csv, casOut = list(name = 'from_csv', replace=TRUE))

  ti <- cas.table.tableInfo(caz, table='from_csv')$TableInfo
  create_time_2 <- ti[ ti$Name=='FROM_CSV', ]$CreateTime
  expect_true(create_time_2 > create_time)
})



# Read txt file
test_that("Read txt files", {
  effort.txt <- test.data('effort.txt')
  
  import_effort_txt <- read.table(effort.txt, header=TRUE)
  import_effort_txt.cas <- cas.read.table(caz, effort.txt, header = TRUE)
  expect_that(import_effort_txt.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_txt), dim(import_effort_txt.cas))
  skip("no support for BIGINT")
  expect_equivalent(summary(import_effort_txt), summary(import_effort_txt.cas))
})


# Read txt file
test_that("Read txt files", {
  effort.txt <- test.data('effort.txt')

  import_effort_txt <- read.table(effort.txt, header=TRUE)
  import_effort_txt.cas <- cas.read.table(caz, effort.txt, header=TRUE, casOut=list(replace=TRUE))
  expect_that(import_effort_txt.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_txt), dim(import_effort_txt.cas))
})

# read .txt file with na.strings
test_that("Read web dat file", {
  missing_vals.txt <- test.data('missing_vals.txt')

  nastr=c("NA", "NaN")
  import_DF0_skipnul <- read.table(missing_vals.txt, 
                                   row.names = NULL, header=TRUE, na.strings = nastr)
  import_DF0_skipnul.cas <- cas.read.table(caz, missing_vals.txt, 
                                           row.names = NULL, header=TRUE, na.strings = nastr,
                                           casOut=list(replace=TRUE))
  import_DF0_skipnul.rdf<-as.data.frame(import_DF0_skipnul)
  import_DF0_skipnul.cdf <-to.casDataFrame(import_DF0_skipnul.cas)
  import_DF0_skipnul.cas2rdf=to.data.frame(import_DF0_skipnul.cdf)
  
  expect_equivalent(import_DF0_skipnul.cas2rdf[1:4], import_DF0_skipnul.rdf[1:4])
  expect_true("" %in% import_DF0_skipnul.cas2rdf$l)
})



# Do not replace in-memory tables, when requested
test_that("Do not replace an in-memory table (titanic)", {
  from_csv <- cas.read.csv(caz, file = titanic.csv,
                           casOut = list(name = 'from_csv', replace=TRUE))

  ti <- cas.table.tableInfo(caz, table='from_csv')$TableInfo
  expect_equivalent(nrow(ti[ ti$Name=='FROM_CSV', ]), 1)

  expect_error(cas.read.csv(caz, file = titanic.csv, casOut = list(name = 'from_csv',
                 replace = FALSE)))
})


# Read comma separated csv files
test_that("Read comma separated csv files", {
  hr_comma_sep.csv <- test.data('hr_comma_sep.csv')
  hr_semicolon_sep.csv <- test.data('hr_semicolon_sep.csv')

  import_hr_csv <- read.csv(hr_comma_sep.csv, header = TRUE, sep = ",", quote = "\"", dec = ".")
  import_hr_csv.cas <- cas.read.csv(caz, hr_comma_sep.csv, header = TRUE, sep = ",", quote = "\"", dec = ".", casOut=list(replace=TRUE))
  write.csv2(import_hr_csv, hr_semicolon_sep.csv)
  expect_that(import_hr_csv.cas, is_a("CASTable"))
  expect_equivalent(dim(import_hr_csv), dim(import_hr_csv.cas))
})


# Read comma separated csv files
test_that("Read comma separated csv files", {
  hr_comma_sep.csv <- test.data('hr_comma_sep.csv')
  hr_semicolon_sep.csv <- test.data('hr_semicolon_sep.csv')

  import_hr_csv <- read.csv(hr_comma_sep.csv, header = TRUE, sep = ",", quote = "\"", dec = ".")
  import_hr_csv.cas <- cas.read.csv(caz, hr_comma_sep.csv, header = TRUE, sep = ",", quote = "\"", dec = ".", casOut=list(replace=TRUE))
  write.csv2(import_hr_csv, hr_semicolon_sep.csv)
  expect_that(import_hr_csv.cas, is_a("CASTable"))
  expect_equivalent(dim(import_hr_csv), dim(import_hr_csv.cas))
})


# read dml file with skip and col.names
colnames2=c("var1 ", "var 2", "var 3", "var 4")
  effort.txt <- test.data('effort.txt')

test_that("read dml file with skip and col.names", {
  import_effort_chknames <- read.table(effort.txt, skip= 1, col.names=colnames2, check.names = FALSE)
  import_effort_chknames.cas <- cas.read.table(caz, effort.txt, skip= 1, col.names=colnames2, check.names = FALSE, casOut=list(replace=TRUE))
  expect_that(import_effort_chknames.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_chknames), dim(import_effort_chknames.cas))
})


# read dml file with skip, col.names and check.names
colnames=c("var1 ", "var 2", "var 3", "var 4", "var 5")

test_that("read dml file with skip, col.names and check.names", {
  class.dlm <- test.data('class.dlm')

  import_class_colnames <- read.table(class.dlm, skip= 1, col.names=colnames)
  import_class_colnames.cas <- cas.read.table(caz, class.dlm, skip= 1, col.names=colnames, casOut=list(replace=TRUE))
  expect_that(import_class_colnames.cas, is_a("CASTable"))
  expect_equivalent(dim(import_class_colnames), dim(import_class_colnames.cas))
})


# read .dlm file with nrows
test_that("read .dlm file with nrows", {
  class.dlm <- test.data('class.dlm')

  import_class_nrows <- read.table(class.dlm, nrows=10, header=TRUE)
  import_class_nrows.cas <- cas.read.table(caz, class.dlm,  nrows=10, header=TRUE, casOut = list(replace=TRUE))
  expect_that(import_class_nrows.cas, is_a("CASTable"))
  expect_equivalent(dim(import_class_nrows), dim(import_class_nrows.cas))
})


# read xlsx file with keepFormulas
test_that("read xlsx file with keepFormulas", {
  verify_xlsx_pkg()

  excel_test.xlsx <- test.data('excel_test.xlsx')

  import_excel_formulas <- read.xlsx(file=excel_test.xlsx, sheetName="Sheet1", keepFormulas=TRUE)
  import_excel_formulas.cas <- cas.read.xlsx(caz, file=excel_test.xlsx, sheetName="Sheet1", keepFormulas=TRUE, casOut=list(replace=TRUE))
  expect_that(import_excel_formulas.cas, is_a("CASTable"))
  expect_equivalent(dim(import_excel_formulas), dim(import_excel_formulas.cas))
})


#read xlsx file with as.data.frame
test_that("read xlsx file with as.data.frame", {
  verify_xlsx_pkg()

  excel_test.xlsx <- test.data('excel_test.xlsx')

  import_excel_list <- read.xlsx(file=excel_test.xlsx, sheetName="Sheet1", as.data.frame= FALSE)
  import_excel_list.cas <- cas.read.xlsx(caz, file=excel_test.xlsx, sheetName="Sheet1", as.data.frame= FALSE, casOut=list(replace=TRUE))
  expect_that(import_excel_list.cas, is_a("CASTable"))
  expect_equivalent(length(import_excel_list), length(import_excel_list.cas))
})


# write xlsx files with sheet name
test_that("write xlsx files with sheet name", {
  verify_xlsx_pkg()

  effort.txt <- test.data('effort.txt')
  effort_cas.xlsx <- file.path(tempdir(), 'effort_cas.xlsx')

  import_effort_txt.cas <- cas.read.table(caz, effort.txt, header=TRUE, casOut=list(replace=TRUE))
  cas.write.xlsx(import_effort_txt.cas, effort_cas.xlsx, sheetName = "Effort data", col.names = FALSE)
  expect_true(file.exists(effort_cas.xlsx))
})


# read xlsx file with start/endRow
test_that("read xlsx file with start/endRow", {
  verify_xlsx_pkg()

  effort.xlsx <- test.data('effort.xlsx')
  effort.txt <- test.data('effort.txt')
  
  # IF the testFile does not exist, create it.
  if(!file.exists(effort.xlsx)){
    # Create xlsx file from txt file.
    write.xlsx(read.table(effort.txt, header = TRUE), testFile, "Effort data")
  }
  
  import_excel_row <- read.xlsx(file=effort.xlsx, sheetName="Effort data", header=FALSE, startRow=3, endRow= 14)
  import_excel_row.cas <- cas.read.xlsx(caz, file=effort.xlsx, sheetName="Effort data", header=FALSE, startRow=3, endRow= 14, casOut = list(replace=TRUE))
  expect_that(import_excel_row.cas, is_a("CASTable"))
  expect_equivalent(dim(import_excel_row), dim(import_excel_row.cas))
})


# read xlsx file with colIndex
test_that("read xlsx file with colIndex", {
  verify_xlsx_pkg()

  excel_test.xlsx <- test.data('excel_test.xlsx')

  import_excel_test_colsubset <- read.xlsx(excel_test.xlsx, 1, colIndex = 2)
  import_excel_test_colsubset.cas <- cas.read.xlsx(caz, excel_test.xlsx, 1, colIndex = 2, casOut=list(replace=TRUE))
  expect_that(import_excel_test_colsubset.cas, is_a("CASTable"))
  expect_equivalent(dim(import_excel_test_colsubset), dim(import_excel_test_colsubset.cas))
})


# Read semicolon separated csv files with comma as decimal
test_that("Read semicolon separated csv files with comma as decimal", {
  hr_semicolon_sep.csv <- test.data('hr_semicolon_sep.csv')

  import_HRsemi_csv <- read.csv(hr_semicolon_sep.csv, header = TRUE, sep = ";", dec = ",")
  
  import_HRsemi_csv.cas <- cas.read.csv(caz, hr_semicolon_sep.csv, header = TRUE, sep = ";", dec = ",")
  expect_that(import_HRsemi_csv.cas, is_a("CASTable"))
  
  import_HRsemi_csv2.cas <- cas.read.table(caz, hr_semicolon_sep.csv, header = TRUE, sep = ";", dec = ",", casOut=list(replace=TRUE))
  expect_that(import_HRsemi_csv2.cas, is_a("CASTable"))
  
  expect_equivalent(dim(import_HRsemi_csv), dim(import_HRsemi_csv.cas))
  expect_equivalent(import_HRsemi_csv.cas, import_HRsemi_csv2.cas)
})


# Read web dat file
test_that("Read web dat file", {
  import_effort_dat <- read.table("https://data.princeton.edu/wws509/datasets/effort.dat", header=TRUE)
  import_effort_dat.cas <- cas.read.table(caz, "https://data.princeton.edu/wws509/datasets/effort.dat", header=TRUE, casOut = list(replace=TRUE))
  expect_that(import_effort_dat.cas, is_a("CASTable"))
  expect_equivalent(dim(import_effort_dat), dim(import_effort_dat.cas))
})


test_that("Tear Down", {
    files <- list("cas.write.csv", "cas.write.txt")
    for (f in files) {
        f <- file.path(tempdir(), f)
        if (file.exists(f)) {
            file.remove(f)
        }
    }
})
