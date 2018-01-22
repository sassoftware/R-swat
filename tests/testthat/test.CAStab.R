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



context("test.CAStab.R")

test_that("delete column", {
  ctt <- ct
  ctt[1]  <- NULL
  expect_equivalent(ctt,ct[-1])
  ctt$n3 <- NULL
  ctt$d <- NULL
  expect_equivalent(ctt,ct[c(-1, -3, -6)])
  ctt[1:2] <- NULL
  expect_equivalent(ctt,ct$s)
})

test_that("row index where", {
  expect_equivalent(as.casTable(caz, df[df$n4 > 15 & df$n1 < 6 , c(1, 4, 5)]), ct[ct$n4 > 15 & ct$n1 < 6 , c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[df$n4 > 15 & df[1] < 6 , c(1, 4, 5)]), ct[ct$n4 > 15 & ct[1] < 6 , c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[df$n4 > 15 | df[1] < 6 , c(1, 4, 5)]), ct[ct$n4 > 15 | ct[1] < 6 , c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[!df$n4 > 15 | df[1] < 6 , c(1, 4, 5)]), ct[!ct$n4 > 15 | ct[1] < 6 , c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[!df$n4 > 15 | df[1] < 6 ,]), ct[!ct$n4 > 15 | ct[1] < 6 ,])
  expect_equivalent(as.casTable(caz, df[!df$n4 > 15 | !df[1] < 6 ,]), ct[!ct$n4 > 15 | !ct[1] < 6 ,])
  
  expect_equivalent(as.casTable(caz, df[, c(1, 4, 5)]), ct[, c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[c(1, 4, 5)]), ct[c("n1", "n4", "s")])
  expect_equivalent(ct[c(1, 4, 5)], ct[c("n1", "n4", "s")])
  # There are issues with the return type.
  #expect_that(class(ct[ct$n4 > 15 & ct$n1 < 6 , c("n1", "n4", "s")]), is_a("CASTable"))
  #expect_that(class(ct[ct$n4 > 15 & ct$n1 < 6 , c("n1", "n4", "s")]), is_a("character"))
})


test_that("drop columns by column number", {
  
  myDF <- df
  myCT <- ct
  
  #drop number column
  myDF[2] <- NULL
  myCT[2] <- NULL
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
  #drop character column
  myDF[4] <- NULL
  myCT[4] <- NULL
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
  #drop date column
  myDF[4] <- NULL
  myCT[4] <- NULL
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
})

test_that("drop columns by column name", {
  
  myDF <- df
  myCT <- ct
  
  #drop number column
  myDF$n2 <- NULL
  myCT$n2 <- NULL
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
  #drop character column
  myDF$s <- NULL
  myCT$s <- NULL
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
  #drop date column
  myDF$d <- NULL
  myCT$d <- NULL
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
})

test_that("drop multiple columns", {
  
  myDF <- df
  myCT <- ct
  
  myDF[3:6] <- list(NULL)
  myCT[3:6] <- list(NULL)
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
})

test_that("drop columns by number w/missing data", {
  
  myDF0 <- df0
  myCT0 <- ct0
  
  #drop number column
  myDF0[2] <- NULL
  myCT0[2] <- NULL
  expect_equivalent(as.casTable(caz, myDF0, casOut = list(replace = TRUE)), myCT0)
  #drop character column
  myDF0[4] <- NULL
  myCT0[4] <- NULL
  expect_equivalent(as.casTable(caz, myDF0, casOut = list(replace = TRUE)), myCT0)
  #drop date column
  myDF0[4] <- NULL
  myCT0[4] <- NULL
  expect_equivalent(as.casTable(caz, myDF0, casOut = list(replace = TRUE)), myCT0)
})

test_that("drop by column name, missing data", {
  
  myDF0 <- df0
  myCT0 <- ct0
  
  #drop number column
  myDF0$n2 <- NULL
  myCT0$n2 <- NULL
  expect_equivalent(as.casTable(caz, myDF0, casOut = list(replace = TRUE)), myCT0)
  #drop character column
  myDF0$s <- NULL
  myCT0$s <- NULL
  expect_equivalent(as.casTable(caz, myDF0, casOut = list(replace = TRUE)), myCT0)
  #drop date column
  myDF0$d <- NULL
  myCT0$d <- NULL
  expect_equivalent(as.casTable(caz, myDF0, casOut = list(replace = TRUE)), myCT0)
})

test_that("drop multiple columns, missing data", {
  
  myDF0 <- df0
  myCT0 <- ct0
  
  myDF0[3:6] <- list(NULL)
  myCT0[3:6] <- list(NULL)
  
  myCT_DF0 <- to.casDataFrame(myCT0)
  myCT_DF0 <- to.data.frame(myCT_DF0)
  expect_equivalent(myDF0, myCT_DF0)
  expect_equivalent(dimnames(myDF0), dimnames(myCT_DF0))
})

###other drop methods
test_that("drop by column number", {
  
  myDF <- df
  myCT <- ct
  
  #drop number column
  myDF <- df[-2]
  myCT <- ct[-2]
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
  #drop character column
  myDF <- df[-4]
  myCT <- ct[-4]
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
  #drop date column
  myDF <- df[-4]
  myCT <- ct[-4]
  expect_equivalent(as.casTable(caz, myDF, casOut = list(replace = TRUE)), myCT)
})

test_that("drop multiple columns", {
  myDF <- df[-(3:6)]
  myCT <- ct[-(3:6)]
  
  myCT_DF <- to.casDataFrame(myCT)
  myCT_DF <- to.data.frame(myCT_DF)
  expect_equivalent(myDF, myCT_DF)
  expect_equivalent(dimnames(myDF), dimnames(myCT_DF))
})

# Create object to reference an existing in-memory table in CAS
test_that("defCasTable", {
  esophct <- as.casTable(caz, esoph, casOut="esophct")
  df_cmpct <- as.casTable(caz, df_cmp, casOut="df_cmpct")
  
  # groupby option
  esophct.grouped <- defCasTable(caz, tablename="esophct", groupby=list("alcgp"))
  expect_silent(esophct.grouped)
  
  df_cmpct.grouped <- defCasTable(caz, tablename="df_cmpct", groupby=list("s"))
  expect_silent(df_cmpct.grouped)
  
  # orderby
  esophct.orderby <- defCasTable(caz, tablename="esophct", orderby=list("ncontrols"))
  esophr.orderby<-esoph[order(esoph$ncontrols),]
  expect_equivalent(esophct.orderby, as.casTable(caz, esophr.orderby))
  
  df_cmpct.orderby <- defCasTable(caz, tablename="df_cmpct", orderby=list("s"))
  df_cmpr.orderby<-df_cmp[order(df_cmp$s),]
  expect_equivalent(df_cmpct.orderby, as.casTable(caz, df_cmpr.orderby))
  
  
  #where option
  esophct.Where <- defCasTable(caz, tablename="esophct", where="agegp='25-34'")
  esophr.where <-esoph[esoph$agegp=='25-34',]
  expect_equivalent(esophct.Where, as.casTable(caz, esophr.where))
  
  df_cmpct.Where <- defCasTable(caz, tablename="df_cmpct", where="cv3<=15")
  df_cmpr.where <-df_cmp[df_cmp$cv3<=15,]
  expect_equivalent(df_cmpct.Where, as.casTable(caz, df_cmpr.where))
  
  
  # columns option
  esophct.cols1 <- defCasTable(caz, tablename="esophct", columns=c("ncases"))
  esophr.cols1 <- esoph[4]
  expect_equivalent(esophct.cols1, as.casTable(caz, esophr.cols1))
  
  df_cmpct.cols1 <- defCasTable(caz, tablename="df_cmpct", columns=c("n1"))
  df_cmpr.cols1 <- df_cmp[1]
  expect_equivalent(df_cmpct.cols1, as.casTable(caz, df_cmpr.cols1))
  
  ## more than on column specified for the columns option
  esophct.cols2 <- defCasTable(caz, tablename="esophct", columns=c("agegp", "alcgp", "tobgp"))
  esophr.cols2 <- esoph[1:3]
  expect_equivalent(esophct.cols2, as.casTable(caz, esophr.cols2))
  
  df_cmpct.cols2 <- defCasTable(caz, tablename="df_cmpct", columns=c("n1", "s", "cv2"))
  df_cmpr.cols2 <- df_cmp[c(1,5,8)]
  expect_equivalent(df_cmpct.cols2, as.casTable(caz, df_cmpr.cols2))
  
})


orig_options <- options()
options(cas.print.messages=TRUE)
test_that("as.castable and dropTable", {
  # Testing that an existing CAS table can't be overwriten by default
  df_cmpct1<-as.casTable(caz, df_cmp, casOut="df_cmpct1")
  expect_message(df_cmpct2<-as.casTable(caz, df, casOut="df_cmpct1"), "already exists")
  
  # Testing that an existing CAS table can't be overwriten with replace=FALSE
  expect_message(as.casTable(caz, df, casOut=list(name="df_cmpct1", replace=FALSE)), "already exists")

  dropTable(df_cmpct1)
  expect_message(df_cmpct1<-as.casTable(caz, df_cmp, casOut=list(name="df_cmpct1", replace=FALSE)), "uploaded file available")

  # Testing that an existing CAS table can be overwriten with replace option
  expect_message(df_cmpct1<-as.casTable(caz, df_cmp, casOut=list(name="df_cmpct1", replace=TRUE)), "uploaded file available")
  


})
options(orig_options)


test_that("to.casDataFrame, as.data.frame, and rownames", {
  df_cmpct<-as.casTable(caz, df_cmp, casOut="df_cmpct")
  df_cmp_cdf<-to.casDataFrame(df_cmpct)
  df_cmp_rdf<-as.data.frame(df_cmp_cdf)
  df_cmp_cdf2<-to.casDataFrame(df_cmpct, obs=3)
  df_cmp_rdf2<-df_cmp_rdf[1:3,]
  rnms<-rownames(df_cmp_cdf)
  rnms2<-rownames(df_cmp_cdf2)
  
  
  # to.casDataFrame w/ obs= option
  expect_equivalent(to.casDataFrame(df_cmpct, obs=3), df_cmp_rdf[1:3,])
  
  # testing that rownames (which just numbers the rows) matches the nrows
  expect_equivalent(as.numeric(tail(rnms, n=1)), nrow(df_cmp_rdf))
  expect_equivalent(as.numeric(tail(rnms2, n=1)), nrow(df_cmp_rdf2))
  
})


