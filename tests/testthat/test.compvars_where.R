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



context("compvars_where.R")



# Row indexing on character values , =="
test_that("Row indexing on character values, ==", {
  r<-df0_[df0_["s0"] == 'dd', 1:5]
  cas<-df0.ct[df0.ct["s0"] == 'dd', 1:5]
  df0.ct@where="s0 = 'dd'"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0.ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})


# Row indexing on missing character values
test_that("Row indexing on missing character values", {
  r<-df0_[df0_["s0"] == ' ', 1:5]
  cas<-df0.ct[df0.ct["s0"] == ' ', 1:5]
  df0.ct@where="s0 = ' '"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0.ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

# Row indexing on numeric values
test_that("Row indexing on numeric values, ==", {
  r<-df_[df_["n5"] == 1.2, 1:5]
  cas<-df.ct[df.ct["n5"] == 1.2, 1:5]
  df0.ct@where="n5 = 1.2"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0.ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})


# Row indexing on character values, !=
test_that("Row indexing on character values, !=", {
  r<-df0_[df0_["s0"] != 'dd', 1:5]
  cas<-df0.ct[df0.ct["s0"] != 'dd', 1:5]
  df0.ct@where="s0 ^= 'dd'"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0.ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

# Row indexing on numeric values, !=
test_that("Row indexing on numeric values, !=", {
  r<-df_[df_["n2"] != 7, 1:5]
  cas<-df.ct[df.ct["n2"] != 7, 1:5]
  df0.ct@where="n2 ^= '7"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0.ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

# Row indexing with multiple conditions, &
test_that("Row indexing with multiple conditions, &", {
  r<-df_[df_["s"]=="dd" & df_["n4"]<=15, c(1,2)]
  cas<-df.ct[df.ct["s"]=="dd" & df.ct["n4"]<=15, c(1,2)]
  df.ct@where="s='dd' and n4<=15"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0.ct[1:2], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

# Row indexing with multiple conditions, |
test_that("Row indexing with multiple conditions, |", {
  r<-df_[df_["s"]=="dd" | df_["n4"]<=15, c(3,4)]
  cas<-df.ct[df.ct["s"]=="dd" | df.ct["n4"]<=15, c(3,4)]
  df.ct@where="s='dd' | n4<=15"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0.ct[3:4], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

# Row indexing with non-integer division
test_that("Row indexing with non-integer division, !=", {
  r<-df_[df_["n4"]%/% df_["n5"]<= 11, c(4, 5)]
  skip("Row index with non-integer division Failure Issue #117")
  cas<-df.ct[df.ct["n4"]%/% df.ct["n5"]<= 11, c(4, 5)]
  rdf<-as.data.frame(r)
  skip("Row index with non-integer division Failure Issue #117")
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  skip("Row index with non-integer division Failure Issue #117")
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})


# Row indexing with non-integer division
test_that("Row indexing with non-integer division, !=", {
  df_$nonintdiv<-df_["n4"]%/% df_["n5"]
  df.ct$nonintdiv<-df.ct["n4"]%/% df.ct["n5"]
  df.ct$nonintdiv2<-"nonintdiv2 = n4/n5;"
  rdf<-as.data.frame(df_$nonintdiv)
  cdf=to.casDataFrame(df.ct$nonintdiv)
  cdf2=to.casDataFrame(df.ct$nonintdiv2)
  cas2rdf=to.data.frame(cdf)
  skip("Row index with non-integer division Failure Issue #117")
  expect_equivalent(cas2rdf, rdf)
  skip("Row index with non-integer division Failure Issue #117")
  expect_equivalent(cdf, cdf2)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})


# Row indexing with compVars
test_that("Row indexing with compVars", {
  df_$comp1<-df_$n3+df_$n4
  df.ct$comp1<-df.ct$n3+df.ct$n4
  r<-df_[df_["comp1"]<30, c(1,2,3,4,5,8)]
  cas<-df.ct[df.ct["comp1"]<30, c(1,2,3,4,5,8)]
  df.ct@where="comp1<30"
  rdf<-as.data.frame(r)
  cdf=to.casDataFrame(cas)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df.ct[c(1,2,3,4,5,8)], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})


# df0_['cmp1'] <- df0_$n1+df0_$n2
test_that("df0_['cmp1'] <- df0_$n1+df0_$n2", {
  df0_['cmp1'] <- df0_$n1+df0_$n2
  df0.ct['cmp1'] <- "cmp1 = n1+n2;"
  rdf<-as.data.frame(df0_$cmp1)
  cdf=to.casDataFrame(df0.ct$cmp1)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# df0_$cmp2 <- df0_$n3+df0_$n4
test_that("df0_$cmp2 <- df0_$n3+df0_$n4", {
  df0_$cmp2 <- df0_$n3+df0_$n4 
  df0.ct$cmp2 <- "cmp2 = n3+n4"
  rdf<-as.data.frame(df0_$cmp2)
  cdf=to.casDataFrame(df0.ct$cmp2)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
}) 


# df0_['cmp3'] <- df0_$n4 - df0_$n2 * df0_$n1
test_that("df0_['cmp3'] <- df0_$n4 - df0_$n2 * df0_$n1", {
  df0_['cmp3'] <- df0_$n4 - df0_$n2 * df0_$n1 
  df0.ct['cmp3'] <- df0.ct$n4 - df0.ct$n2 * df0.ct$n1 
  rdf<-as.data.frame(df0_$cmp3)
  cdf=to.casDataFrame(df0.ct$cmp3)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# df0_$cmp4 <- df0_$n4 - df0_$n1
test_that("df0_$cmp4 <- df0_$n4 - df0_$n1", {
  df0_$cmp4 <- df0_$n4 - df0_$n1 
  df0.ct$cmp4 <- df0.ct$n4 - df0.ct$n1 
  rdf<-as.data.frame(df0_$cmp4)
  cdf=to.casDataFrame(df0.ct$cmp4)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# df0_$cmp5 <- df0_$n4 - 3
test_that("df0_$cmp5 <- df0_$n4 - 3", {
  df0_$cmp5 <- df0_$n4 - 3 
  df0.ct$cmp5 <- df0.ct$n4 - 3 
  rdf<-as.data.frame(df0_$cmp5)
  cdf=to.casDataFrame(df0.ct$cmp5)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# df0_$cmp6 <- df0_$n4 + 3 - 2
test_that("df0_$cmp6 <- df0_$n4 + 3 - 2", {
  df0_$cmp6 <- df0_$n4 + 3 - 2 
  df0.ct$cmp6 <- df0.ct$n4 + 3 - 2 
  rdf<-as.data.frame(df0_$cmp6)
  cdf=to.casDataFrame(df0.ct$cmp6)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# df0_[8] <-  df0_$n1 / 3 * df0_$n2
test_that("df0_[8] <-  df0_$n1 / 3 * df0_$n2", {
  df0_[8] <-  df0_$n1 / 3 * df0_$n2 
  df0.ct[8] <-  df0.ct$n1 / 3 * df0.ct$n2
  rdf<-as.data.frame(df0_[8])
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
}) 


# df0_[8] <-  df0_$n1 / 3 + df0_$n2
test_that("df0_[8] <-  df0_$n1 / 3 + df0_$n2", {
  df0_[8] <-  df0_$n1 / 3 + df0_$n2 
  df0.ct[8] <-  df0.ct$n1 / 3 + df0.ct$n2
  rdf<-as.data.frame(df0_[8]) 
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})


# df0_[8] <-  df0_$n1 + 3 / df0_$n2
test_that("df0_[8] <-  df0_$n1 + 3 / df0_$n2", {
  df0_[8] <-  df0_$n1 + 3 / df0_$n2 
  df0.ct[8] <-  df0.ct$n1 + 3 / df0.ct$n2
  rdf<-as.data.frame(df0_[8]) 
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf) 
})


# df0_[8] <-  (df0_$n1 + 3) / df0_$n2
test_that("df0_[8] <-  (df0_$n1 + 3) / df0_$n2", {
  df0_[8] <-  (df0_$n1 + 3) / df0_$n2 
  df0.ct[8] <-  (df0.ct$n1 + 3) / df0.ct$n2 
  rdf<-as.data.frame(df0_[8]) 
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf) 
})

# df0_[8] <-  df0_$n1 / 3 + df0_$n2
test_that("df0_[8] <-  df0_$n1 / 3 + df0_$n2", {
  df0_[8] <-  df0_$n1 / 3 + df0_$n2 
  df0.ct[8] <-  df0.ct$n1 / 3 + df0.ct$n2 
  rdf<-as.data.frame(df0_[8]) 
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf) 
})

# df0_[8] <-  df0_$n1 / (3 + df0_$n2)
test_that("df0_[8] <-  df0_$n1 / (3 + df0_$n2)", {
  df0_[8] <-  df0_$n1 / (3 + df0_$n2) 
  df0.ct[8] <-  df0.ct$n1 / (3 + df0.ct$n2) 
  rdf<-as.data.frame(df0_[8]) 
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})


# df0_[8] <-  (df0_$n1 / 3) + df0_$n2
test_that("df0_[8] <-  (df0_$n1 / 3) + df0_$n2", {
  df0_[8] <-  (df0_$n1 / 3) + df0_$n2 
  df0.ct[8] <-  (df0.ct$n1 / 3) + df0.ct$n2
  rdf<-as.data.frame(df0_[8]) 
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
}) 


# df0_[8] <-  df0_$n1 / (3 + df0_$n2)
test_that("df0_[8] <-  df0_$n1 / (3 + df0_$n2)", {
  df0_[8] <-  df0_$n1 / (3 + df0_$n2) 
  df0.ct[8] <-  df0.ct$n1 / (3 + df0.ct$n2)
  rdf<-as.data.frame(df0_[8]) 
  cdf=to.casDataFrame(df0.ct[8])
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf) 
}) 


# CompVar using modular arithmetic with missing values
test_that("Using modular arithmetic with missing values", {
  df0_$compMod<-df0_$n2 %% 3
  df0.ct$compMod<-df0.ct$n2 %% 3
  df0.ct$compMod2<-"compMod2=mod(n2, 3)"
  rdf<-as.data.frame(df0_$compMod)
  cdf=to.casDataFrame(df0.ct$compMod)
  cdf2=to.casDataFrame(df0.ct$compMod2)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# CompVar using exponential arithmetic with missing values
test_that("df0_$compExp<-df0_$n1^2", {
  df0_$compExp<-df0_$n1^2
  df0.ct$compExp<-df0.ct$n1^2
  df0.ct['compExp2']<-"compExp2=n1**2;"
  rdf<-as.data.frame(df0_$compExp)
  cdf=to.casDataFrame(df0.ct$compExp)
  cdf2=to.casDataFrame(df0.ct$compExp2)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})

test_that("df0_$compExp<-df0_$n5^3", {
  df0_$compExp<-df0_$n5^3
  df0.ct$compExp<-df0.ct$n5^3
  df0.ct['compExp2']<-"compExp2=n5**3;"
  rdf<-as.data.frame(df0_$compExp)
  cdf=to.casDataFrame(df0.ct$compExp)
  cdf2=to.casDataFrame(df0.ct$compExp2)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
})

test_that("df0_$compExp<-df0_$n1^df0_$n5", {
  df0_$compExp<-df0_$n1^df0_$n5
  df0.ct$compExp<-df0.ct$n1^df0.ct$n5
  df0.ct['compExp2']<-"compExp2=n1**n5;"
  rdf<-as.data.frame(df0_$compExp)
  cdf=to.casDataFrame(df0.ct$compExp2)
  cdf2=to.casDataFrame(df0.ct$compExp)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# CompVar using subtraction to get negative numbers
test_that("Using subtraction to get negative numbers", {
  df0_['compSub'] <- df0_$n3-df0_$n4
  df0.ct['compSub'] <- df0.ct$n3-df0.ct$n4
  df0.ct['compSub2'] <-"compSub2=n3-n4;" 
  rdf<-as.data.frame(df0_$compSub)
  cdf=to.casDataFrame(df0.ct$compSub)
  cdf2=to.casDataFrame(df0.ct$compSub2)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 
})


# CompVar using division and floor function
test_that("Using division and round function", {
  df0_['compRound'] <- floor(df0_$n4/3)
  df0.ct$compRound <- "compRound = floor(n4/3);"
  rdf<-as.data.frame(df0_$compRound)
  cdf=to.casDataFrame(df0.ct$compRound)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})


# CompVar assign constant value with different syntax
test_that("Assign constant value with different syntax", {
  df_$constant<-0.5
  df.ct['constant']<-'constant=0.5;'
  df.ct$constant2<-0.5
  rdf<-as.data.frame(df_$constant)
  cdf=to.casDataFrame(df.ct['constant'])
  cdf2<-to.casDataFrame(df.ct['constant2'])
  cas2rdf<-to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df.ct[1:8]), dimnames(df_)) 
})


# Using one compVar to create another, multiplication
test_that("Using one compVar to create another", {
  df_$constant<-0.5
  df.ct['constant']<-'constant=0.5;'
  df_$weightedVar<-df_$constant*df_$n1 + ((1-df_$constant)*df_$n2)
  df.ct$weightedVar<-df.ct$constant*df.ct$n1 + ((1-df.ct$constant)*df.ct$n2)
  df.ct$weightedVar2 <- "weightedVar2 = constant*n1 + ((1-constant)*n2);"
  rdf<-as.data.frame(df_$weightedVar)
  cdf=to.casDataFrame(df.ct$weightedVar)
  cdf2=to.casDataFrame(df.ct$weightedVar2)
  cas2rdf=to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df.ct[1:9]), dimnames(df_)) 
})



# Creating a duplicate column
test_that("Creating a duplicate column", {

df0_$n5dup<-df0_$n5
df0.ct$n5dup<-df0.ct$n5
df0.ct['n5dup2']<-'n5dup2=n5;'
rdf<-as.data.frame(df0_$n5dup)
cdf=to.casDataFrame(df0.ct$n5dup)
cdf2=to.casDataFrame(df0.ct$n5dup2)
cas2rdf=to.data.frame(cdf)
expect_equivalent(cas2rdf, rdf)
expect_equivalent(cdf, cdf2)
expect_equivalent(dimnames(df0.ct[1:8]), dimnames(df0_)) 

})


orig_options <- options()
options(cas.print.messages=TRUE)

# Deleting a compvar
test_that("Deleting a compvar", {
  
df0_$string<-"string"
df0.ct$string<-"sting"
df0.ct['string2']<-"string2='string';"
expect_silent(df0_$string<-NULL)
expect_silent(df0.ct$string<-NULL)
expect_silent(df0.ct['string2']<-NULL)
expect_equivalent(dimnames(df0.ct), dimnames(df0_))

})

# Deleting a compvar2
test_that("Deleting a compvar", {
  
df0_$string<-"string"
df0.ct$string<-"sting"
df0.ct['string2']<-"string2='string';"
expect_silent(df0_<-df0_[-8])
expect_silent(df0.ct<-df0.ct[-8])
expect_silent(df0.ct<-df0.ct[-8])
expect_equivalent(dimnames(df0.ct), dimnames(df0_))
  
})

# Deleting muiltiple compvars 1
test_that("Deleting a muiltiple compvars 1", {
  
df0_$string<-"string"
df0_$string2<-"string"
df0.ct$string<-"sting"
df0.ct['string2']<-"string2='string';"
  
df0_$dup<-df0_$n2
df0_$dup2<-df0_$n2
df0.ct$dup<-df0.ct$n2
df0.ct['dup2']<-"dup2=n2;"
  
df0_$compExp<-df0_$n1^2
df0_$compExp2<-df0_$n1^2
df0.ct$compExp<-df0.ct$n1^2
df0.ct['compExp2']<-"compExp2=n1**2;"
  
expect_silent(df0_[,8:10]<-NULL)
expect_silent(df0.ct[,8:10]<-NULL)
expect_equivalent(dimnames(df0.ct), dimnames(df0_))
  
})


# Deleting a muiltiple compvars 2
test_that("Deleting a muiltiple compvars 2", {
  
df0_$string<-"string"
df0_$string2<-"string"
df0.ct$string<-"sting"
df0.ct['string2']<-"string2='string';"
  
df0_$dup<-df0_$n2
df0_$dup2<-df0_$n2
df0.ct$dup<-df0.ct$n2
df0.ct['dup2']<-"dup2=n2;"
  
df0_$compExp<-df0_$n1^2
df0_$compExp2<-df0_$n1^2
df0.ct$compExp<-df0.ct$n1^2
df0.ct['compExp2']<-"compExp2=n1**2;"
  
expect_silent(df0_<-df0_[-(8:10)])
expect_silent(df0.ct<-df0.ct[-(8:10)])
expect_equivalent(dimnames(df0.ct), dimnames(df0_))
  
})


# Replacing a compvar
test_that("Deleting a compvar", {
  
df0_$replace<-df0_$n1
df0_$replace2<-df0_$n1
df0.ct$replace<-df0.ct$n1
df0.ct['replace2']<-"replace2=n1;"
expect_silent(df0_$replace<-df0_$n1 + 1)
expect_silent(df0_$replace2<-df0_$n1 + 1)
expect_silent(df0.ct$replace<-df0.ct$n1 + 1)
expect_silent(df0.ct['replace2']<-"replace2=n1 + 1;")
expect_equivalent(dimnames(df0.ct), dimnames(df0_))
  
})

# Replacing a string compvar
test_that("Replacing a string compvar", {
  
df0_$string<-"string"
df0_$string2<-"string"
df0.ct$string<-"sting"
df0.ct['string2']<-"string2='string';"
expect_silent(df0_$string<-"replace string")
expect_silent(df0_$string2<-"replace string")
expect_silent(df0.ct$string<-"replace sting")
expect_silent(df0.ct['string2']<-"string2='replace string';")
expect_equivalent(dimnames(df0.ct), dimnames(df0_))
  
})


# Replacing compvar with another compvar
test_that("Replacing compvar with another compvar", {
  
df0_$compvar<-df0_$n2*df0_$n5
df0.ct$compvar<-df0.ct$n2*df0.ct$n5
  
  
df0_$comprep<-df0_$n2
df0_$comprep2<-df0_$n2
df0.ct$comprep<-df0.ct$n2
df0.ct['comprep']<-"comprep=n2;"
  
# replace with compvar
#expect_error(df0.ct$comprep<-df0.ct$compvar/2, "Cannot define")
#expect_error(df0.ct['comprep2']<-"comprep2=compvar/2;", "Cannot define")

# replace with valid variables
expect_silent(df0_$comprep<-df0_$compvar/2)
expect_silent(df0_$comprep2<-df0_$compvar/2)
expect_silent(df0.ct$comprep<-(df0.ct$n2*df0.ct$n5)/2)
expect_silent(df0.ct$comprep2<-(df0.ct$n2*df0.ct$n5)/2)
expect_equivalent(dimnames(df0.ct), dimnames(df0_))
  
})


options(orig_options)








