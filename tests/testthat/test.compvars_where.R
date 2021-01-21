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

options(cas.print.messages = FALSE)


context("compvars_where.R")


test_that("Row indexing on character values, ==", {
  r <- df0_[df0_["s0"] == "dd", 1:5]
  cas <- df0_ct[df0_ct["s0"] == "dd", 1:5]
  df0_ct@where <- "s0 = 'dd'"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0_ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing on missing character values", {
  r <- df0_[df0_["s0"] == " ", 1:5]
  cas <- df0_ct[df0_ct["s0"] == " ", 1:5]
  df0_ct@where <- "s0 = ' '"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0_ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing on numeric values, ==", {
  r <- df_[df_["n5"] == 1.2, 1:5]
  cas <- df_ct[df_ct["n5"] == 1.2, 1:5]
  df0_ct@where <- "n5 = 1.2"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0_ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing on character values, !=", {
  r <- df0_[df0_["s0"] != "dd", 1:5]
  cas <- df0_ct[df0_ct["s0"] != "dd", 1:5]
  df0_ct@where <- "s0 ^= 'dd'"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0_ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing on numeric values, !=", {
  r <- df_[df_["n2"] != 7, 1:5]
  cas <- df_ct[df_ct["n2"] != 7, 1:5]
  df0_ct@where <- "n2 ^= '7"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0_ct[1:5], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing with multiple conditions, &", {
  r <- df_[df_["s"] == "dd" & df_["n4"] <= 15, c(1, 2)]
  cas <- df_ct[df_ct["s"] == "dd" & df_ct["n4"] <= 15, c(1, 2)]
  df_ct@where <- "s='dd' and n4<=15"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0_ct[1:2], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing with multiple conditions, |", {
  r <- df_[df_["s"] == "dd" | df_["n4"] <= 15, c(3, 4)]
  cas <- df_ct[df_ct["s"] == "dd" | df_ct["n4"] <= 15, c(3, 4)]
  df_ct@where <- "s='dd' | n4<=15"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df0_ct[3:4], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing with integer division, !=", {
  r <- df_[df_["n4"] %/% df_["n5"] <= 11, c(4, 5)]
  cas <- df_ct[df_ct["n4"] %/% df_ct["n5"] <= 11, c(4, 5)]
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("Row indexing with non-integer division, !=", {
  df_$nonintdiv <- df_["n4"] / df_["n5"]
  df_$nonintdiv2 <- df_["n4"] / df_["n5"]
  df_ct$nonintdiv <- df_ct["n4"] / df_ct["n5"]
  df_ct$nonintdiv2 <- "nonintdiv2 = n4 / n5;"
  rdf <- as.data.frame(df_$nonintdiv)
  cdf <- to.CASDataFrame(df_ct$nonintdiv)
  cdf2 <- to.CASDataFrame(df_ct$nonintdiv2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(length(dimnames(df_ct)[1]), length(dimnames(df_)[1]))
  expect_equivalent(dimnames(df_ct)[2], dimnames(df_)[2])
})

test_that("Row indexing with compVars", {
  df_$comp1 <- df_$n3 + df_$n4
  df_ct$comp1 <- df_ct$n3 + df_ct$n4
  r <- df_[df_["comp1"] < 30, c(1, 2, 3, 4, 5, 8)]
  cas <- df_ct[df_ct["comp1"] < 30, c(1, 2, 3, 4, 5, 8)]
  df_ct@where <- "comp1<30"
  rdf <- as.data.frame(r)
  cdf <- to.CASDataFrame(cas)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(df_ct[c(1, 2, 3, 4, 5, 8)], cas)
  expect_equivalent(length(dimnames(cas)[1]), length(dimnames(r)[1]))
  expect_equivalent(dimnames(cas)[2], dimnames(r)[2])
})

test_that("df0_['cmp1'] <- df0_$n1+df0_$n2", {
  df0_["cmp1"] <- df0_$n1 + df0_$n2
  df0_ct["cmp1"] <- "cmp1 = n1+n2;"
  rdf <- as.data.frame(df0_$cmp1)
  cdf <- to.CASDataFrame(df0_ct$cmp1)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_$cmp2 <- df0_$n3+df0_$n4", {
  df0_$cmp2 <- df0_$n3 + df0_$n4
  df0_ct$cmp2 <- "cmp2 = n3+n4"
  rdf <- as.data.frame(df0_$cmp2)
  cdf <- to.CASDataFrame(df0_ct$cmp2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_['cmp3'] <- df0_$n4 - df0_$n2 * df0_$n1", {
  df0_["cmp3"] <- df0_$n4 - df0_$n2 * df0_$n1
  df0_ct["cmp3"] <- df0_ct$n4 - df0_ct$n2 * df0_ct$n1
  rdf <- as.data.frame(df0_$cmp3)
  cdf <- to.CASDataFrame(df0_ct$cmp3)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_$cmp4 <- df0_$n4 - df0_$n1", {
  df0_$cmp4 <- df0_$n4 - df0_$n1
  df0_ct$cmp4 <- df0_ct$n4 - df0_ct$n1
  rdf <- as.data.frame(df0_$cmp4)
  cdf <- to.CASDataFrame(df0_ct$cmp4)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_$cmp5 <- df0_$n4 - 3", {
  df0_$cmp5 <- df0_$n4 - 3
  df0_ct$cmp5 <- df0_ct$n4 - 3
  rdf <- as.data.frame(df0_$cmp5)
  cdf <- to.CASDataFrame(df0_ct$cmp5)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_$cmp6 <- df0_$n4 + 3 - 2", {
  df0_$cmp6 <- df0_$n4 + 3 - 2
  df0_ct$cmp6 <- df0_ct$n4 + 3 - 2
  rdf <- as.data.frame(df0_$cmp6)
  cdf <- to.CASDataFrame(df0_ct$cmp6)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_[8] <- df0_$n1 / 3 * df0_$n2", {
  df0_[8] <- df0_$n1 / 3 * df0_$n2
  df0_ct[8] <- df0_ct$n1 / 3 * df0_ct$n2
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("df0_[8] <- df0_$n1 / 3 + df0_$n2", {
  df0_[8] <- df0_$n1 / 3 + df0_$n2
  df0_ct[8] <- df0_ct$n1 / 3 + df0_ct$n2
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("df0_[8] <- df0_$n1 + 3 / df0_$n2", {
  df0_[8] <- df0_$n1 + 3 / df0_$n2
  df0_ct[8] <- df0_ct$n1 + 3 / df0_ct$n2
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("df0_[8] <- (df0_$n1 + 3) / df0_$n2", {
  df0_[8] <- (df0_$n1 + 3) / df0_$n2
  df0_ct[8] <- (df0_ct$n1 + 3) / df0_ct$n2
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("df0_[8] <- df0_$n1 / 3 + df0_$n2", {
  df0_[8] <- df0_$n1 / 3 + df0_$n2
  df0_ct[8] <- df0_ct$n1 / 3 + df0_ct$n2
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("df0_[8] <- df0_$n1 / (3 + df0_$n2)", {
  df0_[8] <- df0_$n1 / (3 + df0_$n2)
  df0_ct[8] <- df0_ct$n1 / (3 + df0_ct$n2)
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("df0_[8] <- (df0_$n1 / 3) + df0_$n2", {
  df0_[8] <- (df0_$n1 / 3) + df0_$n2
  df0_ct[8] <- (df0_ct$n1 / 3) + df0_ct$n2
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("df0_[8] <- df0_$n1 / (3 + df0_$n2)", {
  df0_[8] <- df0_$n1 / (3 + df0_$n2)
  df0_ct[8] <- df0_ct$n1 / (3 + df0_ct$n2)
  rdf <- as.data.frame(df0_[8])
  cdf <- to.CASDataFrame(df0_ct[8])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("Using modular arithmetic with missing values", {
  df0_$compmod <- df0_$n2 %% 3
  df0_ct$compmod <- df0_ct$n2 %% 3
  df0_ct$compmod2 <- "compmod2=mod(n2, 3)"
  rdf <- as.data.frame(df0_$compmod)
  cdf <- to.CASDataFrame(df0_ct$compmod)
  cdf2 <- to.CASDataFrame(df0_ct$compmod2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_$compExp <- df0_$n1^2 with missing values", {
  df0_$compexp <- df0_$n1^2
  df0_ct$compexp <- df0_ct$n1^2
  df0_ct["compexp2"] <- "compexp2=n1**2;"
  rdf <- as.data.frame(df0_$compexp)
  cdf <- to.CASDataFrame(df0_ct$compexp)
  cdf2 <- to.CASDataFrame(df0_ct$compexp2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("df0_$compExp<-df0_$n5^3", {
  df0_$compexp <- df0_$n5^3
  df0_ct$compexp <- df0_ct$n5^3
  df0_ct["compexp2"] <- "compexp2=n5**3;"
  rdf <- as.data.frame(df0_$compexp)
  cdf <- to.CASDataFrame(df0_ct$compexp)
  cdf2 <- to.CASDataFrame(df0_ct$compexp2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
})

test_that("df0_$compExp<-df0_$n1^df0_$n5", {
  df0_$compexp <- df0_$n1^df0_$n5
  df0_ct$compexp <- df0_ct$n1^df0_ct$n5
  df0_ct["compexp2"] <- "compExp2=n1**n5;"
  rdf <- as.data.frame(df0_$compexp)
  cdf <- to.CASDataFrame(df0_ct$compexp2)
  cdf2 <- to.CASDataFrame(df0_ct$compexp)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("Using subtraction to get negative numbers", {
  df0_["comp_sub"] <- df0_$n3 - df0_$n4
  df0_ct["comp_sub"] <- df0_ct$n3 - df0_ct$n4
  df0_ct["comp_sub2"] <- "comp_sub2=n3-n4;"
  rdf <- as.data.frame(df0_$comp_sub)
  cdf <- to.CASDataFrame(df0_ct$comp_sub)
  cdf2 <- to.CASDataFrame(df0_ct$comp_sub2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("Using division and round function", {
  df0_["comp_round"] <- floor(df0_$n4 / 3)
  df0_ct$comp_round <- "comp_round = floor(n4/3);"
  rdf <- as.data.frame(df0_$comp_round)
  cdf <- to.CASDataFrame(df0_ct$comp_round)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
})

test_that("Assign constant value with different syntax", {
  df_$constant <- 0.5
  df_ct["constant"] <- "constant=0.5;"
  df_ct$constant2 <- 0.5
  rdf <- as.data.frame(df_$constant)
  cdf <- to.CASDataFrame(df_ct["constant"])
  cdf2 <- to.CASDataFrame(df_ct["constant2"])
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df_ct[1:8]), dimnames(df_))
})

test_that("Using one compVar to create another", {
  df_$constant <- 0.5
  df_ct["constant"] <- "constant=0.5;"
  df_$weighted_var <- df_$constant * df_$n1 + ((1 - df_$constant) * df_$n2)
  df_ct$weighted_var <- df_ct$constant * df_ct$n1 + ((1 - df_ct$constant) * df_ct$n2)
  df_ct$weighted_var2 <- "weighted_var2 = constant*n1 + ((1-constant)*n2);"
  rdf <- as.data.frame(df_$weighted_var)
  cdf <- to.CASDataFrame(df_ct$weighted_var)
  cdf2 <- to.CASDataFrame(df_ct$weighted_var2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df_ct[1:9]), dimnames(df_))
})

test_that("Creating a duplicate column", {
  df0_$n5dup <- df0_$n5
  df0_ct$n5dup <- df0_ct$n5
  df0_ct["n5dup2"] <- "n5dup2=n5;"
  rdf <- as.data.frame(df0_$n5dup)
  cdf <- to.CASDataFrame(df0_ct$n5dup)
  cdf2 <- to.CASDataFrame(df0_ct$n5dup2)
  cas2rdf <- to.data.frame(cdf)
  expect_equivalent(cas2rdf, rdf)
  expect_equivalent(cdf, cdf2)
  expect_equivalent(dimnames(df0_ct[1:8]), dimnames(df0_))
})

test_that("Deleting a compvar", {
  df0_$string <- "string"
  df0_ct$string <- "sting"
  df0_ct["string2"] <- "string2='string';"
  expect_silent(df0_$string <- NULL)
  expect_silent(df0_ct$string <- NULL)
  expect_silent(df0_ct["string2"] <- NULL)
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Deleting a compvar 2", {
  df0_$string <- "string"
  df0_ct$string <- "sting"
  df0_ct["string2"] <- "string2='string';"
  expect_silent(df0_ <- df0_[-8])
  expect_silent(df0_ct <- df0_ct[-8])
  expect_silent(df0_ct <- df0_ct[-8])
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Deleting a muiltiple compvars 1", {
  df0_$string <- "string"
  df0_$string2 <- "string"
  df0_ct$string <- "sting"
  df0_ct["string2"] <- "string2='string';"

  df0_$dup <- df0_$n2
  df0_$dup2 <- df0_$n2
  df0_ct$dup <- df0_ct$n2
  df0_ct["dup2"] <- "dup2=n2;"

  df0_$compexp <- df0_$n1^2
  df0_$compexp2 <- df0_$n1^2
  df0_ct$compexp <- df0_ct$n1^2
  df0_ct["compexp2"] <- "compexp2=n1**2;"

  expect_silent(df0_[, 8:10] <- NULL)
  expect_silent(df0_ct[, 8:10] <- NULL)
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Deleting a muiltiple compvars 2", {
  df0_$string <- "string"
  df0_$string2 <- "string"
  df0_ct$string <- "sting"
  df0_ct["string2"] <- "string2='string';"

  df0_$dup <- df0_$n2
  df0_$dup2 <- df0_$n2
  df0_ct$dup <- df0_ct$n2
  df0_ct["dup2"] <- "dup2=n2;"

  df0_$compexp <- df0_$n1^2
  df0_$compexp2 <- df0_$n1^2
  df0_ct$compexp <- df0_ct$n1^2
  df0_ct["compexp2"] <- "compexp2=n1**2;"

  expect_silent(df0_ <- df0_[- (8:10)])
  expect_silent(df0_ct <- df0_ct[- (8:10)])
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Deleting a compvar", {
  df0_$replace <- df0_$n1
  df0_$replace2 <- df0_$n1
  df0_ct$replace <- df0_ct$n1
  df0_ct["replace2"] <- "replace2=n1;"
  expect_silent(df0_$replace <- df0_$n1 + 1)
  expect_silent(df0_$replace2 <- df0_$n1 + 1)
  expect_silent(df0_ct$replace <- df0_ct$n1 + 1)
  expect_silent(df0_ct["replace2"] <- "replace2=n1 + 1;")
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Replacing a string compvar", {
  df0_$string <- "string"
  df0_$string2 <- "string"
  df0_ct$string <- "sting"
  df0_ct["string2"] <- "string2='string';"
  expect_silent(df0_$string <- "replace string")
  expect_silent(df0_$string2 <- "replace string")
  expect_silent(df0_ct$string <- "replace sting")
  expect_silent(df0_ct["string2"] <- "string2='replace string';")
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Replacing compvar with another compvar", {
  df0_$compvar <- df0_$n2 * df0_$n5
  df0_ct$compvar <- df0_ct$n2 * df0_ct$n5

  df0_$comprep <- df0_$n2
  df0_$comprep2 <- df0_$n2
  df0_ct$comprep <- df0_ct$n2
  df0_ct["comprep2"] <- "comprep2=n2;"

  # replace with compvar
  df0_$comprep <- df0_$compvar / 2
  df0_$comprep2 <- df0_$compvar / 2
  expect_silent(df0_ct$comprep <- df0_ct$compvar / 2)
  expect_silent(df0_ct["comprep2"] <- "comprep2=compvar/2;")
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))

  # delete compvar composed of another compvar
  df0_$comprep <- NULL
  df0_$comprep2 <- NULL
  expect_silent(df0_ct$comprep <- NULL)
  expect_silent(df0_ct["comprep2"] <- NULL)
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Replacing string compvar with another string compvar", {
  df0_$compvar <- "compvar1"
  df0_ct$compvar <- "compvar1"


  df0_$comprep <- "compvar2"
  df0_$comprep2 <- "compvar2"
  df0_ct$comprep <- "compvar2"
  df0_ct["comprep2"] <- "comprep2='compvar2';"

  # replace with compvar
  df0_$comprep <- "compvar2"
  df0_$comprep2 <- "compvar2"
  expect_silent(df0_ct$comprep <- df0_ct$compvar)
  expect_silent(df0_ct["comprep2"] <- "comprep2=compvar;")
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))

  # delete compvar composed of another compvar
  df0_$comprep <- NULL
  df0_$comprep2 <- NULL
  expect_silent(df0_ct$comprep <- NULL)
  expect_silent(df0_ct["comprep2"] <- NULL)
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})

test_that("Replacing compvar with non-compvar", {
  df0_$compvar <- df0_$n3 / df0_$n5
  df0_ct$compvar <- df0_ct$n2 / df0_ct$n5

  df0_$comprep <- df0_$n4
  df0_$comprep2 <- df0_$n4
  df0_ct$comprep <- df0_ct$n4
  df0_ct["comprep2"] <- "comprep2=n4;"

  # replace with compvar
  df0_$comprep <- df0_$compvar + df0_$n4
  df0_$comprep2 <- df0_$compvar + df0_$n4
  expect_silent(df0_ct$comprep <- df0_ct$compvar + df0_ct$n4)
  expect_silent(df0_ct["comprep2"] <- "comprep2=compvar + n4;")
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))

  # replace with non-compvar variables
  expect_silent(df0_$comprep <- (df0_$n3 / df0_$n5) + df0_$n4)
  expect_silent(df0_$comprep2 <- (df0_$n3 / df0_$n5) + df0_$n4)
  expect_silent(df0_ct$comprep <- (df0_ct$n3 / df0_ct$n5) + df0_ct$n4)
  expect_silent(df0_ct$comprep2 <- (df0_ct$n3 / df0_ct$n5) + df0_ct$n4)
  expect_equivalent(dimnames(df0_ct), dimnames(df0_))
})
