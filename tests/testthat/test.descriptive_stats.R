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


# #test.descriptive_stats.R
context("descriptive_stats.R")

# overloaded functions
test_that("Table Meta Functions", {
  expect_equivalent(rownames(ct), rownames(df))
  ## colnames()
  expect_equivalent(colnames(df), colnames(ct))

  ## dimnames()
  expect_equivalent(dimnames(df), dimnames(ct))

  ## nrow()
  expect_equivalent(nrow(df), nrow(ct))

  ## ncol()
  expect_equivalent(ncol(df), ncol(ct))

  ## dim()
  expect_equivalent(dim(df), dim(ct))
})

test_that("cas.count", {
  expect_that(cas.count(ct1), is_a("data.frame"))
  Column = c("n1", "n2", "n3", "n4")
  N = c(6)
  col_count = data.frame(Column, N)
  col_count$Column <- as.character(col_count$Column)
  expect_equivalent(cas.count(ct[1:4]), col_count)
})

# min()
test_that("cas.min", {
  expect_that(cas.min(ct1), is_a("data.frame"))
  Column = c("n1", "n2", "n3", "n4")
  Min = c(2, 5, 8, 8)
  col_min = data.frame(Column, Min)
  col_min$Column <- as.character(col_min$Column)
  expect_equivalent(cas.min(ct[1:4]), col_min)
})

test_that("cas.min", {expect_that(cas.min(i2), is_a("data.frame"))  
  Column = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")  
  min = c(min(iris[1]), min(iris[2]),min(iris[3]), min(iris[4]))  
  col_min = data.frame(Column, min)  
  col_min$Column <- as.character(col_min$Column)  
  expect_equivalent(cas.min(i2[1:4]), col_min)
  
  #missing values
  expect_that(cas.min(ct), is_a("data.frame"))  
  Column = c("n1", "n2", "n3", "n4")  
  min = c(2,5,12,8)  
  col_min = data.frame(Column, min)  
  col_min$Column <- as.character(col_min$Column)  
  expect_equivalent(cas.min(ct0[1:4]), col_min)
  })

#max
test_that("cas.max", {expect_that(cas.max(i2), is_a("data.frame"))  
  Column = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")  
  Max = c(max(iris[1]), max(iris[2]),max(iris[3]), max(iris[4]))  
  col_max = data.frame(Column, Max)  
  col_max$Column <- as.character(col_max$Column)  
  expect_equivalent(cas.max(i2[1:4]), col_max)
  
  expect_that(cas.max(ct), is_a("data.frame"))  
  Column = c("n1", "n2", "n3", "n4")  
  Max = c(598, 120, 198, 1120)  
  col_max = data.frame(Column, Max)  
  col_max$Column <- as.character(col_max$Column)  
  expect_equivalent(cas.max(ct0[1:4]), col_max)
  })     

# median
test_that("cas.median", {expect_that(cas.median(mtcars.ct), is_a("data.frame"))  
  variable = c("mpg", "cyl", "disp", "hp")  
  median = c(median(mtcars[[1]]), median(mtcars[[2]]),median(mtcars[[3]]), median(mtcars[[4]]))  
  col_median = data.frame(variable, median)  
  expect_equivalent(cas.median(mtcars.ct[1:4])[2], col_median[2])
  
  expect_that(cas.median(ctn), is_a("data.frame"))  
  Column = c("n1", "n2", "n3", "n4")  
  median = c(median(dfn[[1]], na.rm=TRUE), median(dfn[[2]], na.rm=TRUE), median(dfn[[3]], na.rm=TRUE), median(dfn[[4]], na.rm=TRUE))
  col_median = data.frame(Column, median)  
  col_median$Column <- as.character(col_median$Column)  
  expect_equivalent(cas.median(ctn[1:4]), col_median)
})

# median
test_that("cas.median", {expect_that(cas.median(mtcars.ct), is_a("data.frame"))  
  variable = c("mpg", "cyl", "disp", "hp")  
  median = c(median(mtcars[[1]]), median(mtcars[[2]]),median(mtcars[[3]]), median(mtcars[[4]]))  
  col_median = data.frame(variable, median)  
  expect_equivalent(cas.median(mtcars.ct[1:4])[2], col_median[2])
  
  expect_that(cas.median(ctn), is_a("data.frame"))  
  Column = c("n1", "n2", "n3", "n4")  
  median = c(median(dfn[[1]], na.rm=TRUE), median(dfn[[2]], na.rm=TRUE), median(dfn[[3]], na.rm=TRUE), median(dfn[[4]], na.rm=TRUE))
  col_median = data.frame(Column, median)  
  col_median$Column <- as.character(col_median$Column)  
  expect_equivalent(cas.median(ctn[1:4]), col_median)
})

#max
test_that("cas.max", {expect_that(cas.max(i2), is_a("data.frame"))  
  Column = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")  
  Max = c(max(iris[1]), max(iris[2]),max(iris[3]), max(iris[4]))  
  col_max = data.frame(Column, Max)  
  col_max$Column <- as.character(col_max$Column)  
  expect_equivalent(cas.max(i2[1:4]), col_max)
  
  expect_that(cas.max(ct), is_a("data.frame"))  
  Column = c("n1", "n2", "n3", "n4")  
  Max = c(598, 120, 198, 1120)  
  col_max = data.frame(Column, Max)  
  col_max$Column <- as.character(col_max$Column)  
  expect_equivalent(cas.max(ct0[1:4]), col_max)
  })     

test_that("cas.mode", {
  expect_that(cas.mode(ct), is_a("data.frame"))
  expect_equivalent(names(cas.mode(ct[5])), c("Column", "Mode", "Count"))
  expect_equivalent(dim(cas.mode(ct)), c(6,3))
  expect_equivalent(dim(cas.mode(ct[1:5])), c(5,3))
  expect_equivalent(dim(cas.mode(i2)), c(5,3))
  expect_equivalent(cas.mode(i2)[4,3], 29 )
})

test_that("cas.quantile", {
  expect_that(cas.quantile(ct1, q = list(25, 50, 75)), is_a("data.frame"))
  expect_error(cas.quantile(ct))
  Variable = c("n1", "n1", "n2", "n2", "n3", "n3", "n4", "n4")
  Pctl = c(25, 75)
  Value = c(3, 5, 6, 8, 12, 15, 15, 17)
  col_quant = data.frame(Variable, Pctl, Value)
  col_quant$Variable <- as.character(col_quant$Variable)
  expect_equivalent(cas.quantile(ct[1:4], q = c(25, 75)), col_quant)
})

test_that("cas.tvalue", {
  expect_that(cas.tvalue(ct), is_a("numeric"))
  n1 = c(3,3,3,3,3,3)
  n2 = c(5, 6, 7, 7, 8, 120)
  n3 = c(12, 13, 15, 15, 8, 198)
  n4 = c(15, 16, 17,  15, 8, 1120)
  dfc = data.frame(n1, n2, n3, n4)
  ctc  <- as.casTable(caz, dfc)
  response <- as.numeric(c(NaN, 1.348879, 1.406952, 1.077021))
  names(response) <- c('n1', 'n2', 'n3', 'n4')
  expect_equal(cas.tvalue(ctc), response, tolerance=1e-6)
})


test_that("colSums", {
  out <- colSums(ct[1:4])
  expect_true(class(out) == "numeric" || class(out) == "integer")
  expect_equivalent(colSums(df[1:4]), out)
})

## colMeans()
test_that("colMeans", {
  expect_that(colMeans(ct[1:4]), is_a("numeric"))
  expect_equivalent(colMeans(df[1:4]), colMeans(ct[1:4]))
  # colMeans seems to always return the columns in order, even if a different order is specified.
  expect_failure(expect_equivalent(colMeans(ct[c(1, 4, 3)]), colMeans(df[c(1, 3, 4)])))
  expect_equivalent(colMeans(ct[c(1, 4, 3)]), colMeans(df[c(1, 4, 3)]))
})

## head()
test_that("head", {
  # this diff because of the factor column in 's'
  #expect_equivalent(head(df, 4), head(ct, 4))
  expect_equivalent(head(df[1:4], 4), head(ct[1:4], 4))
  expect_that(head(ct,4), is_a("data.frame"))
})

test_that("tail", {
  # Order is non-deterministic in CAS tables
  i2@orderby <- list('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width') 
  iris.sorted <- iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width), ]
  expect_equivalent(tail(df[1:4], 4), tail(ct[1:4], 4))
  expect_that(tail(ct,4), is_a("data.frame"))
  expect_equivalent(tail(i2[1:4]), tail(iris.sorted[1:4]))
  i2@orderby <- list()
  
  # check that row index numbering is correct
  expect_equivalent(rownames(tail(i2[1:4])), rownames(tail(iris[1:4])))
  
  # check that tail works correctly after sort
  ct@orderby <- list("n4")
  expect_equivalent(tail(df[order(df$n4), c(1:4)], 4), tail(ct[c(1:4)], 4))
  expect_true(all.equal(tail(df[order(df$n4), c(1:4)], 4), tail(ct[c(1:4)], 4), tolerance=1e-06, check.attributes=FALSE))
  
  #
  # NOTE: Character columns are returned as factors in R and Date columns
  #       are returned as Character.  Currently the tail function is not the
  #       the same for those two types.
  # 
  # Column 5 is a Character column.  Column 6 is a Date column.
  # expect_equivalent(tail(df[5:6], 4), tail(ct[5:6], 4))
})  

# Correlation
test_that("correlation", {
  expect_equivalent(cor(ct1), cor(df1))
  expect_equivalent(cor(i2[1:4]), cor(iris[1:4]))
  expect_equivalent(cor(i2$Sepal.Length, i2$Sepal.Width), cor(iris$Sepal.Length, iris$Sepal.Width))
  expect_equivalent(cor(i2[1:2], i2[3:4]), cor(iris[1:2], iris[3:4]))
  expect_that(cor(ct1), is_a("matrix"))
  expect_that(cor(df1), is_a("matrix"))
  expect_that(cor(i2[1:2], i2[3:4]), is_a("matrix"))
  expect_that(cor(iris[1:2], iris[3:4]), is_a("matrix"))
  # use=
  expect_equivalent(cor(ct0$n1, ct0$n2), as.numeric(NA))
  expect_equivalent(cor(ct0$n1, ct0$n2), cor(df0$n1, df0$n2))
  expect_equivalent(cor(df0$n1, df0$n2, use='complete'), cor(ct0$n1, ct0$n2, use='complete'))
  expect_true(all.equal(cor(ctn), cor(dfn),tolerance = 1.e-6))
  # method
  expect_true(all.equal(cor(iris$Sepal.Length, iris$Sepal.Width, method="pearson"),-0.1175698, tolerance = 1.e-6))
  #everything
  expect_equivalent(cor(ct$n1, ct$n2, use = "everything"),cor(df$n1, df$n2, use = "everything"))
  
  # Missing values test
  expect_equivalent(cor(ctn), cor(dfn))
  expect_equal(cor(ct0[1:4], use = "complete"), cor(df0[1:4], use = "complete"), tolerance = 1.e-5)
})

# Covariance
test_that("covariance", {
  expect_equivalent(cov(ct1), cov(df1))
  expect_equivalent(cov(i2[1:4]), cov(iris[1:4]))
  expect_equivalent(cov(i2$Sepal.Length, i2$Sepal.Width), cov(iris$Sepal.Length, iris$Sepal.Width))
  expect_equivalent(cov(i2[1:2], i2[3:4]), cov(iris[1:2], iris[3:4]))
  expect_that(cov(ct1), is_a("matrix"))
  expect_that(cov(i2[1:2], i2[3:4]), is_a("matrix"))
  expect_that(cov(df1), is_a("matrix"))
  expect_that(cov(iris[1:2], iris[3:4]), is_a("matrix"))
  # use=
  expect_equivalent(cov(ct0$n1, ct0$n2), as.numeric(NA))
  expect_equivalent(cov(ct0$n1, ct0$n2), cov(df0$n1, df0$n2))
  expect_equivalent(cov(df0$n1, df0$n2, use='complete'), cov(ct0$n1, ct0$n2, use='complete'))
  expect_equivalent(cov(df0[1:3], use='complete'), cov(ct0[1:3], use='complete'))
  expect_true(all.equal(cov(ctn), cov(dfn), tolerance = 1.e-6))
  # method
  expect_true(all.equal(cov(iris$Sepal.Length, iris$Sepal.Width, method="pearson"),-0.042434, tolerance = 1.e-6))
  expect_true(all.equal(cov(i2$Sepal.Length, i2$Sepal.Width, method="pearson"),-0.042434, tolerance = 1.e-6))
  #everything
  expect_equivalent(cov(ct$n1, ct$n2, use = "everything"),cov(df$n1, df$n2, use = "everything"))
})

#str
test_that("str", {
  expect_output(str(ct), "CASTable")
})

# summary()
test_that("summary", {
  expect_that(summary(i2), is_a("table"))
  expect_equivalent(summary(i2), summary(iris))
})

#column selection
test_that("column selection", { 
  expect_equivalent(i2[1:3], i2[c('Sepal.Length', 'Sepal.Width', 'Petal.Length')]) 
  expect_equivalent(i2[1], i2$Sepal.Length)  
  expect_equivalent(i2['Sepal.Length'], i2$Sepal.Length)
})

#uss
test_that("cas.uss", {   
  expect_equivalent(cas.uss(i2[1:4]), c(5223.85, 1430.40, 2582.71, 302.33 ) )
})  

# cv
test_that("cas.cv", {   
  cv1 = c(14.17113, 14.25642,  46.97441, 63.55511  ) 
  all.equal(cas.cv(i2[1:4]), cv1, check.names = FALSE, tolerance = 1.e-4) 
  # error due to issue 87
  all.equal(cas.cv(ct0[1:4]), c(235.9001012,  187.7981392,  162.8643165, 210.3022417), tolerance = 1.e-4, check.names = FALSE )
})  

#quantile
test_that("cas.quantile", { quantile = c(19.200, 6.000, 196.300, 123.000)  
expect_equivalent(cas.quantile(mtcars.ct[1:4], 50)[[3]], quantile)
quantile = c(2.5,5.5,13,11.5) 
expect_equivalent(cas.quantile(ctn[1:4], 20)[[3]], quantile)
}) 

#sd
test_that("cas.sd", { sd = c(6.0269481, 1.7859216, 123.9386938, 68.5628685)
    expect_equivalent(cas.sd(mtcars.ct[1:4])[[2]], sd)
    sd = c(265.75985, 50.87730, 82.40934, 494.63087)
    all.equal(cas.sd(ctn[1:4])[[2]], sd, tolerance = 1.e-7 )
})

#css
test_that("cas.css", {   
  css1 = c( 102.16833,     28.30693,    464.32540,     86.56993 ) 
  all.equal(cas.css(i2[1:4]), css1, check.names = FALSE, tolerance = 1.e-4)
  all.equal(cas.css(ct0[1:4]), c(282513.2,  10354.0,  27165.2, 978638.8), tolerance = 1.e-4, check.names = FALSE )
})

