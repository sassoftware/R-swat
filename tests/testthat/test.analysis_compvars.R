# test.analysis_compvars.R

library(swat)

options(cas.print.messages=FALSE)


context("test.analysis_compvars.R")
test_that("unique", {
  # Computed columns are not supported with the unique function
  expect_error(expect_equivalent(unique(ct_cmp$n1), unique(ct_cmp$cv1)))
  expect_error(expect_equivalent(unique(ct_cmp$cv1), unique(df_cmp$cv1)))
  expect_error(expect_equivalent(unique(ct_cmp[c(1:3,6:8)]), unique(df_cmp[c(1:3,6:8)])))
})

test_that("subset", {
  expect_equivalent(subset(ct_cmp, subset = ct_cmp$cv1 > 15, select = c("n1", "n2", "cv1"), drop = FALSE),
                    as.casTable(caz, subset(df_cmp, subset = df_cmp$cv2 > 15, select = c("n1", "n2", "cv1"), drop = FALSE), 
                                casOut =list(name='foobar', replace=TRUE)))
})

test_that("nrow", {
  expect_equivalent(6, nrow(ct_cmp))
  expect_equivalent(6, nrow(ct_cmp[6:8]))
})

test_that("ncol", {
  expect_equivalent(4, ncol(ct_cmp[5:8]))
  expect_equivalent(1, ncol(ct_cmp[7]))
  expect_equivalent(1, ncol(ct_cmp$cv1))
})

test_that("dim", {
  expect_equivalent(dim(df_cmp[1]), dim(ct_cmp[1]))
  expect_equivalent(dim(df_cmp[4:8]), dim(ct_cmp[4:8]))
  expect_equivalent(dim(cas.count(ct_cmp[4:8])), c(3,2))
})

# test_that("min, max, median", {
#   min(df_cmp[8])
#   bench <-c(8, 8, 2, 5 )
#   expect_equivalent(cas.min(ct_cmp[3:8])[2], as.data.frame(bench))
#   
#   expect_true(all.equal(cas.min(ct_cmp[3:8][2]), 
#                         as.data.frame(sapply(df_cmp[c(3:4, 7:8)], min, na.rm = TRUE)), check.attributes=FALSE ))
#   
#   expect_true(all.equal(cas.max(ct_cmp[3:8][2]), 
#                         as.data.frame(sapply(df_cmp[c(3:4, 7:8)], max, na.rm = TRUE)), check.attributes=FALSE ))
#   
#   expect_true(all.equal(cas.median(ct_cmp[3:8][2]), 
#                         as.data.frame(sapply(df_cmp[c(3:4, 7:8)], median, na.rm = TRUE)), check.attributes=FALSE ))
# })

test_that("min, max, median", {
  min(df_cmp[8])
  bench <-c(8, 8, 2, 5 )
  expect_equivalent(cas.min(ct_cmp[3:8])[2], as.data.frame(bench))
  
  expect_equivalent(cas.min(ct_cmp[3:8])[2], 
                    as.data.frame(sapply(df_cmp[c(3:4, 7:8)], min, na.rm = TRUE)) )
  
  expect_equivalent(cas.max(ct_cmp[3:8])[2], 
                    as.data.frame(sapply(df_cmp[c(3:4, 7:8)], max, na.rm = TRUE)) )
  
  expect_equivalent(cas.median(ct_cmp[3:8])[2], 
                    as.data.frame(sapply(df_cmp[c(3:4, 7:8)], median, na.rm = TRUE)) )
})

test_that("mode", {
  bench <- c(3,2,2,2,3,1,3,2)
  expect_true(all.equal(cas.mode(ct_cmp[1:8])[3], 
                        as.data.frame(bench), check.attributes=FALSE ))
})

test_that("tvalue", {
  expect_equivalent(cas.tvalue(ct_cmp[1:2]), cas.tvalue(ct_cmp[7:8]))
  expect_equivalent(length(cas.tvalue(ct_cmp[3:8])), 4)
})

test_that("colSums", {
  expect_equivalent(colSums(df_cmp[c(1:4,7:8)]), colSums(ct_cmp[c(1:4,7:8)]))    
})

test_that("head, tail", {
  expect_equivalent(head(df_cmp[c(1:4,7:8)], 4), head(ct_cmp[c(1:4,7:8)], 4))
  
  
  expect_equivalent(tail(df_cmp[c(1:4,7:8)], 4), tail(ct_cmp[c(1:4,7:8)], 4))
})

test_that("cor, cov", {
  expect_equal(cor(df_cmp[c(1:4,7:8)], use = "complete"), cor(ct_cmp[c(1:4,7:8)], use = "complete"), tolerance = 1.e-5)
  
  expect_equivalent(cov(ct_cmp$n1, ct_cmp$n2), cov(df_cmp$cv1, df_cmp$cv2))
  expect_equivalent(cov(ct_cmp$n1, ct_cmp$n2), cov(ct_cmp$cv1, ct_cmp$cv2))
  expect_true(all.equal(cov(df_cmp[c(1:4,7:8)]), cov(ct_cmp[c(1:4,7:8)]), tolerance = 1.e-6))
})

test_that("summary", {
  expect_true(all.equal(summary(ct_cmp[c(1:4,7:8)]), summary(df_cmp[c(1:4,7:8)], quantile.type = 2), check.attributes = FALSE))
})
