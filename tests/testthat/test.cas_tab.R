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


context("test.CAStab.R")


test_that("delete column", {
  ctt <- ct
  ctt[1] <- NULL
  expect_equivalent(ctt, ct[-1])
  ctt$n3 <- NULL
  ctt$d <- NULL
  expect_equivalent(ctt, ct[c(-1, -3, -6)])
  ctt[1:2] <- NULL
  expect_equivalent(ctt, ct$s)
})

test_that("row index where", {
  expect_equivalent(as.casTable(caz, df[df$n4 > 15 & df$n1 < 6, c(1, 4, 5)]),
                    ct[ct$n4 > 15 & ct$n1 < 6, c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[df$n4 > 15 & df[1] < 6, c(1, 4, 5)]),
                    ct[ct$n4 > 15 & ct[1] < 6, c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[df$n4 > 15 | df[1] < 6, c(1, 4, 5)]),
                    ct[ct$n4 > 15 | ct[1] < 6, c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[!df$n4 > 15 | df[1] < 6, c(1, 4, 5)]),
                    ct[!ct$n4 > 15 | ct[1] < 6, c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[!df$n4 > 15 | df[1] < 6, ]),
                    ct[!ct$n4 > 15 | ct[1] < 6, ])
  expect_equivalent(as.casTable(caz, df[!df$n4 > 15 | !df[1] < 6, ]),
                    ct[!ct$n4 > 15 | !ct[1] < 6, ])

  expect_equivalent(as.casTable(caz, df[, c(1, 4, 5)]), ct[, c("n1", "n4", "s")])
  expect_equivalent(as.casTable(caz, df[c(1, 4, 5)]), ct[c("n1", "n4", "s")])
  expect_equivalent(ct[c(1, 4, 5)], ct[c("n1", "n4", "s")])

  # There are issues with the return type.
  # > expect_that(class(ct[ct$n4 > 15 & ct$n1 < 6 , c("n1", "n4", "s")]), is_a("CASTable"))
  # > expect_that(class(ct[ct$n4 > 15 & ct$n1 < 6 , c("n1", "n4", "s")]), is_a("character"))
})


test_that("drop columns by column number", {
  my_df <- df
  my_ct <- ct

  # drop number column
  my_df[2] <- NULL
  my_ct[2] <- NULL
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)

  # drop character column
  my_df[4] <- NULL
  my_ct[4] <- NULL
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)

  # drop date column
  my_df[4] <- NULL
  my_ct[4] <- NULL
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)
})

test_that("drop columns by column name", {
  my_df <- df
  my_ct <- ct

  # drop number column
  my_df$n2 <- NULL
  my_ct$n2 <- NULL
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)

  # drop character column
  my_df$s <- NULL
  my_ct$s <- NULL
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)

  # drop date column
  my_df$d <- NULL
  my_ct$d <- NULL
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)
})

test_that("drop multiple columns", {
  my_df <- df
  my_ct <- ct

  my_df[3:6] <- list(NULL)
  my_ct[3:6] <- list(NULL)
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)
})

test_that("drop columns by number w/missing data", {
  my_df0 <- df0
  my_ct0 <- ct0

  # drop number column
  my_df0[2] <- NULL
  my_ct0[2] <- NULL
  expect_equivalent(as.casTable(caz, my_df0, casOut = list(replace = TRUE)), my_ct0)

  # drop character column
  my_df0[4] <- NULL
  my_ct0[4] <- NULL
  expect_equivalent(as.casTable(caz, my_df0, casOut = list(replace = TRUE)), my_ct0)

  # drop date column
  my_df0[4] <- NULL
  my_ct0[4] <- NULL
  expect_equivalent(as.casTable(caz, my_df0, casOut = list(replace = TRUE)), my_ct0)
})

test_that("drop by column name, missing data", {
  my_df0 <- df0
  my_ct0 <- ct0

  # drop number column
  my_df0$n2 <- NULL
  my_ct0$n2 <- NULL
  expect_equivalent(as.casTable(caz, my_df0, casOut = list(replace = TRUE)), my_ct0)

  # drop character column
  my_df0$s <- NULL
  my_ct0$s <- NULL

  expect_equivalent(as.casTable(caz, my_df0, casOut = list(replace = TRUE)), my_ct0)
  # drop date column
  my_df0$d <- NULL
  my_ct0$d <- NULL
  expect_equivalent(as.casTable(caz, my_df0, casOut = list(replace = TRUE)), my_ct0)
})

test_that("drop multiple columns, missing data", {
  my_df0 <- df0
  my_ct0 <- ct0

  my_df0[3:6] <- list(NULL)
  my_ct0[3:6] <- list(NULL)

  my_ct_df0 <- to.casDataFrame(my_ct0)
  my_ct_df0 <- to.data.frame(my_ct_df0)
  expect_equivalent(my_df0, my_ct_df0)
  expect_equivalent(dimnames(my_df0), dimnames(my_ct_df0))
})

### other drop methods
test_that("drop by column number", {
  my_df <- df
  my_ct <- ct

  # drop number column
  my_df <- df[-2]
  my_ct <- ct[-2]
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)

  # drop character column
  my_df <- df[-4]
  my_ct <- ct[-4]
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)

  # drop date column
  my_df <- df[-4]
  my_ct <- ct[-4]
  expect_equivalent(as.casTable(caz, my_df, casOut = list(replace = TRUE)), my_ct)
})

test_that("drop multiple columns", {
  my_df <- df[- (3:6)]
  my_ct <- ct[- (3:6)]

  my_ct_df <- to.casDataFrame(my_ct)
  my_ct_df <- to.data.frame(my_ct_df)
  expect_equivalent(my_df, my_ct_df)
  expect_equivalent(dimnames(my_df), dimnames(my_ct_df))
})

# Create object to reference an existing in-memory table in CAS
test_that("defCasTable", {
  esophct <- as.casTable(caz, esoph, casOut = list(name = "esophct", replace = TRUE))
  df_cmpct <- as.casTable(caz, df_cmp, casOut = list(name = "df_cmpct", replace = TRUE))

  # groupby option
  esophct_grouped <- defCasTable(caz, tablename = "esophct", groupby = list("alcgp"))
  expect_silent(esophct_grouped)

  df_cmpct_grouped <- defCasTable(caz, tablename = "df_cmpct", groupby = list("s"))
  expect_silent(df_cmpct_grouped)

  # orderby
  esophct_orderby <- defCasTable(caz, tablename = "esophct", orderby = list("ncontrols"))
  esophr_orderby <- esoph[order(esoph$ncontrols), ]
  expect_equivalent(esophct_orderby, as.casTable(caz, esophr_orderby))

  df_cmpct_orderby <- defCasTable(caz, tablename = "df_cmpct", orderby = list("s"))
  df_cmpr_orderby <- df_cmp[order(df_cmp$s), ]
  expect_equivalent(df_cmpct_orderby, as.casTable(caz, df_cmpr_orderby))


  # where option
  esophct_where <- defCasTable(caz, tablename = "esophct", where = "agegp='25-34'")
  esophr_where <- esoph[esoph$agegp == "25-34", ]
  expect_equivalent(esophct_where, as.casTable(caz, esophr_where))

  df_cmpct_where <- defCasTable(caz, tablename = "df_cmpct", where = "cv3<=15")
  df_cmpr_where <- df_cmp[df_cmp$cv3 <= 15, ]
  expect_equivalent(df_cmpct_where, as.casTable(caz, df_cmpr_where))


  # columns option
  esophct_cols1 <- defCasTable(caz, tablename = "esophct", columns = c("ncases"))
  esophr_cols1 <- esoph[4]
  expect_equivalent(esophct_cols1, as.casTable(caz, esophr_cols1))

  df_cmpct_cols1 <- defCasTable(caz, tablename = "df_cmpct", columns = c("n1"))
  df_cmpr_cols1 <- df_cmp[1]
  expect_equivalent(df_cmpct_cols1, as.casTable(caz, df_cmpr_cols1))

  # more than on column specified for the columns option
  esophct_cols2 <- defCasTable(caz, tablename = "esophct", columns = c("agegp", "alcgp", "tobgp"))
  esophr_cols2 <- esoph[1:3]
  expect_equivalent(esophct_cols2, as.casTable(caz, esophr_cols2))

  df_cmpct_cols2 <- defCasTable(caz, tablename = "df_cmpct", columns = c("n1", "s", "cv2"))
  df_cmpr_cols2 <- df_cmp[c(1, 5, 8)]
  expect_equivalent(df_cmpct_cols2, as.casTable(caz, df_cmpr_cols2))
})


test_that("as.castable and dropTable", {
  # Testing that an existing CAS table can't be overwriten by default
  df_cmpct1 <- as.casTable(caz, df_cmp, casOut = list(name = "df_cmpct1", replace = TRUE))
  expect_error(as.casTable(caz, df, casOut = "df_cmpct1"))

  # Testing that an existing CAS table can't be overwriten with replace=FALSE
  expect_error(as.casTable(caz, df, casOut = list(name = "df_cmpct1", replace = FALSE)))

  dropTable(df_cmpct1)

  ti <- cas.table.tableInfo(caz)$TableInfo
  expect_equivalent(nrow(ti[ti$Name == "DF_CMPCT1", ]), 0)

  as.casTable(caz, df_cmp, casOut = list(name = "df_cmpct1", replace = FALSE))

  ti <- cas.table.tableInfo(caz)$TableInfo
  expect_equivalent(nrow(ti[ti$Name == "DF_CMPCT1", ]), 1)
  create_time <- ti[ti$Name == "DF_CMPCT1", ]$CreateTime

  # Testing that an existing CAS table can be overwriten with replace option
  df_cmpct1 <- as.casTable(caz, df_cmp, casOut = list(name = "df_cmpct1", replace = TRUE))

  ti <- cas.table.tableInfo(caz)$TableInfo
  expect_equivalent(nrow(ti[ti$Name == "DF_CMPCT1", ]), 1)
  expect_true(ti[ti$Name == "DF_CMPCT1", ]$CreateTime > create_time)
})


test_that("to.casDataFrame, as.data.frame, and rownames", {
  df_cmpct <- as.casTable(caz, df_cmp, casOut = list(name = "df_cmpct", replace = TRUE))
  df_cmp_cdf <- to.casDataFrame(df_cmpct)
  df_cmp_rdf <- as.data.frame(df_cmp_cdf)
  df_cmp_cdf2 <- to.casDataFrame(df_cmpct, obs = 3)
  df_cmp_rdf2 <- df_cmp_rdf[1:3, ]
  rnms <- rownames(df_cmp_cdf)
  rnms2 <- rownames(df_cmp_cdf2)


  # to.casDataFrame w/ obs= option
  expect_equivalent(to.casDataFrame(df_cmpct, obs = 3), df_cmp_rdf[1:3, ])

  # testing that rownames (which just numbers the rows) matches the nrows
  expect_equivalent(as.numeric(tail(rnms, n = 1)), nrow(df_cmp_rdf))
  expect_equivalent(as.numeric(tail(rnms2, n = 1)), nrow(df_cmp_rdf2))
})
