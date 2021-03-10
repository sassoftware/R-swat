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


context("test.table_functions.R")

test_that("unique", {
  # Confirming the overwritten function still works.
  # unique(data.frame) returns a data.frame
  expect_that(unique(df), is_a("data.frame"))

  # unique(CASTable) returns a CAS data.frame
  expect_is(unique(i2), "data.frame")

  # Numeric uniques
  expect_is(unique(i2[1]), "numeric")
  expect_is(unique(i2$Sepal.Length), "numeric")
  expect_equivalent(unique(i2[1]), unique(i2$Sepal.Length))

  # Character uniques
  expect_is(unique(i2[5]), "character")
  expect_is(unique(i2$Species), "character")
  expect_equivalent(unique(i2[5]), unique(i2$Species))

  # unique(CASTable) returns sorted values. unique(data.frame) does not.
  expect_failure(expect_equivalent(unique(i2$Sepal.Width), unique(iris$Sepal.Width)))
  expect_equivalent(sort(unique(i2$Sepal.Width)), sort(unique(iris$Sepal.Width)))
})

test_that("subset", {
  expect_equivalent(
    subset(ct, subset = ct$n4 > 15, select = c("n1", "n4", "s"), drop = FALSE),
    as.CASTable(caz, subset(df, subset = df$n4 > 15, select = c("n1", "n4", "s"), drop = FALSE),
      casOut = list(name = "foo", replace = TRUE)
    )
  )
  expect_equivalent(
    subset(ct, subset = ct$n4 > 15, select = c("n1", "n4", "s"), drop = FALSE),
    subset(ct, subset = ct[4] > 15, select = c("n1", "n4", "s"), drop = FALSE)
  )
  expect_equivalent(
    ct[ct$n4 > 15, c("n1", "n4", "s")],
    subset(ct, subset = ct[4] > 15, select = c("n1", "n4", "s"), drop = FALSE)
  )

  expect_that(
    subset(ct, subset = ct$n4 > 15, select = c("n1", "n4", "s"), drop = FALSE),
    is_a("CASTable")
  )

  expect_equivalent(
    subset(df0_ct, s == "dd"),
    as.CASTable(caz, subset(df0_, s == "dd"), casOut = list(replace = TRUE))
  )

  # Subsetting on character value, , !=
  expect_equivalent(
    subset(df0_ct, s != "dd", drop = FALSE),
    as.CASTable(caz, subset(df0_, s0 != " ", drop = FALSE), casOut = list(replace = TRUE))
  )

  # Subsetting on missing character values, !=
  expect_equivalent(
    subset(df0_ct, s0 != " ", drop = FALSE),
    as.CASTable(caz, subset(df0_, s != "dd", drop = FALSE), casOut = list(replace = TRUE))
  )

  # Subsetting on missing character values, ==
  expect_equivalent(
    subset(df0_ct, s0 == " ", drop = FALSE),
    as.CASTable(caz, subset(df0_, s == "dd", drop = FALSE), casOut = list(replace = TRUE))
  )

  # Subsetting on numeric values, !=
  expect_equivalent(
    subset(df_ct, n5 != 1.2, drop = FALSE),
    as.CASTable(caz, subset(df_, n5 != 1.2, drop = FALSE), casOut = list(replace = TRUE))
  )

  # Row indexing on character values, ==
  expect_equivalent(
    subset(df_ct, n5 == 1.2, drop = FALSE),
    as.CASTable(caz, subset(df_, n5 == 1.2, drop = FALSE), casOut = list(replace = TRUE))
  )

  # Subsetting with multiple conditions, |
  expect_equivalent(
    subset(df_ct, s == "dd" | n4 <= 15, drop = FALSE),
    as.CASTable(caz, subset(df_, s == "dd" | n4 <= 15, drop = FALSE), casOut = list(replace = TRUE))
  )

  # Subsetting with multiple conditions, &
  expect_equivalent(
    subset(df_ct, s == "dd" & n4 <= 15, drop = FALSE),
    as.CASTable(caz, subset(df_, s == "dd" & n4 <= 15, drop = FALSE), casOut = list(replace = TRUE))
  )

  # Subsetting with compVar
  df_$comp1 <- df_$n3 + df_$n4
  df_ct$comp1 <- df_ct$n3 + df_ct$n4
  expect_equivalent(
    subset(df_ct, comp1 < 30, drop = FALSE),
    as.CASTable(caz, subset(df_, comp1 < 30, drop = FALSE), casOut = list(replace = TRUE))
  )
})

test_that("row index", {
  expect_equivalent(
    as.CASTable(caz, df[df$n4 > 15 & df$n1 < 6, c(1, 4, 5)], casOut = list(replace = TRUE)),
    ct[ct$n4 > 15 & ct$n1 < 6, c("n1", "n4", "s")]
  )
  expect_equivalent(
    as.CASTable(caz, df[df$n4 > 15 & df[1] < 6, c(1, 4, 5)], casOut = list(replace = TRUE)),
    ct[ct$n4 > 15 & ct[1] < 6, c("n1", "n4", "s")]
  )
  expect_equivalent(
    as.CASTable(caz, df[df$n4 > 15 | df[1] < 6, c(1, 4, 5)], casOut = list(replace = TRUE)),
    ct[ct$n4 > 15 | ct[1] < 6, c("n1", "n4", "s")]
  )
  expect_equivalent(
    as.CASTable(caz, df[!df$n4 > 15 | df[1] < 6, c(1, 4, 5)], casOut = list(replace = TRUE)),
    ct[!ct$n4 > 15 | ct[1] < 6, c("n1", "n4", "s")]
  )
  expect_equivalent(
    as.CASTable(caz, df[!df$n4 > 15 | df[1] < 6, ], casOut = list(replace = TRUE)),
    ct[!ct$n4 > 15 | ct[1] < 6, ]
  )
  expect_equivalent(
    as.CASTable(caz, df[!df$n4 > 15 | !df[1] < 6, ], casOut = list(replace = TRUE)),
    ct[!ct$n4 > 15 | !ct[1] < 6, ]
  )

  expect_equivalent(
    as.CASTable(caz, df[, c(1, 4, 5)], casOut = list(replace = TRUE)),
    ct[, c("n1", "n4", "s")]
  )
  expect_equivalent(
    as.CASTable(caz, df[c(1, 4, 5)], casOut = list(replace = TRUE)),
    ct[c("n1", "n4", "s")]
  )
  expect_equivalent(ct[c(1, 4, 5)], ct[c("n1", "n4", "s")])
})

test_that("rbind", {
  expect_equivalent(colSums(rbind(ct1, ct2)), colSums(rbind(df1, df2)))
  expect_equivalent(colSums(rbind(ct1, ct1)), colSums(rbind(df1, df1)))
  expect_equivalent(dim(rbind(ct1, ct1, ct3)), c(30, 4))
})

test_that("cbind", {
  expect_equivalent(colSums(cbind(ct1, ct3)), colSums(cbind(df1, df3)))
  expect_equivalent(dim(cbind(ct1, ct3)), c(10, 4))
})

test_that("rbind numeric, different data", {
  df_bind <- rbind(df1, df2)
  ct_bind <- rbind(ct1, ct2)

  ct_df <- as.data.frame(ct_bind)
  df_bind <- df_bind[order(df_bind$X1, df_bind$X2), ]
  ct_df <- ct_df[order(ct_df$X1, ct_df$X2), ]

  expect_equivalent(df_bind, ct_df)
})

test_that("rbind numeric, repeated data", {
  df_bind <- rbind(df1, df1)
  ct_bind <- rbind(ct1, ct1)

  ct_df <- as.data.frame(ct_bind)
  df_bind <- df_bind[order(df_bind$X1, df_bind$X2), ]
  ct_df <- ct_df[order(ct_df$X1, ct_df$X2), ]

  expect_equivalent(df_bind, ct_df)
})

test_that("rbind numeric and character with missing", {
  df_bind <- rbind(df, df0)
  ct_bind <- rbind(ct, ct0)
  expect_equivalent(as.CASTable(caz, df_bind, casOut = list(replace = TRUE)), ct_bind)
})

test_that("cbind numeric, different data", {
  df_bind <- cbind(df1, df3)
  ct_bind <- cbind(ct1, ct3)
  ct_df <- as.data.frame(ct_bind)
  expect_equivalent(df_bind, ct_df)
})

test_that("cbind numeric and character with missing", {
  df_bind <- cbind(df1, df3)
  ct_bind <- cbind(ct1, ct3)
  expect_equivalent(as.CASTable(caz, df_bind, casOut = list(replace = TRUE)), ct_bind)
})

test_that("cbind numeric and character with missing", {
  df_bind <- cbind(df1, df3)
  ct_bind <- cbind(ct1, ct3)

  ct_df <- as.data.frame(ct_bind)
  expect_equivalent(df_bind, ct_df)
})

test_that("cbind failure since duplicate column names", {
  expect_error(cbind(ct1, ct2))
})

test_that("Test that names returns the correct values", {
  # Columns names to test against
  columns <- c("a", "b", "c", "d", "e", "f")

  # Make a copy of an existing data frame
  my_df <- df

  # Set the column names on the data frame
  names(my_df) <- columns

  # Store the value returned by names on the data frame
  names_df_orig <- names(my_df)

  # Create a CAS table from the R data frame
  my_ct <- as.CASTable(caz, my_df, casOut = list(replace = TRUE))

  # Get the columns names from the CAS table
  names_ct_orig <- names(my_ct)

  # The column names from the CAS table should match the column names added to
  # the data frame
  expect_equivalent(columns, names_ct_orig)

  # Specify specific columns to return
  expect_equivalent(columns[1:4], names(my_ct[1:4]))

  # The column names from the CAS table should match the column names from the data frame
  expect_equivalent(names_df_orig, names_ct_orig)

  # names(CASTable) <- VECTOR cannot be used to set the column names for a CAS Table
  expect_error(names(my_ct) <- columns)
  expect_error(names(my_ct) <- NULL)
})

test_that("Test that colnames returns the correct values", {
  # Columns names to test against
  columns <- c("a", "b", "c", "d", "e", "f")

  # Make a copy of an existing data frame
  my_df <- df

  # Set the column names on the data frame
  colnames(my_df) <- columns

  # Store the value returned by names on the data frame
  colnames_df_orig <- colnames(my_df)

  # Create a CAS table from the R data frame
  my_ct <- as.CASTable(caz, my_df, casOut = list(replace = TRUE))

  # Get the columns names from the CAS table
  colnames_ct_orig <- colnames(my_ct)

  # Specify specific columns to return
  expect_equivalent(columns[2:5], colnames(my_ct[2:5]))

  # The column names from the CAS table should match the column names from the data frame
  expect_equivalent(colnames_df_orig, colnames_ct_orig)

  # names(CASTable) <- VECTOR cannot be used to set the column names for a CAS Table
  expect_error(colnames(my_ct) <- columns)
  expect_error(colnames(my_ct) <- NULL)
})

test_that("Test that names and colnames return the same values", {
  # Make a copy of a CASTable object
  my_ct <- ct

  # Get the orignal column names
  names_orig <- names(my_ct)
  colnames_orig <- colnames(my_ct)
  expect_equivalent(names_orig, colnames_orig)
})

test_that("Test that nrows returns the correct value", {
  # ct has six rows.
  expect_equivalent(6, nrow(ct))

  # Does subsetting columns return correct number of rows
  expect_equivalent(6, nrow(ct[2:5]))
})

test_that("Test that length returns the correct values for CASTables", {
  # length(CASTable) returns the number of columns.
  expect_equivalent(6, length(ct))
  expect_equivalent(4, length(ct[2:5]))
  expect_equivalent(1, length(ct[1]))
  expect_equivalent(1, length(ct$n1))
})

test_that("Test that length returns the correct values for CAS data.frames", {
  # Pull the CAS data.frame from the ct CASTable
  ct_df <- as.data.frame(ct)

  # length(CAS data.frame) returns the same values as length(data.frame)
  expect_equivalent(length(df), length(ct_df))
  expect_equivalent(length(df$n1), length(ct_df$n1))
  expect_equivalent(length(df[1]), length(ct_df[1]))
  expect_equivalent(length(df[2:5]), length(ct_df[2:5]))
})

test_that("Test that ncol returns the correct values for CASTables", {
  # length(CASTable) returns the number of columns.
  expect_equivalent(6, ncol(ct))
  expect_equivalent(4, ncol(ct[2:5]))
  expect_equivalent(1, ncol(ct[1]))
  expect_equivalent(1, ncol(ct$n1))
})

test_that("Test that ncol returns the correct values for CAS data.frames", {
  # Pull the CAS data.frame from the ct CASTable
  ct_df <- as.data.frame(ct)

  # length(CAS data.frame) returns the same values as length(data.frame)
  expect_equivalent(ncol(df), ncol(ct_df))
  expect_equivalent(ncol(df$n1), ncol(ct_df$n1))
  expect_equivalent(ncol(df[1]), ncol(ct_df[1]))
  expect_equivalent(ncol(df[2:5]), ncol(ct_df[2:5]))
})

test_that("Test that dim returns the correct values for CASTables", {
  # dim(CASTable) returns the same values as dim(data.frame) for most cases
  expect_equivalent(dim(df), dim(ct))
  expect_equivalent(dim(df[1]), dim(ct[1]))
  expect_equivalent(dim(df[2:5]), dim(ct[2:5]))

  # dim(data.frame$column) returns: NULL
  # dim(CASTable$column) returns: rows 1
  expect_failure(expect_equivalent(dim(df$n1), dim(ct$n1)))
})

test_that("Test that dim returns the correct values for CAS data.frames", {
  # Pull the CAS data.frame from the ct CASTable
  ct_df <- as.data.frame(ct)

  # dim(CAS data.frame) returns the same values as dim(data.frame)
  expect_equivalent(dim(df), dim(ct_df))
  expect_equivalent(dim(df$n1), dim(ct_df$n1))
  expect_equivalent(dim(df[1]), dim(ct_df[1]))
  expect_equivalent(dim(df[2:5]), dim(ct_df[2:5]))
})
