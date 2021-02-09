# test.analysis_compvars.R

library(swat)

options(cas.print.messages = FALSE)

.num_cols <- function(x) {
  if (class(x) != "data.frame") stop("parameter must be a data frame")
  return(names(x)[sapply(names(x), function(y) !(class(x[, y]) %in% c("character", "factor")))])
}

context("test.analysis_compvars.R")

test_that("unique", {
  # Computed columns are not supported with the unique function
  expect_error(expect_equivalent(unique(ct_cmp$n1), unique(ct_cmp$cv1)))
  expect_error(expect_equivalent(unique(ct_cmp$cv1), unique(df_cmp$cv1)))
  expect_error(expect_equivalent(unique(ct_cmp[c(1:3, 6:8)]), unique(df_cmp[c(1:3, 6:8)])))
})

test_that("subset", {
  expect_equivalent(
    subset(ct_cmp, subset = ct_cmp$cv1 > 15,
           select = c("n1", "n2", "cv1"), drop = FALSE),
    as.CASTable(caz, subset(df_cmp, subset = df_cmp$cv2 > 15,
                            select = c("n1", "n2", "cv1"), drop = FALSE),
                casOut = list(name = "foobar", replace = TRUE)
    )
  )
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
  expect_equivalent(dim(df_cmp), dim(ct_cmp))
  expect_equivalent(dim(df_cmp[1]), dim(ct_cmp[1]))
  expect_equivalent(dim(df_cmp[4:8]), dim(ct_cmp[4:8]))
})

test_that("min", {
  cmin <- cas.min(t)
  dfmin <- sapply(titanic, min)
  expect_true(length(names(cmin)) > 0)
  expect_equivalent(cmin, dfmin)
  expect_equivalent(names(cmin), names(dfmin))
  expect_equivalent(class(cmin), "character")

  cmin <- cas.min(t, na.rm = TRUE)
  dfmin <- sapply(titanic, min, na.rm = TRUE)
  expect_true(length(names(cmin)) > 0)
  expect_equivalent(cmin, dfmin)
  expect_equivalent(names(cmin), names(dfmin))
  expect_equivalent(class(cmin), "character")

  nums <- .num_cols(titanic)
  cmin <- cas.min(t[, nums], na.rm = TRUE)
  dfmin <- sapply(titanic[, nums], min, na.rm = TRUE)
  expect_equivalent(cmin, dfmin)
  expect_true(length(names(cmin)) > 0)
  expect_equivalent(names(cmin), nums)
  expect_equivalent(names(cmin), names(dfmin))
  expect_equivalent(class(cmin), "numeric")

  expect_error(min(t))

  nums <- .num_cols(titanic)
  expect_equivalent(min(t[, nums]), min(titanic[, nums]))
  expect_equivalent(min(t[, nums], na.rm = TRUE), min(titanic[, nums], na.rm = TRUE))
})

test_that("max", {
  cmax <- cas.max(t)
  dfmax <- sapply(titanic, max)
  expect_true(length(names(cmax)) > 0)
  expect_equivalent(cmax, dfmax)
  expect_equivalent(names(cmax), names(dfmax))
  expect_equivalent(class(cmax), "character")

  cmax <- cas.max(t, na.rm = TRUE)
  dfmax <- sapply(titanic, max, na.rm = TRUE)
  expect_true(length(names(cmax)) > 0)
  expect_equivalent(cmax, dfmax)
  expect_equivalent(names(cmax), names(dfmax))
  expect_equivalent(class(cmax), "character")

  nums <- .num_cols(titanic)
  cmax <- cas.max(t[, nums], na.rm = TRUE)
  dfmax <- sapply(titanic[, nums], max, na.rm = TRUE)
  expect_equivalent(cmax, dfmax)
  expect_true(length(names(cmax)) > 0)
  expect_equivalent(names(cmax), nums)
  expect_equivalent(names(cmax), names(dfmax))
  expect_equivalent(class(cmax), "numeric")

  expect_error(max(t))

  nums <- .num_cols(titanic)
  expect_equivalent(max(t[, nums]), max(titanic[, nums]))
  expect_equivalent(max(t[, nums], na.rm = TRUE), max(titanic[, nums], na.rm = TRUE))
})

test_that("mean", {
  nums <- .num_cols(titanic)
  titanic2 <- titanic[, nums]

  cmean <- cas.mean(t)
  dfmean <- sapply(titanic2, mean)
  expect_true(length(names(cmean)) > 0)
  expect_equivalent(cmean, dfmean)
  expect_equivalent(names(cmean), names(dfmean))
  expect_equivalent(class(cmean), "numeric")

  cmean <- cas.mean(t, na.rm = TRUE)
  dfmean <- sapply(titanic2, function(x) mean(x, na.rm = TRUE))
  expect_true(length(names(cmean)) > 0)
  expect_equivalent(cmean, dfmean)
  expect_equivalent(names(cmean), names(dfmean))
  expect_equivalent(class(cmean), "numeric")

  expect_error(mean(t))

  # Age contains NaNs
  expect_true(is.na(mean(t[, nums])))
  expect_equivalent(mean(t[, nums], na.rm = TRUE), 74.33830351)
})

test_that("colMeans", {
  nums <- .num_cols(titanic)

  expect_equivalent(colMeans(t[, nums]), colMeans(titanic[, nums]))
  expect_equivalent(colMeans(t[, nums], na.rm = TRUE), colMeans(titanic[, nums], na.rm = TRUE))

  expect_error(colMeans(t))
})

test_that("median", {
  nums <- .num_cols(titanic)
  titanic2 <- titanic[, nums]

  cmed <- cas.median(t)
  dfmed <- sapply(titanic2, median)
  expect_true(length(names(cmed)) > 0)
  expect_equivalent(cmed, dfmed)
  expect_equivalent(names(cmed), names(dfmed))
  expect_equivalent(class(cmed), "numeric")

  cmed <- cas.median(t, na.rm = TRUE)
  dfmed <- sapply(titanic2, function(x) median(x, na.rm = TRUE))
  expect_true(length(names(cmed)) > 0)
  expect_equivalent(cmed, dfmed)
  expect_equivalent(names(cmed), names(dfmed))
  expect_equivalent(class(cmed), "numeric")

  expect_error(median(t))

  # Age contains NaNs
  expect_true(is.na(median(t["Age"])))
  expect_equivalent(
    median(t["Age"], na.rm = TRUE),
    median(titanic["Age"][!is.na(titanic["Age"])])
  )
})

test_that("quantile", {
  nums <- .num_cols(titanic)
  titanic2 <- titanic[, nums]

  q <- c(0, 10, 75, 100)
  cq <- cas.quantile(t, q = q)
  dfq <- sapply(titanic2, quantile, probs = q / 100, na.rm = TRUE, type = 1)
  dfq[, "Age"] <- NA
  expect_true(length(colnames(cq)) > 0)
  expect_equivalent(cq, dfq)
  expect_equivalent(colnames(cq), colnames(dfq))
  expect_equivalent(class(cq), c("matrix", "array"))

  cq <- cas.quantile(t, q = q, na.rm = TRUE)
  dfq <- sapply(titanic2, quantile, na.rm = TRUE, probs = q / 100, type = 1)
  expect_true(length(colnames(cq)) > 0)
  expect_equivalent(cq, dfq)
  expect_equivalent(colnames(cq), colnames(dfq))
  expect_equivalent(class(cq), c("matrix", "array"))

  expect_error(quantile(t))

  # Age contains NaNs
  expect_true(all(is.na(quantile(t["Age"]))))
  expect_equivalent(
    quantile(t["Age"], probs = q / 100, na.rm = TRUE),
    quantile(titanic["Age"], probs = q / 100, na.rm = TRUE, type = 1)
  )
})

test_that("mode", {
  m <- cas.mode(t, na.rm = TRUE)
  expect_equivalent(class(m), "data.frame")
  expect_equivalent(names(t), names(m))
  expect_equivalent(nrow(m), 25)

  expect_true(all(m[, "PassengerId"] == 891:867))

  survived <- as.vector(na.omit(m[, "Survived"]))
  expect_equivalent(length(survived), 1)
  expect_equivalent(survived, 0)

  pclass <- as.vector(na.omit(m[, "Pclass"]))
  expect_equivalent(length(pclass), 1)
  expect_equivalent(pclass, 3)

  sex <- m[, "Sex"]
  sex <- sex[sex != ""]
  expect_equivalent(length(sex), 1)
  expect_equivalent(sex, "male")

  age <- as.vector(na.omit(m[, "Age"]))
  expect_equivalent(length(age), 1)
  expect_equivalent(age, 24)

  sibsp <- as.vector(na.omit(m[, "SibSp"]))
  expect_equivalent(length(sibsp), 1)
  expect_equivalent(sibsp, 0)

  parch <- as.vector(na.omit(m[, "Parch"]))
  expect_equivalent(length(parch), 1)
  expect_equivalent(parch, 0)

  ticket <- m[, "Ticket"]
  ticket <- ticket[ticket != ""]
  expect_equivalent(length(ticket), 3)
  expect_equivalent(ticket, c("CA. 2343", "347082", "1601"))

  fare <- as.vector(na.omit(m[, "Fare"]))
  expect_equivalent(length(fare), 1)
  expect_equivalent(fare, 8.05)

  cabin <- m[, "Cabin"]
  cabin <- cabin[cabin != ""]
  expect_equivalent(length(cabin), 3)
  expect_equivalent(cabin, c("G6", "C23 C25 C27", "B96 B98"))

  embarked <- m[, "Embarked"]
  embarked <- embarked[embarked != ""]
  expect_equivalent(length(embarked), 1)
  expect_equivalent(embarked, "S")

  # With missing values
  m <- cas.mode(t, na.rm = FALSE)
  expect_equivalent(class(m), "data.frame")
  expect_equivalent(names(t), names(m))
  expect_equivalent(nrow(m), 25)

  age <- as.vector(na.omit(m[, "Age"]))
  expect_equivalent(length(age), 0)

  cabin <- m[, "Cabin"]
  cabin <- cabin[cabin != ""]
  expect_equivalent(length(cabin), 0)

  # With max.tie = 9
  m <- cas.mode(t, na.rm = FALSE, max.tie = 9)
  expect_equivalent(class(m), "data.frame")
  expect_equivalent(names(t), names(m))
  expect_equivalent(nrow(m), 9)

  # With max.tie = -9
  expect_error(cas.mode(t, na.rm = FALSE, max.tie = -9))
})

test_that("tvalue", {
  ct <- cas.tvalue(t, na.rm = TRUE)
  nums <- .num_cols(titanic)
  dft <- sapply(nums, function(x) t.test(titanic[, x])$statistic)
  expect_true(length(names(ct)) > 0)
  expect_equivalent(names(ct), nums)
  expect_equivalent(ct, dft)

  # Check missing values
  ct <- cas.tvalue(t)
  filt <- sapply(ct, is.na)
  expect_equivalent(names(ct[filt]), "Age") 
})

test_that("sum", {
  nums <- .num_cols(titanic)

  cout <- cas.sum(t)
  dfout <- sapply(titanic[, nums], sum)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)

  # na.rm = TRUE
  cout <- cas.sum(t, na.rm = TRUE)
  dfout <- sapply(titanic[, nums], sum, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)

  # sum
  expect_equivalent(sum(t), sum(titanic[, nums]))
  expect_equivalent(sum(t, na.rm = TRUE), sum(titanic[, nums], na.rm = TRUE))
})

test_that("colSums", {
  nums <- .num_cols(titanic)

  expect_equivalent(colSums(t[, nums]), colSums(titanic[, nums]))
  expect_equivalent(colSums(t[, nums], na.rm = TRUE), colSums(titanic[, nums], na.rm = TRUE))

  expect_error(colSums(t))
})

test_that("sd", {
  nums <- .num_cols(titanic)

  cout <- cas.sd(t)
  dfout <- sapply(titanic[, nums], sd)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)

  # na.rm = TRUE
  cout <- cas.sd(t, na.rm = TRUE)
  dfout <- sapply(titanic[, nums], sd, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)
})

test_that("var", {
  nums <- .num_cols(titanic)

  cout <- cas.var(t)
  dfout <- sapply(titanic[, nums], var)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)

  # na.rm = TRUE
  cout <- cas.var(t, na.rm = TRUE)
  dfout <- sapply(titanic[, nums], var, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)
})

test_that("nmiss", {
  cout <- cas.nmiss(t)
  dfout <- sapply(titanic, function(x) sum(is.na(x)))
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("count", {
  cout <- cas.count(t)
  dfout <- sapply(titanic, function(x) sum(!is.na(x)))
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), names(dfout))
  expect_equivalent(cout, dfout)
})

test_that("stderr", {
  # TODO: Need a way to verify numbers
  nums <- .num_cols(titanic)

  cout <- cas.stderr(t)
  # dfout <- sapply(titanic[, nums], stderr)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)

  cout <- cas.stderr(t, na.rm = TRUE)
  # dfout <- sapply(titanic[, nums], stderr, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)
})

test_that("uss", {
  # TODO: Need a way to verify numbers
  nums <- .num_cols(titanic)

  cout <- cas.uss(t)
  # dfout <- sapply(titanic[, nums], uss)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)

  cout <- cas.uss(t, na.rm = TRUE)
  # dfout <- sapply(titanic[, nums], uss, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)
})

test_that("css", {
  # TODO: Need a way to verify numbers
  nums <- .num_cols(titanic)

  cout <- cas.css(t)
  # dfout <- sapply(titanic[, nums], css)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)

  cout <- cas.css(t, na.rm = TRUE)
  # dfout <- sapply(titanic[, nums], css, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)
})

test_that("cv", {
  cv <- function(x, na.rm = FALSE) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(100*(sd(x)/mean(x)))
    }
  }

  nums <- .num_cols(titanic)

  cout <- cas.cv(t)
  dfout <- sapply(titanic[, nums], cv)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)

  cout <- cas.cv(t, na.rm = TRUE)
  dfout <- sapply(titanic[, nums], cv, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  expect_equivalent(cout, dfout)
})

test_that("probt", {
  # TODO: Need a way to verify numbers
  nums <- .num_cols(titanic)

  cout <- cas.probt(t)
  # dfout <- sapply(titanic[, nums], uss)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)

  cout <- cas.probt(t, na.rm = TRUE)
  # dfout <- sapply(titanic[, nums], uss, na.rm = TRUE)
  expect_true(length(names(cout)) > 0)
  expect_equivalent(names(cout), nums)
  # expect_equivalent(cout, dfout)
})

test_that("head, tail", {
  titanic.sorted <- titanic[order(titanic$PassengerId), ]
  t.sorted <- cas.copy(t)
  t.sorted@orderby <- c("PassengerId")

  expect_equivalent(head(t.sorted), head(titanic))
  expect_equivalent(head(t.sorted, 4), head(titanic, 4))

  expect_equivalent(tail(t.sorted), tail(titanic))
  expect_equivalent(tail(t.sorted, 4), tail(titanic, 4))
})

test_that("cor", {
  expect_equal(cor(df_cmp[c(1:4, 7:8)], use = "complete"),
               cor(ct_cmp[c(1:4, 7:8)], use = "complete"), tolerance = 1.e-5)
})

test_that("cov", {
  expect_equivalent(cov(ct_cmp$n1, ct_cmp$n2), cov(df_cmp$cv1, df_cmp$cv2))
  expect_equivalent(cov(ct_cmp$n1, ct_cmp$n2), cov(ct_cmp$cv1, ct_cmp$cv2))
  expect_true(all.equal(cov(df_cmp[c(1:4, 7:8)]), cov(ct_cmp[c(1:4, 7:8)]), tolerance = 1.e-6))
})

test_that("summary", {
  expect_true(all.equal(summary(t, factor.threshold = 0),
                        summary(titanic, quantile.type = 1)))
  expect_true(all.equal(summary(ct_cmp, factor.threshold = 0),
                        summary(df_cmp, quantile.type = 1)))

  t2 <- data.frame(titanic)
  cols <- names(t2)[!(names(t2) %in% "Embarked")]
  t2$Sex <- as.factor(t2$Sex)
  expect_true(all.equal(summary(t[, cols]), summary(t2[, cols], quantile.type = 1)))
})
