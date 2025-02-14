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


context("test.rswat.R")

test_that("test.basic_connection", {
  expect_true(caz$hostname == HOSTNAME)
  expect_true(caz$port == PORT)
# expect_true(caz$username == USERNAME)
  expect_true(grepl("^[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}$",
                    caz$session, perl = TRUE))
  expect_true(grepl("\\bprotocol=(https?|cas|auto)\\b", caz$soptions,
                    perl = TRUE))
})

test_that("test.connection_failure", {
  expect_error(CAS(HOSTNAME, 19999, USERNAME, PASSWORD, protocol = PROTOCOL, path=PATH))
})

test_that("connection patterns", {
  if(is.null(PATH) || PATH=='') {
    s1 <- CAS$new(paste(PROTOCOL, '://', HOSTNAME, ':', PORT, sep=''), username=USERNAME, password=PASSWORD)
  } else {
    s1 <- CAS$new(paste(PROTOCOL, '://', HOSTNAME, ':', PORT, '/', PATH,  sep=''), username=USERNAME, password=PASSWORD)
  }

  expect_true(exists("s1"))
  if(is.null(PATH) || PATH=='') {
    s2 <- CAS$new(paste(PROTOCOL, '://', HOSTNAME, ':', PORT, sep=''), NULL, USERNAME, PASSWORD)
  } else {
    s2 <- CAS$new(paste(PROTOCOL, '://', HOSTNAME, ':', PORT, '/', PATH,  sep=''), NULL, USERNAME, PASSWORD)
  }
  expect_true(exists("s2"))

  s3 <- CAS$new(paste(PROTOCOL, '://', HOSTNAME, sep=''), PORT, USERNAME, PASSWORD, path=PATH)
  expect_true(exists("s3"))

  s4 <- CAS$new(HOSTNAME, PORT, USERNAME, PASSWORD, protocol=PROTOCOL, path=PATH)
  expect_true(exists("s4"))

  swat::cas.terminate(s1)
  swat::cas.terminate(s2)
  swat::cas.terminate(s3)
  swat::cas.terminate(s4)
})

test_that('test.results', {
  out <- caz$retrieve('tableinfo')
  expect_true(!is.null(out$performance))
  expect_true(!is.null(out$disposition))
  expect_true(!is.null(out$messages))
  expect_true(!is.null(out$results))

  out <- caz$retrieve('tableinfo', table='#$&^*#*^$#@aontehu')
  expect_true(!is.null(out$performance))
  expect_true(!is.null(out$disposition))
  expect_true(out$disposition$severity == 2)
  expect_true(!is.null(out$messages))
  expect_true(length(grepl('^ERROR', out$messages, perl=TRUE)) > 0)
  expect_true(!is.null(out$results))
})

test_that("test.copy_connection", {
  s2 <- caz$copy()
  expect_true(s2$hostname == caz$hostname)
  expect_true(s2$port == caz$port)
  expect_true(s2$username == caz$username)
  expect_true(s2$session != caz$session)
  expect_true(grepl("^[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}$",
                    s2$session, perl = TRUE))
  expect_true(caz$soptions == s2$soptions)
  swat::cas.terminate(s2)
})

test_that("test.fork_connection", {
  slist = caz$fork(3)
  expect_true(length(slist) == 3)
  expect_true(slist[[1]]$hostname == caz$hostname)
  expect_true(slist[[1]]$port == caz$port)
  expect_true(slist[[1]]$username == caz$username)
  expect_true(slist[[1]]$session == caz$session)
  expect_true(slist[[1]]$soptions == caz$soptions)
  expect_true(slist[[2]]$hostname == caz$hostname)
  expect_true(slist[[2]]$port == caz$port)
  expect_true(slist[[2]]$username == caz$username)
  expect_true(slist[[2]]$session != caz$session)
  expect_true(grepl("^[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}$",
                    slist[[2]]$session, perl = TRUE))
  expect_true(slist[[2]]$soptions == caz$soptions)
  expect_true(slist[[3]]$hostname == caz$hostname)
  expect_true(slist[[3]]$port == caz$port)
  expect_true(slist[[3]]$username == caz$username)
  expect_true(slist[[3]]$session != caz$session)
  expect_true(grepl("^[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}$",
                    slist[[3]]$session, perl = TRUE))
  expect_true(slist[[3]]$soptions == caz$soptions)

  # Note, slist[[1]] is the original connection.  Don't terminate
  swat::cas.terminate(slist[[2]])
  swat::cas.terminate(slist[[3]])
})

test_that("test.connect_with_bad_session", {
  expect_error(CAS(HOSTNAME, PORT, USERNAME, PASSWORD, protocol = PROTOCOL,
                   session = "bad-session"))
})

test_that("test.set_session_locale", {
  u = CAS(HOSTNAME, PORT, USERNAME, PASSWORD, protocol = PROTOCOL,
          locale = "es_US", path=PATH)
  expect_true(grepl("\\blocale=es_US\\b", u$soptions, perl = TRUE))
  swat::cas.terminate(u)
})

test_that("test.set_bad_session_locale", {
  expect_error(CAS(HOSTNAME, PORT, USERNAME, PASSWORD, protocol = PROTOCOL,
                   locale = "bad-locale"))
})

test_that("test.test_echo", {
  out <- caz$retrieve("echo", a1 = 10, b1 = 12.5, c1 = "string value",
                      d1 = list(1, 2, 3), e1 = list(x1 = 100, y1 = "y-value",
                                                    z1 = list(20.5, 1.75)))
  d <- out$results
  expect_true(d$a1 == 10)
  expect_true(d$b1 == 12.5)
  expect_true(d$c1 == "string value")
  expect_true(length(d$d1) == 3)
  expect_true(d$d1[[1]] == 1)
  expect_true(d$d1[[2]] == 2)
  expect_true(d$d1[[3]] == 3)
  expect_true(length(d$e1) == 3)
  expect_true(d$e1$x1 == 100)
  expect_true(d$e1$y1 == "y-value")
  expect_true(length(d$e1$z1) == 2)
  expect_true(d$e1$z1[[1]] == 20.5)
  expect_true(d$e1$z1[[2]] == 1.75)

  out <- caz$retrieve("echo", a1 = 10, b1 = 12.5, c1 = "string value",
                      d1 = c(1, 2, 3), e1 = list(x1 = 100, y1 = "y-value",
                                                    z1 = c(20.5, 1.75)))
  d <- out$results
  expect_true(d$a1 == 10)
  expect_true(d$b1 == 12.5)
  expect_true(d$c1 == "string value")
  expect_true(length(d$d1) == 3)
  expect_true(d$d1[[1]] == 1)
  expect_true(d$d1[[2]] == 2)
  expect_true(d$d1[[3]] == 3)
  expect_true(length(d$e1) == 3)
  expect_true(d$e1$x1 == 100)
  expect_true(d$e1$y1 == "y-value")
  expect_true(length(d$e1$z1) == 2)
  expect_true(d$e1$z1[[1]] == 20.5)
  expect_true(d$e1$z1[[2]] == 1.75)

  out <- caz$retrieve("echo", a1 = 10, b1 = 12.5, c1 = "string value",
                      d1 = c('one', 'two', 'three'),
                      e1 = list(x1 = 100, y1 = "y-value",
                                z1 = list(num=c(20.5, 1.75))),
                      f1 = list('one', 'two', 'three'))
  d <- out$results
  expect_true(d$a1 == 10)
  expect_true(d$b1 == 12.5)
  expect_true(d$c1 == "string value")
  expect_true(length(d$d1) == 3)
  expect_true(d$d1[[1]] == 'one')
  expect_true(d$d1[[2]] == 'two')
  expect_true(d$d1[[3]] == 'three')
  expect_true(length(d$e1) == 3)
  expect_true(d$e1$x1 == 100)
  expect_true(d$e1$y1 == "y-value")
  expect_true(length(d$e1$z1) == 1)
  expect_true(length(d$e1$z1$num) == 2)
  expect_true(d$e1$z1$num[[1]] == 20.5)
  expect_true(d$e1$z1$num[[2]] == 1.75)
  expect_true(d$f1[[1]] == 'one')
  expect_true(d$f1[[2]] == 'two')
  expect_true(d$f1[[3]] == 'three')
})

test_that("test.test_summary", {
  out <- caz$retrieve("queryCaslib",caslib="castesttmp")
  if ( out$results[[1]] == 0 )
    # caslib castesttmp not present on udanext deployments
    skip("The caslib 'castesttmp' does not exist.")

  out = caz$retrieve("loadactionset", actionset = "simple")
  expect_true(out$disposition$statusCode == 0)
  out = caz$retrieve("loadtable", path = "datasources/cars_single.sashdat",
                   caslib = "castesttmp")
  expect_true(out$disposition$statusCode == 0)
  out = caz$retrieve("summary", table = list(name = "datasources.cars_single"))
  summ = out$results$Summary
  expect_true(dim(summ)[[2]] >= 15)
  n <- names(summ)
  expect_true(n[[1]] == "Column")
  expect_true(n[[2]] == "Min")
  expect_true(n[[3]] == "Max")
  expect_true(n[[4]] == "N")
  expect_true(n[[5]] == "NMiss")
  expect_true(n[[6]] == "Mean")
  expect_true(n[[7]] == "Sum")
  expect_true(n[[8]] == "Std")
  expect_true(n[[9]] == "StdErr")
  expect_true(n[[10]] == "Var")
  expect_true(n[[11]] == "USS")
  expect_true(n[[12]] == "CSS")
  expect_true(n[[13]] == "CV")
  expect_true(n[[14]] == "TValue")
  expect_true(n[[15]] == "ProbT")
  t <- lapply(summ, class)
  expect_true(t[[1]] == "character")
  expect_true(t[[2]] == "numeric")
  expect_true(t[[3]] == "numeric")
  expect_true(t[[4]] == "numeric" || t[[4]] == "integer")
  expect_true(t[[5]] == "numeric" || t[[5]] == "integer")
  expect_true(t[[6]] == "numeric")
  expect_true(t[[7]] == "numeric")
  expect_true(t[[8]] == "numeric")
  expect_true(t[[9]] == "numeric")
  expect_true(t[[10]] == "numeric")
  expect_true(t[[11]] == "numeric")
  expect_true(t[[12]] == "numeric")
  expect_true(t[[13]] == "numeric")
  expect_true(t[[14]] == "numeric")
  expect_true(t[[15]] == "numeric")
  m <- summ$Min
  expect_true(m[[1]] == 10280)
  expect_true(m[[2]] == 9875)
  expect_true(m[[3]] == 1.3)
  expect_true(m[[4]] == 3)
  expect_true(m[[5]] == 73)
  expect_true(m[[6]] == 10)
  expect_true(m[[7]] == 12)
  expect_true(m[[8]] == 1850)
  expect_true(m[[9]] == 89)
  expect_true(m[[10]] == 143)
  m <- summ$NMiss
  expect_true(m[[1]] == 0)
  expect_true(m[[2]] == 0)
  expect_true(m[[3]] == 0)
  expect_true(m[[4]] == 2)
  expect_true(m[[5]] == 0)
  expect_true(m[[6]] == 0)
  expect_true(m[[7]] == 0)
  expect_true(m[[8]] == 0)
  expect_true(m[[9]] == 0)
  expect_true(m[[10]] == 0)
})

test_that("test.test_alltypes", {
  out = caz$retrieve("loadactionset", actionset = "actiontest")
  if ( out$disposition$statusCode != 0 )
    # actiontest not present on udanext deployments
    skip(out$messages[[1]])

  out = caz$retrieve("alltypes", casout = "typestable")
  out = caz$retrieve("fetch", table = list(name = "typestable"),
                   sastypes = FALSE)
  df = out$results$Fetch
  expect_true(df$Double[[1]] == 42.42)
  expect_true(class(df$Double[[1]]) == "numeric")
  expect_true(df$Char[[1]] == "AbC➂➁➀")
  expect_true(class(df$Char[[1]]) == "character")
  expect_true(df$Varchar[[1]] == "This is a test of the Emergency Broadcast System. This is only a test. BEEEEEEEEEEEEEEEEEEP WHAAAA SCREEEEEEEEEEEECH. ➉➈➇➆➅➄➃➂➁➀ Blastoff!")
  expect_true(class(df$Varchar[[1]]) == "character")
  expect_true(df$Int32[[1]] == 42)
  expect_true(class(df$Int32[[1]]) == "integer")
  expect_true(as.character(df$Int64[[1]]) == "9223372036854775808")
  expect_true(class(df$Int64[[1]]) == "numeric")
  expect_true(df$Date[[1]] == as.Date("1963-05-19", "%Y-%m-%d"))
  expect_true(class(df$Date[[1]]) == "Date")
  op <- options(digits.secs = 7)
  expect_true(as.character(df$Time[[1]]) == as.character(as.POSIXct(strptime("1960-01-01 11:12:13.141516",
                                                                             "%Y-%m-%d %H:%M:%OS"))))
  options(op)
  expect_true(class(df$Time[[1]])[[1]] == "POSIXct")
  expect_true(df$Datetime[[1]] == as.POSIXct(strptime("1963-05-19 11:12:13.141516",
                                                      "%Y-%m-%d %H:%M:%OS", tz = "UTC")))
  expect_true(class(df$Datetime[[1]])[[1]] == "POSIXct")
  expect_true(df$DecSext[[1]] == "12345678901234567890.123456789")
  expect_true(class(df$DecSext[[1]]) == "character")
  expect_true(class(df$Varbinary[[1]]) == "character")
  expect_true(df$Binary[[1]] == "bm9wcXJzdHV2d3h5ent8fX5/gIGCg4SFhoeIiYqLjI0=")
  expect_true(class(df$Binary[[1]]) == "character")
})

test_that("test.test_array_types", {
  out <- caz$retrieve("queryCaslib",caslib="castesttmp")
  if ( out$results[[1]] == 0 )
    # caslib castesttmp not present on udanext deployments
    skip("The caslib 'castesttmp' does not exist.")

  out <- caz$retrieve("loadtable", path = "datasources/summary_array.sashdat",
                    caslib = "castesttmp")
  expect_true(out$disposition$statusCode == 0)
  out <- caz$retrieve("fetch", table = list(name = "datasources.summary_array"),
                    sastypes = FALSE)
  expect_true(out$disposition$statusCode == 0)
  df <- out$results$Fetch
  for (i in 1:14) {
    expect_true(df$"_Min_"[[i]] == df$myArray1[[i]])
    expect_true(df$"_Max_"[[i]] == df$myArray2[[i]])
    expect_true(df$"_N_"[[i]] == df$myArray3[[i]])
    expect_true(df$"_NMiss_"[[i]] == df$myArray4[[i]])
    expect_true(df$"_Mean_"[[i]] == df$myArray5[[i]])
    expect_true(df$"_Sum_"[[i]] == df$myArray6[[i]])
    expect_true(df$"_Std_"[[i]] == df$myArray7[[i]])
    expect_true(df$"_StdErr_"[[i]] == df$myArray8[[i]])
    expect_true(df$"_Var_"[[i]] == df$myArray9[[i]])
    expect_true(df$"_USS_"[[i]] == df$myArray10[[i]])
    expect_true(df$"_CSS_"[[i]] == df$myArray11[[i]])
    expect_true(df$"_CV_"[[i]] == df$myArray12[[i]])
    expect_true(df$"_T_"[[i]] == df$myArray13[[i]])
    expect_true(df$"_PRT_"[[i]] == df$myArray14[[i]])
  }
})

test_that("test.test_multiple_connection_retrieval", {
   out = caz$retrieve("loadactionset", actionset = "actiontest")
   if ( out$disposition$statusCode != 0 )
     # actiontest not present on udanext deployments
     skip(out$messages[[1]])

   f = caz$fork(3)

   expect_true(length(f) == 3)
   expect_true(length(f[[1]]$session) > 0)
   expect_true(length(f[[2]]$session) > 0)
   expect_true(length(f[[3]]$session) > 0)
   expect_true(f[[1]]$session != f[[2]]$session)
   expect_true(f[[2]]$session != f[[3]]$session)

   f[[1]]$retrieve("loadactionset", actionset = "actiontest")
   f[[2]]$retrieve("loadactionset", actionset = "actiontest")
   f[[3]]$retrieve("loadactionset", actionset = "actiontest")
   f[[1]]$invoke("testsleep", duration = 6000)
   f[[2]]$invoke("testsleep", duration = 11000)
   f[[3]]$invoke("testsleep", duration = 500)

   w <- CASEventWatcher(f)

   order <- list()
   while ( TRUE )
   {
      output <- getnext(w)
      if ( is.null(output$response) ) break

      if ( length(output$response$messages) > 0 )
      {
          if ( grepl('500 milliseconds', output$response$messages[[1]]) )
          {
             order <- c(order, f[[3]]$session)
          }
          else if ( grepl('6000 milliseconds', output$response$messages[[1]]) )
          {
             order <- c(order, f[[1]]$session)
          }
          else if ( grepl('11000 milliseconds', output$response$messages[[1]]) )
          {
             order <- c(order, f[[2]]$session)
          }
      }
   }

   f1Found = FALSE
   f2Found = FALSE
   f3Found = FALSE

   # TODO: This is the wrong order.  It should be 3, 1, 2!
   expect_true(length(order) == 3)

   for ( i in 1:length(order) )
   {
       if (order[[i]] == f[[3]]$session)
       {
           f1Found = TRUE
       }
       else if (order[[i]] == f[[1]]$session)
       {
           f2Found = TRUE
       }
       else if (order[[i]] == f[[2]]$session)
       {
           f3Found = TRUE
       }
   }

   expect_true(f1Found)
   expect_true(f2Found)
   expect_true(f3Found)

   # note f[[1]] is the original connection, not a copy.  Don't terminate.
   swat::cas.terminate(f[[2]])
   swat::cas.terminate(f[[3]])
})

test_that("test.test_addtable", {
  if ( PROTOCOL == 'http' || PROTOCOL == 'https' )
      skip('Not implemented in REST interface yet.')

  skip('Fails in unit tests, but not outside them (SIGPIPE occurs)')

  iiris <- cbind(iris, Index = 1:dim(iris)[[1]])

  dmh = CASDataMsgHandler(iiris, nrecs = 20)
  caz$retrieve("addtable", table = "iris", datamsghandler = dmh,
             vars = dmh$vars, reclen = dmh$reclen)

  out = caz$retrieve("tableinfo", table = "iris")
  data = out$results$TableInfo

  expect_true(data$Name[[1]] == "IRIS")
  expect_true(data$Rows[[1]] == 150)
  expect_true(data$Columns[[1]] == 6)

  out = caz$retrieve("columninfo", table = list(name = "iris"))
  data = out$results$ColumnInfo

  expect_true(dim(data)[[1]] == 6)
  expect_true(data$Column[[1]] == "Sepal.Length")
  expect_true(data$Column[[2]] == "Sepal.Width")
  expect_true(data$Column[[3]] == "Petal.Length")
  expect_true(data$Column[[4]] == "Petal.Width")
  expect_true(data$Column[[5]] == "Species")
  expect_true(data$Column[[6]] == "Index")
  expect_true(data$Type[[1]] == "double")
  expect_true(data$Type[[2]] == "double")
  expect_true(data$Type[[3]] == "double")
  expect_true(data$Type[[4]] == "double")
  expect_true(data$Type[[5]] == "varchar")
  expect_true(data$Type[[6]] == "int64")

  out = caz$retrieve("fetch", table = list(name = "iris"),
                   to = 1000)
  data = out$results$Fetch[with(out$results$Fetch, order(Index)), ]

  for (i in 1:length(data)) {
    expect_true(data$Sepal.Length[[i]] == iiris$Sepal.Length[[i]])
    expect_true(data$Sepal.Width[[i]] == iiris$Sepal.Width[[i]])
    expect_true(data$Petal.Length[[i]] == iiris$Petal.Length[[i]])
    expect_true(data$Petal.Width[[i]] == iiris$Petal.Width[[i]])
    expect_true(data$Species[[i]] == iiris$Species[[i]])
    expect_true(data$Index[[i]] == iiris$Index[[i]])
  }
})

test_that("test.test_upload", {
  caz$upload(iris, casout = "iris2")
  out = caz$retrieve("tableinfo", table = "iris2")
  data = out$results$TableInfo
  expect_true(data$Name[[1]] == "IRIS2")
  expect_true(data$Rows[[1]] == 150)
  expect_true(data$Columns[[1]] == 5)
  out = caz$retrieve("columninfo", table = list(name = "iris2"))
  data = out$results$ColumnInfo
  expect_true(dim(data)[[1]] == 5)
  expect_true(data$Column[[1]] == "Sepal.Length")
  expect_true(data$Column[[2]] == "Sepal.Width")
  expect_true(data$Column[[3]] == "Petal.Length")
  expect_true(data$Column[[4]] == "Petal.Width")
  expect_true(data$Column[[5]] == "Species")
  expect_true(data$Type[[1]] == "double")
  expect_true(data$Type[[2]] == "double")
  expect_true(data$Type[[3]] == "double")
  expect_true(data$Type[[4]] == "double")
  expect_true(data$Type[[5]] == "varchar")
})
