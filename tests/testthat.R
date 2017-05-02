library(testthat) 
library(swat)

options(testthat.default_reporter='tap',
        cas.print.messages=FALSE,
        cas.gen.function.sig=FALSE)

test_check("swat")
