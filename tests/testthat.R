library(testthat) 
library(swat)

options(testthat.default_reporter='tap',
        cas.print.messages=FALSE,
        cas.gen.function.sig=FALSE)

# special r-universe variable
# used to avoid to run tests in the r-universe CI
if (Sys.getenv("UNIVERSE_NAME") != "sassoftware") {
  test_check("swat")
}
