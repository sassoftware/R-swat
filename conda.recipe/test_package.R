junit_xml <- paste(Sys.getenv("TEST_REPORT_DIR"),
                   Sys.getenv("R_BASE"), "-", Sys.getenv("R_VERSION"),
                   "-junit.xml", sep="")
test_dir <- system.file("tests", "testthat", package = "swat")

testthat::test_dir(test_dir, stop_on_failure=TRUE) #,
                  #reporter=testthat::JunitReporter$new(file=junit_xml))
