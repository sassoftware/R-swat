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


context("test.connection.R")

env_vars <- list(
  CAS_URL = Sys.getenv("CAS_URL"),
  CASURL = Sys.getenv("CASURL"),
  CAS_HOST = Sys.getenv("CAS_HOST"),
  CASHOST = Sys.getenv("CASHOST"),
  CAS_PORT = Sys.getenv("CAS_PORT"),
  CASPORT = Sys.getenv("CASPORT"),
  CAS_USER = Sys.getenv("CAS_USER"),
  CASUSER = Sys.getenv("CASUSER"),
  CASUSERNAME = Sys.getenv("CASUSERNAME"),
  CAS_USERNAME = Sys.getenv("CAS_USERNAME"),
  CAS_PROTOCOL = Sys.getenv("CAS_PROTOCOL"),
  CASPROTOCOL = Sys.getenv("CASPROTOCOL"),
  CAS_TOKEN = Sys.getenv("CAS_TOKEN"),
  CASTOKEN = Sys.getenv("CASTOKEN"),
  CAS_PASSWORD = Sys.getenv("CAS_PASSWORD"),
  CASPASSWORD = Sys.getenv("CASPASSWORD")
)

setup({
  do.call("Sys.unsetenv", list(names(env_vars)))
})

teardown({
  do.call("Sys.setenv", env_vars)
})

test_that("test.connection_info", {
  f <- function() {
    return(swat:::.get_connection_info(NULL, NULL, NULL, NULL, NULL, NULL))
  }
  expect_error(f())

  f <- function() {
    return(swat:::.get_connection_info("cas-server-1.com", NULL, NULL, NULL, NULL, NULL))
  }
  expect_error(f())

  # cas
  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     NULL, NULL, NULL, NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "", password = "", protocol = "cas"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", NULL, NULL, NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "", protocol = "cas"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     NULL, "mytoken", NULL, NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", NULL, NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  # http
  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     NULL, NULL, "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345", port = 12345,
                         username = "", password = "", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", NULL, "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345", port = 12345,
                         username = "myuserid", password = "", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     NULL, "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345", port = 12345,
                         username = "", password = "mytoken", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "http"))

  # cas with path (which means nothing)
  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "", "", NULL, "cas-server/base")
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "", password = "", protocol = "cas"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "", NULL, "cas-server/base")
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "", protocol = "cas"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     NULL, "mytoken", "", "cas-server/base")
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", NULL, "cas-server/base")
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  # http with path
  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "", "", "http", "cas-server/base")
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "", password = "", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "", "http", "cas-server/base")
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "myuserid", password = "", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     NULL, "mytoken", "http", "cas-server/base")
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "", password = "mytoken", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", "http", "cas-server/base")
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "http"))

  # URL with path and separate port
  out <- swat:::.get_connection_info("cas-server-1.com/cas-server/base", 12345,
                                     "", "", "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "", password = "", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com/cas-server/base", 12345,
                                     "myuserid", "", "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "myuserid", password = "", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com/cas-server/base", 12345,
                                     NULL, "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "", password = "mytoken", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com/cas-server/base", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "http"))

  out <- swat:::.get_connection_info("https://cas-server-1.com/cas-server/base", 12345,
                                     "", "", "http", NULL)
  expect_equal(out, list(hostname = "https://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "", password = "", protocol = "https"))

  out <- swat:::.get_connection_info("https://cas-server-1.com/cas-server/base", 12345,
                                     "myuserid", "", "http", NULL)
  expect_equal(out, list(hostname = "https://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "myuserid", password = "", protocol = "https"))

  out <- swat:::.get_connection_info("https://cas-server-1.com/cas-server/base", 12345,
                                     "", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "https://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "", password = "mytoken", protocol = "https"))

  out <- swat:::.get_connection_info("https://cas-server-1.com/cas-server/base", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "https://cas-server-1.com:12345/cas-server/base", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "https"))
})

test_that("test.protocols", {
  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", "auto", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:12345", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "http"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", "https", NULL)
  expect_equal(out, list(hostname = "https://cas-server-1.com:12345", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "https"))

  out <- swat:::.get_connection_info("cas-server-1.com", 12345,
                                     "myuserid", "mytoken", "cas", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  f <- function() {
    return(swat:::.get_connection_info("cas-server-1.com", 12345,
                                       "myuserid", "mytoken", "unknown", NULL))
  }
  expect_error(f())
})

test_that("test.duplicate_parameters", {
  out <- swat:::.get_connection_info("cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "cas", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas://cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas://otheruser:@cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "otheruser", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas://:otherpassword@cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "myuserid", password = "otherpassword", protocol = "cas"))

  out <- swat:::.get_connection_info("cas://otheruser:otherpassword@cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "otheruser", password = "otherpassword", protocol = "cas"))

  out <- swat:::.get_connection_info("http://cas-server-1.com:5570", 12345, "myuserid",
                                     "mytoken", "cas", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:5570", port = 5570,
                         username = "myuserid", password = "mytoken", protocol = "http"))

  out <- swat:::.get_connection_info("http://otheruser:@cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "cas", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:5570", port = 5570,
                         username = "otheruser", password = "mytoken", protocol = "http"))

  out <- swat:::.get_connection_info("http://otheruser:otherpassword@cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "cas", NULL)
  expect_equal(out, list(hostname = "http://cas-server-1.com:5570", port = 5570,
                         username = "otheruser", password = "otherpassword", protocol = "http"))
})

test_that("test.multiple_hosts", {
  f <- function() {
    return(swat:::.get_connection_info(NULL, NULL, NULL, NULL, NULL, NULL))
  }
  expect_error(f())

  f <- function() {
    return(swat:::.get_connection_info(c(
      "cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com"
    ), NULL, NULL, NULL, NULL, NULL))
  }
  expect_error(f())

  # cas
  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, NULL, NULL, NULL)
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "", password = "", protocol = "cas"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", NULL, NULL, NULL)
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "", protocol = "cas"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, "mytoken", NULL, NULL)
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "", password = "mytoken", protocol = "cas"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", "mytoken", NULL, NULL)
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "mytoken", protocol = "cas"
  ))

  # http
  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, NULL, "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345",
      "http://cas-server-2.com:12345",
      "http://cas-server-3.com:12345",
      sep = " "
    ),
    port = 12345, username = "", password = "", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", NULL, "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345",
      "http://cas-server-2.com:12345",
      "http://cas-server-3.com:12345",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345",
      "http://cas-server-2.com:12345",
      "http://cas-server-3.com:12345",
      sep = " "
    ),
    port = 12345, username = "", password = "mytoken", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345",
      "http://cas-server-2.com:12345",
      "http://cas-server-3.com:12345",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "mytoken", protocol = "http"
  ))

  # cas with path (which means nothing)
  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, NULL, NULL, "cas-server/base")
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "", password = "", protocol = "cas"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", NULL, NULL, "cas-server/base")
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "", protocol = "cas"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, "mytoken", NULL, "cas-server/base")
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "", password = "mytoken", protocol = "cas"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", "mytoken", NULL, "cas-server/base")
  expect_equal(out, list(
    hostname = paste("cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "mytoken", protocol = "cas"
  ))

  # http with path
  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, NULL, "http", "cas-server/base")
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      "http://cas-server-3.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "", password = "", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", NULL, "http", "cas-server/base")
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      "http://cas-server-3.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, NULL, "mytoken", "http", "cas-server/base")
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      "http://cas-server-3.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "", password = "mytoken", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com",
    "cas-server-2.com",
    "cas-server-3.com"
  ), 12345, "myuserid", "mytoken", "http", "cas-server/base")
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      "http://cas-server-3.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "mytoken", protocol = "http"
  ))

  # URL with path and separate port
  out <- swat:::.get_connection_info(c(
    "cas-server-1.com/cas-server/base",
    "cas-server-2.com/cas-server/base"
  ), 12345, NULL, NULL, "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "", password = "", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com/cas-server/base",
    "cas-server-2.com/cas-server/base"
  ), 12345, "myuserid", NULL, "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com/cas-server/base",
    "cas-server-2.com/cas-server/base"
  ), 12345, NULL, "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "", password = "mytoken", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "cas-server-1.com/cas-server/base",
    "cas-server-2.com/cas-server/base"
  ), 12345, "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste("http://cas-server-1.com:12345/cas-server/base",
      "http://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "mytoken", protocol = "http"
  ))

  out <- swat:::.get_connection_info(c(
    "https://cas-server-1.com/cas-server/base",
    "https://cas-server-2.com/cas-server/base"
  ), 12345, NULL, NULL, "http", NULL)
  expect_equal(out, list(
    hostname = paste("https://cas-server-1.com:12345/cas-server/base",
      "https://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "", password = "", protocol = "https"
  ))

  out <- swat:::.get_connection_info(c(
    "https://cas-server-1.com/cas-server/base",
    "https://cas-server-2.com/cas-server/base"
  ), 12345, "myuserid", NULL, "http", NULL)
  expect_equal(out, list(
    hostname = paste("https://cas-server-1.com:12345/cas-server/base",
      "https://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "", protocol = "https"
  ))

  out <- swat:::.get_connection_info(c(
    "https://cas-server-1.com/cas-server/base",
    "https://cas-server-2.com/cas-server/base"
  ), 12345, NULL, "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste("https://cas-server-1.com:12345/cas-server/base",
      "https://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "", password = "mytoken", protocol = "https"
  ))

  out <- swat:::.get_connection_info(c(
    "https://cas-server-1.com/cas-server/base",
    "https://cas-server-2.com/cas-server/base"
  ), 12345, "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste("https://cas-server-1.com:12345/cas-server/base",
      "https://cas-server-2.com:12345/cas-server/base",
      sep = " "
    ),
    port = 12345, username = "myuserid", password = "mytoken", protocol = "https"
  ))
})

test_that("test.hostname_expansion", {
  out <- swat:::.get_connection_info("cas-server-[1,2,3].com:5570", 12345,
                                     "myuserid", "mytoken", "cas", NULL)
  expect_equal(out, list(
    hostname = paste(
      "cas-server-1.com",
      "cas-server-2.com",
      "cas-server-3.com"
    ),
    port = 5570, username = "myuserid", password = "mytoken", protocol = "cas"
  ))

  out <- swat:::.get_connection_info("cas-server-[1].com:5570", 12345,
                                     "myuserid", "mytoken", "cas", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("[cas-server-1,cas-server-2].com:5570]", NULL,
                                     "myuserid", "mytoken", NULL, NULL)
  expect_equal(out, list(
    hostname = paste(
      "cas-server-1.com",
      "cas-server-2.com"
    ),
    port = 5570, username = "myuserid", password = "mytoken", protocol = "cas"
  ))

  out <- swat:::.get_connection_info("cas-server-[1,2,3].com:5570", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste(
      "http://cas-server-1.com:5570",
      "http://cas-server-2.com:5570",
      "http://cas-server-3.com:5570"
    ),
    port = 5570, username = "myuserid", password = "mytoken", protocol = "http"
  ))

  out <- swat:::.get_connection_info("cas-server-[1].com:5570", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = "http://cas-server-1.com:5570",
    port = 5570, username = "myuserid", password = "mytoken", protocol = "http"
  ))

  out <- swat:::.get_connection_info("[cas-server-1,cas-server-2].com:5570]", NULL,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(
    hostname = paste(
      "http://cas-server-1.com:5570",
      "http://cas-server-2.com:5570"
    ),
    port = 5570, username = "myuserid", password = "mytoken", protocol = "http"
  ))
})

test_that("test.cas_url", {
  out <- swat:::.get_connection_info("cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", NULL, NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas://cas-server-1.com", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas://cas-server-1.com:5570", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 5570,
                         username = "myuserid", password = "mytoken", protocol = "cas"))

  out <- swat:::.get_connection_info("cas://cas-server-1.com/cas-server/base", 12345,
                                     "myuserid", "mytoken", "http", NULL)
  expect_equal(out, list(hostname = "cas-server-1.com", port = 12345,
                         username = "myuserid", password = "mytoken", protocol = "cas"))
})
