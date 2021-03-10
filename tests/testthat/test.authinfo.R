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


context("test.authinfo.R")

test_that("authinfo", {
  authinfo <- c(test.data("missing.authinfo"), test.data("test1.authinfo"))
  authinfo <- c("/u/kesmit/github/R-swat/tests/data/missing.authinfo",
                "/u/kesmit/github/R-swat/tests/data/test1.authinfo")

  # Basic match
  out <- swat:::.query_authinfo("cas-server-1.org", filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-1.org")
  expect_equivalent(out$username, "user-1")
  expect_equivalent(out$password, "pwd-user-1")
  expect_equivalent(out$port, NULL)

  # Machine and protocol match
  out <- swat:::.query_authinfo("cas-server-2.org", protocol = 5500, filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-2.org")
  expect_equivalent(out$username, "user-2")
  expect_equivalent(out$password, "pwd-user-2")
  expect_equivalent(out$port, 5500)

  out <- swat:::.query_authinfo("cas-server-2.org", protocol = 5501, filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-2.org")
  expect_equivalent(out$username, "user-2.1")
  expect_equivalent(out$password, "pwd-user-2.1")
  expect_equivalent(out$port, 5501)

  # Alternate names machine => host / protocol => port / login => user
  out <- swat:::.query_authinfo("cas-server-2.org", protocol = 5502, filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-2.org")
  expect_equivalent(out$username, "user-2.2")
  expect_equivalent(out$password, "pwd-user-2.2")
  expect_equivalent(out$port, 5502)

  # Named protocols
  out <- swat:::.query_authinfo("cas-server-3.org", protocol = "http", filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-3.org")
  expect_equivalent(out$username, "user-3")
  expect_equivalent(out$password, "pwd-user-3")
  expect_equivalent(out$port, 80)

  out <- swat:::.query_authinfo("cas-server-3.org", protocol = "https", filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-3.org")
  expect_equivalent(out$username, "user-3.1")
  expect_equivalent(out$password, "pwd-user-3.1")
  expect_equivalent(out$port, 443)

  # Bad protocol
  expect_warning(expect_error(
    swat:::.query_authinfo("cas-server-3.org", protocol = "unknown", filepath = authinfo),
    "Could not convert protocol"))

  # Default user
  out <- swat:::.query_authinfo("unknown.org", filepath = authinfo)
  expect_equivalent(out$hostname, "*")
  expect_equivalent(out$username, "default-user")
  expect_equivalent(out$password, "default-user-pwd")
  expect_equivalent(out$port, NULL)

  # Bad keyword (prot instead of port)
  out <- swat:::.query_authinfo("cas-server-4.org", filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-4.org")
  expect_equivalent(out$username, "user-4")
  expect_equivalent(out$password, "pwd-user-4")
  expect_equivalent(out$port, NULL)

  out <- swat:::.query_authinfo("cas-server-4.org", protocol = 11111, filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-4.org")
  expect_equivalent(out$username, "user-4")
  expect_equivalent(out$password, "pwd-user-4")
  expect_equivalent(out$port, NULL)

  out <- swat:::.query_authinfo("cas-server-4.org", protocol = 22222, filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-4.org")
  expect_equivalent(out$username, "user-4")
  expect_equivalent(out$password, "pwd-user-4")
  expect_equivalent(out$port, NULL)

  # Quoted strings
  out <- swat:::.query_authinfo("cas-server-5.org", filepath = authinfo)
  expect_equivalent(out$hostname, "cas-server-5.org")
  expect_equivalent(out$username, "user-5")
  expect_equivalent(out$password, "pwd user 5")
  expect_equivalent(out$port, NULL)

  # Environment variable
  ai_env <- Sys.getenv("AUTHINFO")
  net_env <- Sys.getenv("NETRC")
  Sys.setenv(AUTHINFO = paste(authinfo, collapse = .Platform$path.sep))
  Sys.setenv(NETRC = paste(authinfo, collapse = .Platform$path.sep))

  out <- swat:::.query_authinfo("cas-server-5.org")
  expect_equivalent(out$hostname, "cas-server-5.org")
  expect_equivalent(out$username, "user-5")
  expect_equivalent(out$password, "pwd user 5")
  expect_equivalent(out$port, NULL)

  Sys.unsetenv("AUTHINFO")

  out <- swat:::.query_authinfo("cas-server-5.org")
  expect_equivalent(out$hostname, "cas-server-5.org")
  expect_equivalent(out$username, "user-5")
  expect_equivalent(out$password, "pwd user 5")
  expect_equivalent(out$port, NULL)

  if (length(ai_env) == 0) {
    Sys.unsetenv("AUTHINFO")
  } else {
    Sys.setenv(AUTHINFO = ai_env)
  }

  if (length(net_env) == 0) {
    Sys.unsetenv("NETRC")
  } else {
    Sys.setenv(NETRC = net_env)
  }

  # No default
  authinfo <- c(test.data("missing.authinfo"), test.data("test1.authinfo"))
  authinfo <- c("/u/kesmit/github/R-swat/tests/data/missing.authinfo",
                "/u/kesmit/github/R-swat/tests/data/test2.authinfo")

  # No default
  out <- swat:::.query_authinfo("unknown.org", filepath = authinfo)
  expect_equivalent(out$hostname, NULL)
  expect_equivalent(out$username, NULL)
  expect_equivalent(out$password, NULL)
  expect_equivalent(out$port, NULL)
})
