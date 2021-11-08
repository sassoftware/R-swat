
# Change Log

## 1.6.4 - 2021-11-08

- Added `to.r.data.frame` function to download a `CAStbl` as `data.frame` without defining a `casDataFrame` 

## 1.6.3 - 2021-08-11

- Fix duplicate action parameter issue
- Fix integer issue in table to data.frame converter

## 1.6.2 - 2021-06-18

- Add authorization code style of authentication

## 1.6.1 - 2021-05-10

- Improve CAS result table to `data.frame` performance
- Fix blob support in REST interface
- Add support for RStudio connections
- Improve libssl / libcrypto detection

## 1.6.0 - 2021-01-13

- Fix binary parameters and downloads
- Remove extra dependencies (dplyr, random)
- Improve connection parameter parsing and add `CAS_URL` environment variable support
- Improve performance of initial server connection

## 1.5.0 - 2020-06-08

- Add support for R v4.0
- Improve unit testing framework
- Add blob result value support

## 1.4.1 - 2019-12-02

- Improve connection URL parsing and default ports
- Fix issue with generated wrapper parameters
- Add caslibs to rbind function

## 1.4.0 - 2018-09-18

- Add support for binary protocol on Windows
- Fix problem when explicitly specifying authinfo file

## 1.3.0 - 2018-07-25

- Add support for By group sets in rbind.bygroups
- Improve error reporting if no connection object is available for action call
- Display error if specified authinfo file is missing

## 1.2.1 - 2018-06-05

- Added options for handling By groups
- Added rbind.bygroups for concatenating data.frames in CAS results containing By groups
- Added cas.terminate and cas.close functions for closing connections
- Added cas.upload, cas.upload.file, and cas.upload.frame functions for uploading data
- Added performance, severity, statusCode, reason, status, messages, and events fields to CAS object
- Improved performance of summary function
- Improve error reporting

## 1.2.0 - 2018-01-24

- Improve controller failover support
- Add encryption support to REST interface
- Improvements to data selection, orderby and groupby in CAS table objects
- Fix issues related to new columns in new correlation action

## 1.1.0 - 2017-10-13

- Add common help page for all CAS actions
- Add support for controller failover and integer missing values
- Fixes for cor and cov
- Improved handling of computed variables
- Added additional tests
- Handling of int64s to vectors instead of lists

## 1.0.0 - 2017-05-01

- CAS object for connecting to SAS Cloud Analytic Services (CAS)
- Automatically generated functions for calling CAS actions.
- CAS table object for interacting and invoking actions on CAS in-memory tables.
- Support for creating CAS tables using R-style data readers, as well as data.frame-like access to CAS tables.
