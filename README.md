# SAS Scripting Wrapper for Analytics Transfer (SWAT) for R

SWAT is an R package that enables you to interface with
with SAS Cloud Analytics Services (CAS), the in-memory server that is 
the centerpiece of the [SAS Viya](http://www.sas.com/en_us/software/viya.html)
platform. 
Using the SWAT package, you can write an R program that connects to a CAS server, 
analyze large in-memory data sets, and then work with the results of the data 
analysis using familiar data-wrangling technigues in R.

This package provides facilities for starting a CAS session and 
running actions in CAS--such as classifying data with a decision tree or modeling
with linear regression. Data processing is performed by CAS, which can scale from
a single-machine server to distributed servers that run on multiple hosts and
perform massively parallel processing.

## Installation and Configuration

### Requirements

To use the SWAT package for R, the client machine that runs R must meet 
the following requirements: 

* Use 64-bit Linux or 64-bit Windows. 
* Use a 64-bit version of R. 
* Use R 3.1.0 or later.
* Install the ``httr`` and ``jsonlite`` packages. These packages have additional dependencies
that are automatically installed from CRAN when you run install.packages(). 

### Installation

The SWAT package for R is available from SAS as a tar.gz file. You can download 
releases from https://github.com/sassoftware/r-swat/releases.

After you download the package, you can install the package with a command that is 
similar to the following: 

```
R CMD INSTALL r-swat-X.X.X-platform.tar.gz
```

You can also install from within R directly using a URL.

```
# Make sure prerequisites are installed
> install.packages('httr')
> install.packages('jsonlite')

> install.packages('https://github.com/sassoftware/r-swat/releases/download/vX.X.X/r-swat-X.X.X-platform.tar.gz',
                   repos=NULL, type='file')
```

If you are running on a platform that does not have an associated installer, 
you should install the source code tar.gz.  These platforms will be
limited to using the CAS REST interface only.

```
> install.packages('https://github.com/sassoftware/r-swat/archive/vX.X.X.tar.gz', 
                   repos=NULL, type='file')
```

## Connecting to CAS

### Authinfo File
Using a .authinfo file is not required to use the package, but is recommended. When you 
enter your credentials (user ID and password) in a .authinfo file and secure the 
permissions, you can avoid specifying those credentials in programs. 

Throughout the documentation, it is assumed that you have a .authinfo file. The following 
statement connects R to CAS and supplies the credentials from the .authinfo file: 

```
conn <- swat::CAS('cloud.example.com', 8777, protocol='http')
```

If you do not use a .authinfo file, then you must connect with a statement like the following: 

```
conn <- swat::CAS('cloud.example.com', 8777, protocol='http', username='sasdemo', password='!s3cret')
```

### Binary and REST Communication

Communication between R and CAS can be performed in a binary format with proprietary C 
libraries, or over HTTP to a REST interface on the server. The C libraries (and therefore 
binary communication) are supported for 64-bit Linux only. Connections to CAS that use 
binary communication are similar to the following example: 

```
conn <- swat::CAS('cloud.example.com', 5570)
```

## Example

```
> library(swat)

# Connect to CAS
> conn <- swat::CAS('cloud.example.com', 8777)

# Load Iris data set into in-memory table
> iris.ct <- as.casTable(conn, iris)

# Use basic R functions on CAS table
> min(iris.ct$Sepal.Length)
[1] 4.3

> max(iris.ct$Sepal.Length)
[1] 7.9

> mean(iris.ct$Sepal.Length)
[1] 5.843333

# Call CAS actions on the table
> out <- cas.simple.summary(iris.ct)
> out
$Summary
        Column Min Max   N NMiss     Mean   Sum       Std     StdErr       Var
1 Sepal.Length 4.3 7.9 150     0 5.843333 876.5 0.8280661 0.06761132 0.6856935
2  Sepal.Width 2.0 4.4 150     0 3.057333 458.6 0.4358663 0.03558833 0.1899794
3 Petal.Length 1.0 6.9 150     0 3.758000 563.7 1.7652982 0.14413600 3.1162779
4  Petal.Width 0.1 2.5 150     0 1.199333 179.9 0.7622377 0.06223645 0.5810063
      USS       CSS       CV   TValue         ProbT
1 5223.85 102.16833 14.17113 86.42537 3.331256e-129
2 1430.40  28.30693 14.25642 85.90830 8.004458e-129
3 2582.71 464.32540 46.97441 26.07260  2.166017e-57
4  302.33  86.56993 63.55511 19.27060  2.659021e-42

# Explore results
> out$Summary[c('Column', 'Min', 'Max')]
```
