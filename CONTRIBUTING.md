# SAS SWAT Developer How-To

Developing SWAT using the REST interface is just like developing any 
other project on GitHub.  You clone the project, do your work, 
and submit a pull request.  However, the binary interface is a bit
different since it requires the bundled SAS TK libraries an R
C extension modules.

## Developing Against the Binary CAS Interface

In order to run against CAS using the binary interface, you must copy
the C libraries from a platform-specific distribution to your git
clone.  These files are located in the swat/src/ directory.
So to develop on Linux, you would clone SWAT from GitHub, download the
Linux-specific tar.gz file, unzip it, and copy the swat/src/\*.so
files to your clone directory.  From that point on, you should be able
to connect to both REST and binary CAS ports from your clone.

## Submitting a Pull Request

Submitting a pull request uses the standard process at GitHub.
Note that in the submitted changes, there must always be a unit test
for the code being contributed.  Pull requests that do not have a
unit test will not be accepted.

You also must include the text from the ContributerAgreement.txt file
along with your sign-off verifying that the change originated from you.

## Testing

For the most part, testing the SAS SWAT package is just like testing
any other R package.  Tests are written using the testthat package.  

Since CAS is a network resource and requires authentication, there is
some extra setup involved in getting your tests configured to run 
against your CAS server.  Normally this involves setting the following
environment variables.

* CASHOST - the hostname or IP address of your CAS server (Default: None)
* CASPORT - the port of your CAS server (Default: None)
* CASPROTOCOL - the protocol being using ('cas', 'http', 'https'; Default: 'cas')

* CASUSER - the CAS account username (Default: None)
* CASPASSWORD - the CAS account password (Default: None)

You can set these environment variables within your R sessions as follows:
    
    Sys.setenv(CASHOST='myhost.com')
    Sys.setenv(CASPORT=5570)
    Sys.setenv(CASPROTOCOL='cas')

Some of these can alternatively be specified using configuration files.
The CASHOST, CASPORT, and CASPROTOCOL variables can be specified in a .casrc
in your home directory (or in any directory from the directory you are 
running from all the way up to your home directory).  It is actually written
in Lua, but the most basic form is as follows:

    cashost = 'myhost.com'
    casport = 5570
    casprotocol = 'cas'

The CASUSER and CASPASSWORD variables are usually extracted from your
`~/.authinfo` file automatically.  The only reason you should use environment
variables is if you have a generalized test running account that is
shared across various tools.

Once you have these setup, you can use tools like devtools to run the suite:

    devtools::test()

You can also run each test individually using profiling as follows:

    testthat::test_file('path/to/test-file.R')
