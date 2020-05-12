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


context('test.datetime.R')

test_that('test.cas.datetime', {
    expect_equal(class(cas2rPOSIXct('315662400000000')), c('POSIXct', 'POSIXt'))
    expect_equal(cas2rPOSIXct('315662400000000'),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(cas2rPOSIXct(315662400000000),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(class(cas2rPOSIXlt('315662400000000')), c('POSIXlt', 'POSIXt'))
    expect_equal(cas2rPOSIXlt('315662400000000'),
                 as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(cas2rPOSIXlt(315662400000000),
                 as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(cas2sasDateTime('315662400000000'), 315662400)
    expect_equal(cas2sasDateTime(315662400000000), 315662400)

    expect_equal(class(cas2rDate(3653)), 'Date')
    expect_equal(cas2rDate(3653),
                 as.Date(as.POSIXlt(origin='1970-01-01', tz='UTC', 0)))
    expect_equal(cas2sasDate(3653), 3653)

    expect_equal(class(casTime2rPOSIXct('43200000000')), c('POSIXct', 'POSIXt'))
    expect_equal(casTime2rPOSIXct('43200000000'),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(casTime2rPOSIXct(43200000000),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(class(casTime2rPOSIXlt('43200000000')), c('POSIXlt', 'POSIXt'))
    expect_equal(casTime2rPOSIXlt('43200000000'),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(casTime2rPOSIXlt(43200000000),
                 as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(cas2sasTime('43200000000'), 43200)
    expect_equal(cas2sasTime(43200000000), 43200)
})

test_that('test.r2cas', {
    expect_equal(rPOSIXlt2cas(as.POSIXlt(strptime("1970-01-01 12:00:00",
                                                  "%Y-%m-%d %H:%M:%S", tz='UTC'))),
                 315662400000000)
    expect_equal(rPOSIXct2cas(as.POSIXct(strptime("1970-01-01 12:00:00",
                                                  "%Y-%m-%d %H:%M:%S", tz='UTC'))),
                 315662400000000)

    expect_equal(rDate2cas(as.Date(as.POSIXlt(origin='1970-01-01', tz='UTC', 0))), 3653)
})

test_that('test.sas.datetime', {
    expect_equal(class(sas2rPOSIXct('315662400')), c('POSIXct', 'POSIXt'))
    expect_equal(sas2rPOSIXct('315662400'),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(sas2rPOSIXct(315662400),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(class(sas2rPOSIXlt('315662400')), c('POSIXlt', 'POSIXt'))
    expect_equal(sas2rPOSIXlt('315662400'),
                 as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(sas2rPOSIXlt(315662400),
                 as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(sas2casDateTime('315662400'), 315662400000000)
    expect_equal(sas2casDateTime(315662400), 315662400000000)

    expect_equal(class(sas2rDate(3653)), 'Date')
    expect_equal(sas2rDate(3653),
                 as.Date(as.POSIXlt(origin='1970-01-01', tz='UTC', 0)))
    expect_equal(sas2casDate(3653), 3653)

    expect_equal(class(sasTime2rPOSIXct('43200')), c('POSIXct', 'POSIXt'))
    expect_equal(sasTime2rPOSIXct('43200'),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(sasTime2rPOSIXct(43200),
                 as.POSIXct(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(class(sasTime2rPOSIXlt('43200')), c('POSIXlt', 'POSIXt'))
    expect_equal(sasTime2rPOSIXlt('43200'),
                 as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))
    expect_equal(sasTime2rPOSIXlt(43200),
                 as.POSIXlt(strptime("1970-01-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz='UTC')))

    expect_equal(sas2casTime('43200'), 43200000000)
    expect_equal(sas2casTime(43200), 43200000000)
})

test_that('test.r2sas', {
    expect_equal(rPOSIXlt2sas(as.POSIXlt(strptime("1970-01-01 12:00:00",
                                                  "%Y-%m-%d %H:%M:%S", tz='UTC'))),
                 315662400)
    expect_equal(rPOSIXct2sas(as.POSIXct(strptime("1970-01-01 12:00:00",
                                                  "%Y-%m-%d %H:%M:%S", tz='UTC'))),
                 315662400)

    expect_equal(rDate2sas(as.Date(as.POSIXlt(origin='1970-01-01', tz='UTC', 0))), 3653)
})
