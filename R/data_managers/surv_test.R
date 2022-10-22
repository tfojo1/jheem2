library(testit)
library(crayon)

source("surveillance_data_manager.R")

test.header <- function (func_name) {
    cat(cyan(sprintf("-- Running test suite for %s --\n", func_name)))
}

test.footer <- function () {
    if (total.count == passed.count) {
        cat(cyan(sprintf("-- All Tests Passed: %g/%d --\n", passed.count, total.count)))
    } else {
        cat(red(sprintf("-- Some Tests Passed: %g/%d --\n", passed.count, total.count)))
    }
}

gyfyv.test <- function (input, ex, if_fail) {
    #get.years.for.year.value function (gyfyv)
    value = get.years.for.year.value(NA, input)
    if (all(eval(ex, list(x = value)))) {
        cat(green(sprintf("Test %g Passed: %s (%s)\n", total.count + 1, if_fail, toString(input))))
        passed.count <<- passed.count + 1
    } else {
        cat(red(sprintf("Test %g Failed: %s\n", total.count + 1, if_fail)))
    }
    total.count <<- total.count + 1
}

#Globals until I figure out a better way (environments?)
passed.count = 0
total.count = 0

test.header("get.years.for.year.value")

gyfyv.test("2001a", expression(is.na(x)), "Invalid single year")
gyfyv.test("2018", expression(x == c(2018)), "Valid single year")
gyfyv.test("2018-2020", expression(x == c(2018,2019,2020)), "Valid date range")
gyfyv.test("2020-2018", expression(x == c(2020, 2019, 2018)),"Valid inverted date range")
gyfyv.test("2020-2020", expression(x == c(2020)),"Valid date range")
gyfyv.test("2018-2020a", expression(is.na(x)),"Invalid date range (invalid character)")
gyfyv.test("2018a-2020", expression(is.na(x)),"Invalid date range (invalid character)")
gyfyv.test(c("2018","2019","2020"), expression(x == c(2018,2019,2020)),
    "Valid vector of character years")
gyfyv.test(c("2017","2019","2020"), expression(is.na(x)),
    "Invalid vector of character years (not sequential)")
gyfyv.test(c("2018","2020","2021"), expression(is.na(x)),
    "Invalid vector of character years (not sequential)")
gyfyv.test(c("2018a","2020","2021"), expression(is.na(x)),
    "Invalid vector of character years (invalid character)")
gyfyv.test(2019, expression(x == c(2019)), "Valid single double parameter")
gyfyv.test(c(2019), expression(x == c(2019)), "Valid single double vector parameter")
gyfyv.test(c(2019,2020,2021,2022), expression(x == c(2019,2020,2021,2022)),
    "Valid multi double vector parameter")
gyfyv.test(c(2019,2020,2021,2023), expression(is.na(x)),
    "Invalid multi double vector parameter (non sequential)")
gyfyv.test(2019L, expression(x == c(2019)), "Valid single integer parameter")
gyfyv.test(c(2019L), expression(x == c(2019)), "Valid single integer vector parameter")
gyfyv.test(c(2019L,2020L,2021L,2022L), expression(x == c(2019,2020,2021,2022)),
     "Valid multi integer vector parameter")
gyfyv.test(c(2019L,2020L,2021L,2023L), expression(is.na(x)),
     "Invalid multi integer vector parameter")

test.footer()
