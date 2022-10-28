library(crayon)

source("surveillance_data_manager.R")

#Globals until I figure out a better way (environments?)
gyfyv = list( passed.count = 0, total.count = 0 )
gabfav = list( passed.count = 0, total.count = 0 )

test.header <- function (func_name) {
    cat(cyan(sprintf("-- Running test suite for %s --\n", func_name)))
}

test.footer <- function (func_name, var.data) {
    str = sprintf("Tests Passed: %g/%d --\n", var.data$passed.count, var.data$total.count)
    if (var.data$total.count == var.data$passed.count) {
        cat(cyan(sprintf("-- %s : All %s",func_name, str)))
    } else if (var.data$passed.count > 0) {
        cat(orange(sprintf("-- %s : Some %s",func_name, str)))
    } else {
        cat(red(sprintf("-- %s : No %s", func_name, str)))
    }

}

gyfyv.test <- function (input, ex, if_fail) {
    #get.years.for.year.value function (gyfyv)
    value = get.years.for.year.value(NA, input)
    if (all(eval(ex, list(x = value)))) {
        cat(green(sprintf("Test %g Passed: %s (%s)\n", gyfyv$total.count + 1, if_fail, 
                          toString(input))))
        gyfyv$passed.count <<- gyfyv$passed.count + 1
    } else {
        cat(red(sprintf("Test %g Failed: %s\n", gyfyv$total.count + 1, if_fail)))
        cat(red(sprintf("\t%g != %s\n", value, ex)))
    }
    gyfyv$total.count <<- gyfyv$total.count + 1
}

gabfav.test <- function (input, ex, if_fail) {
    #get.age.bounds.for.age.value (gabfav)
    value = get.age.bounds.for.age.value(NA,input)
    if (all(eval(ex, list(x = value)))) {
        cat(green(sprintf("Test %g Passed: %s (%s)\n", gabfav$total.count + 1, if_fail, 
                          toString(input))))
        gabfav$passed.count <<- gabfav$passed.count + 1
    } else {
        cat(red(sprintf("Test %g Failed: %s\n", gabfav$total.count + 1, if_fail)))
        cat(red(sprintf("\t%g != %s\n", value, ex)))
    }
    gabfav$total.count <<- gabfav$total.count + 1
}

test.header("get.years.for.year.value")

gyfyv.test("2001a", expression(is.na(x)), "Invalid single year")
gyfyv.test("2018", expression(!is.na(x) && (x == c(2018))), "Valid single year")
gyfyv.test("2018-2020", expression(!is.na(x) && (x == c(2018,2019,2020))), "Valid date range")
gyfyv.test("2020-2018", expression(!is.na(x) && (x == c(2020, 2019, 2018))),
    "Valid inverted date range")
gyfyv.test("2020-2020", expression(!is.na(x) && (x == c(2020))),"Valid date range")
gyfyv.test("2019-2020-2021-2022", expression (is.na(x)), "Invalid range (only one dash allowed)")
gyfyv.test("2018-2020a", expression(is.na(x)),"Invalid date range (invalid character)")
gyfyv.test("2018a-2020", expression(is.na(x)),"Invalid date range (invalid character)")
gyfyv.test(c("2018","2019","2020"), expression(!is.na(x) && (x == c(2018,2019,2020))),
    "Valid vector of character years")
gyfyv.test(c("2017","2019","2020"), expression(is.na(x)),
    "Invalid vector of character years (not sequential)")
gyfyv.test(c("2018","2020","2021"), expression(is.na(x)),
    "Invalid vector of character years (not sequential)")
gyfyv.test(c("2018a","2020","2021"), expression(is.na(x)),
    "Invalid vector of character years (invalid character)")
gyfyv.test(2019, expression(!is.na(x) && (x == c(2019))), "Valid single double parameter")
gyfyv.test(c(2019), expression(!is.na(x) && (x == c(2019))), 
    "Valid single double vector parameter")
gyfyv.test(c(2019,2020,2021,2022), expression(!is.na(x) && (x == c(2019,2020,2021,2022))),
    "Valid multi double vector parameter")
gyfyv.test(c(2019,2020,2021,2023), expression(is.na(x)),
    "Invalid multi double vector parameter (non sequential)")
gyfyv.test(2019L, expression(!is.na(x) && (x == c(2019))), "Valid single integer parameter")
gyfyv.test(c(2019L), expression(!is.na(x) && (x == c(2019))), 
     "Valid single integer vector parameter")
gyfyv.test(c(2019L,2020L,2021L,2022L), expression(!is.na(x) && (x == c(2019,2020,2021,2022))),
     "Valid multi integer vector parameter")
gyfyv.test(c(2019L,2020L,2021L,2023L), expression(is.na(x)),
     "Invalid multi integer vector parameter")

test.footer("get.years.for.year.value", gyfyv)

test.header("get.age.bounds.for.age.value")

gabfav.test(34, expression (!is.na(x) && (x == c(34))), "Valid single age, double")
gabfav.test(34L, expression (!is.na(x) && (x == c(34))), "Valid single age, integer")
gabfav.test("34", expression (!is.na(x) && (x == c(34))), "Valid single age, character")
gabfav.test("34a", expression(is.na(x)), "Invalid single age, character")
gabfav.test("34-36", expression(!is.na(x) && (x == c(34,35,36))), "Valid age range, character")
gabfav.test("34-36a", expression(is.na(x)), "Invalid age range, character")
gabfav.test("34-36-38", expression(is.na(x)), "Invalid age range, only one dash allowed")
gabfav.test(c("34","35","36"), expression(!is.na(x) && (x == c(34,35,36))), 
     "Valid age range, character vector")
gabfav.test(c("34","35","37"), expression(is.na(x)),
     "Invalid age range for character vector, non sequential")
gabfav.test(c("34","35b","36"), expression(is.na(x)),
     "Invalid age range for character vector, invalid character")
gabfav.test(c(34,35,36), expression(!is.na(x) && (x == c(34,35,36))),
     "Valid age range for double vector")
gabfav.test(c(36,35,34), expression(!is.na(x) && (x == c(36,35,34))),
     "Valid age range for double vector")
gabfav.test(c(33,35,36), expression(is.na(x)), "Invalid age range for double vector, non sequential")
gabfav.test(c(33L,35L,36L), expression(is.na(x)), 
     "Invalid age range for integer vector, non sequential")
gabfav.test(c(34L,35L,36L), expression(!is.na(x) && (x == c(34,35,36))),
     "Valid age range for integer vector")

test.footer("get.age.bounds.for.age.value", gabfav)
