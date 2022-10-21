library(testit)

source("surveillance_data_manager.R")

test.header <- function (func_name) {
    cat(sprintf("-- Running test suite for %s --\n", func_name))
}

test.header("get.years.for.year.value")

#Test 1
x = get.years.for.year.value(NA,"2001a")
assert ("1. 2001a should return NA", is.na(x))

#Test 2
x = get.years.for.year.value(NA, "2018")
assert ("2. String 2018 should convert easily", x == c(2018))

#Test 3
x = get.years.for.year.value(NA, "2018-2020")
assert ("3. Valid date range 2018-2020", x == c(2018,2019,2020))

#Test 4
x = get.years.for.year.value(NA, "2020-2018")
assert ("4. Valid inverted date range 2020-2018", x == c(2020, 2019, 2018))

#Test 5
x = get.years.for.year.value(NA, "2020-2020")
assert ("5. Valid date range 2020-2020", x == c(2020))

#Test 6
x = get.years.for.year.value(NA, "2018-2020a")
assert ("6. Invalid date range 2018-2020a", is.na(x))

#Test 7
x = get.years.for.year.value(NA, "2018a-2020")
assert ("7. Invalid date range 2018a-2020", is.na(x))

#Test 8
x = get.years.for.year.value(NA, c("2018","2019","2020"))
assert ("8. Valid vector of character years", x == c(2018,2019,2020))

#Test 9
x = get.years.for.year.value(NA, c("2017","2019","2020"))
assert ("9. Invalid vector of character years", is.na(x))

#Test 10
x = get.years.for.year.value(NA, c("2018","2020","2021"))
assert ("10. Invalid vector of character years", is.na(x))

#Test 11
x = get.years.for.year.value(NA, 2019)
assert ("11. Valid single double parameter", x == c(2019))

#Test 12
x = get.years.for.year.value(NA, c(2019))
assert ("12. Valid single double vector parameter", x == c(2019))

#Test 13
x = get.years.for.year.value(NA, c(2019,2020,2021,2022))
assert ("13. Valid multi double vector parameter", x == c(2019,2020,2021,2022))

#Test 14
x = get.years.for.year.value(NA, c(2019,2020,2021,2023))
assert ("14. Invalid multi double vector parameter", is.na(x))

#Test 15
x = get.years.for.year.value(NA, 2019L)
assert ("15. Valid single integer parameter", x == c(2019))

#Test 16
x = get.years.for.year.value(NA, c(2019L))
assert ("16. Valid single integer vector parameter", x == c(2019))

#Test 17
x = get.years.for.year.value(NA, c(2019L,2020L,2021L,2022L))
assert ("17. Valid multi integer vector parameter", x == c(2019,2020,2021,2022))

#Test 18
x = get.years.for.year.value(NA, c(2019L,2020L,2021L,2023L))
assert ("18. Invalid multi integer vector parameter", is.na(x))


cat(sprintf("-- All Tests Passed --\n"))
