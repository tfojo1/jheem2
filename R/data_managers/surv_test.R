library(crayon)

source("../tests/test_framework.R")

move.wd.to.root()

source(test.source ("data_managers", "surveillance_data_manager.R"))

# get.years.for.year.value function tests
gyfyv <- create.test.function.suite ( "get.years.for.year.value",
                                      function (input) get.years.for.year.value(NA,input) )

gyfyv$header()

gyfyv$test("2001a", expression(is.na(x)), "Invalid single year")
gyfyv$test("2018", expression(x == c(2018)), "Valid single year")
gyfyv$test("2018-2020", expression(x == c(2018,2019,2020)), "Valid date range")
gyfyv$test("2020-2018", expression(x == c(2020, 2019, 2018)),
    "Valid inverted date range")
gyfyv$test("2020-2020", expression(x == c(2020)),"Valid date range")
gyfyv$test("2019-2020-2021-2022", expression (is.na(x)), "Invalid range (only one dash allowed)")
gyfyv$test("2018-2020a", expression(is.na(x)),"Invalid date range (invalid character)")
gyfyv$test("2018a-2020", expression(is.na(x)),"Invalid date range (invalid character)")
gyfyv$test(c("2018","2019","2020"), expression(x == c(2018,2019,2020)),
    "Valid vector of character years")
gyfyv$test(c("2017","2019","2020"), expression(is.na(x)),
    "Invalid vector of character years (not sequential)")
gyfyv$test(c("2018","2020","2021"), expression(is.na(x)),
    "Invalid vector of character years (not sequential)")
gyfyv$test(c("2018a","2020","2021"), expression(is.na(x)),
    "Invalid vector of character years (invalid character)")
gyfyv$test(2019, expression(x == c(2019)), "Valid single double parameter")
gyfyv$test(c(2019), expression(x == c(2019)), 
    "Valid single double vector parameter")
gyfyv$test(c(2019,2020,2021,2022), expression(x == c(2019,2020,2021,2022)),
    "Valid multi double vector parameter")
gyfyv$test(c(2019,2020,2021,2023), expression(is.na(x)),
    "Invalid multi double vector parameter (non sequential)")
gyfyv$test(2019L, expression(x == c(2019)), "Valid single integer parameter")
gyfyv$test(c(2019L), expression(x == c(2019)), 
     "Valid single integer vector parameter")
gyfyv$test(c(2019L,2020L,2021L,2022L), expression(x == c(2019,2020,2021,2022)),
     "Valid multi integer vector parameter")
gyfyv$test(c(2019L,2020L,2021L,2023L), expression(is.na(x)),
     "Invalid multi integer vector parameter")

gyfyv$footer()

# get.age.bounds.for.age.value function tests
gabfav <- create.test.function.suite ( "get.age.bounds.for.age.value",
                                       function (input) get.age.bounds.for.age.value(NA,input) )

gabfav$header()

gabfav$test(34, expression (x == c(34)), "Valid single age, double")
gabfav$test(34L, expression (x == c(34)), "Valid single age, integer")
gabfav$test("34", expression (x == c(34)), "Valid single age, character")
gabfav$test("34a", expression(is.na(x)), "Invalid single age, character")
gabfav$test("43", expression(x == c(43)), "Valid check")
gabfav$test("34 years", expression(x == c(34)), "Valid singe age, character")
gabfav$test("34years", expression(x == c(34)), "Valid singe age, character")
gabfav$test("34-36", expression(x == c(34,35,36)), "Valid age range, character")
gabfav$test("34-36 years", expression(x == c(34,35,36)), "Valid age range with string")
gabfav$test("34 years-36 years", expression(x == c(34,35,36)), 
            "Valid age range with strings")
gabfav$test("   34    years -  36    years   ", expression(x == c(34,35,36)), 
            "Valid age range with strings")
gabfav$test("75+ years", expression(x = c(75,76,77,78,79,80)), 
            "Valid age range with +")
gabfav$test("25-35+ years", expression(is.na(x)), "Invalid age range (mixing ranges and +)")
gabfav$test("34-36a", expression(is.na(x)), "Invalid age range, character")
gabfav$test("34-36-38", expression(is.na(x)), "Invalid age range, only one dash allowed")
gabfav$test(c("34","35","36"), expression(x == c(34,35,36)), 
     "Valid age range, character vector")
gabfav$test(c("34","35","37"), expression(is.na(x)),
     "Invalid age range for character vector, non sequential")
gabfav$test(c("34","35b","36"), expression(is.na(x)),
     "Invalid age range for character vector, invalid character")
gabfav$test(c(34,35,36), expression(x == c(34,35,36)),
     "Valid age range for double vector")
gabfav$test(c(36,35,34), expression(x == c(36,35,34)),
     "Valid age range for double vector")
gabfav$test(c(33,35,36), expression(is.na(x)), "Invalid age range for double vector, non sequential")
gabfav$test(c(33L,35L,36L), expression(is.na(x)), 
     "Invalid age range for integer vector, non sequential")
gabfav$test(c(34L,35L,36L), expression(x == c(34,35,36)),
     "Valid age range for integer vector")

gabfav$footer()

# Function Testing

cat(white("\nFunction Testing\n\n"))

get.surveillance.data(msa.surveillance,
                      c("new","prevalence","aids.diagnoses", "ooogy"), "2000-2014")
