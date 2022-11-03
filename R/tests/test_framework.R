
#' test.source
#' 
#' Function to convert a module and src filename into the proper format
#' for testing individual files as well as testing all at once.
#' If we are running the individual file inside the directory, we don't need the module
#' name but if we are running from run_all_tests.R, we do.  So this function will insert
#' that data depending on where it's run.
#'
#' @param module This is the directory of the main directory that is holding the source file
#' @param filename This is the name of the source file inside the above directory
#'
#' @return A string representing the correct path to the source file
#'
test.source <- function (module, filename) {
    # get working directory
    cwd = getwd()
    cwd.split = strsplit(cwd, "/")#[[1]]
    # Check for the name of the module; if it is there, then we don't
    # need to add it.  In the case of the run_all_tests.R method, it needs 
    # to be inserted
    if (!is.na(match(module,cwd.split))) {
        module = ""
    } 

    sprintf("%s/%s/%s",cwd,module,filename)
}

#' move.wd.to.root
#'
#' Function to move the working directory from somewhere in the source tree
#' to the R/ directory.  If we are unable to find the R directory, the program stops.
#'
#' @return The current working directory (which must be R/)
#'
move.wd.to.root <- function() {
    current.dir = getwd()

    split = strsplit(current.dir,"/")[[1]]

    #Find the location of the R directory in our current tree
    r.location = match("R", split)

    if (is.na(r.location)) {
        stop("Unable to locate the root R directory, aborting")
    }

    # We located the R directory
    up.dir.count = length(split) - r.location

    if (up.dir.count > 0) {
        # We're not already there, move up the tree to the R directory
        setwd(paste(rep("../",up.dir.count),collapse=""))
    }
    getwd()
}

#' create.test.function.suite
#' 
#' Function to create the suite of functions to test a particular input
#' to a function.
#'
#' @param name A string representation of the function, to be displayed in the header and footer
#' @param f A function that takes a single input and is run as the test.
#'
#' @return A list containing several variables and functions to aid in the construction of test
#'         suites.  Please see data_manager/surv_test.R for implementation
#'
create.test.function.suite <- function ( name, f ) {
    rv <- list()

    rv$name = name
    rv$passed.count = 0
    rv$total.count = 0
    rv$f = f
    # F is a function that takes a single argument as input
    rv$test <- function (input, ex, if_fail) {
        #get.years.for.year.value function (gyfyv)
        value = rv$f(input)
        if (all(eval(ex, list(x = value)))) {
            cat(green(sprintf("Test %g Passed: %s (%s)\n", rv$total.count + 1, if_fail, 
                              toString(input))))
            rv$passed.count <<- rv$passed.count + 1
        } else {
            cat(red(sprintf("Test %g Failed: %s\n", rv$total.count + 1, if_fail)))
            cat(red(sprintf("\t%g != %s\n", value, ex)))
        }
        rv$total.count <<- rv$total.count + 1
    }

    rv$header <- function () {
        cat(cyan(sprintf("-- Running test suite for %s --\n", rv$name)))
    }

    rv$footer <- function () {
        str = sprintf("Tests Passed: %g/%d --\n", rv$passed.count, rv$total.count)
        if (rv$total.count == rv$passed.count) {
            cat(cyan(sprintf("-- %s : All %s",rv$name, str)))
        } else if (rv$passed.count > 0) {
            cat(yellow(sprintf("-- %s : Some %s",rv$name, str)))
        } else {
            cat(red(sprintf("-- %s : No %s", rv$name, str)))
        }
    }

    rv
}
