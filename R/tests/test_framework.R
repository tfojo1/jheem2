
# Function to convert a module and src filename into the proper format
# for testing individual files as well as testing all at once.
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

# Function to move the working directory from somewhere in the tree
# to the R/ directory
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

# Function to create the suite of functions to test a particular input
# to a function
create.test.function.suite <- function ( name, f, var ) {
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
            cat(orange(sprintf("-- %s : Some %s",rv$name, str)))
        } else {
            cat(red(sprintf("-- %s : No %s", rv$name, str)))
        }
    }

    rv
}
