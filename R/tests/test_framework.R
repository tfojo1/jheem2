
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
