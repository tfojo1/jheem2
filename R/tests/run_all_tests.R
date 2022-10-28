
library(testit)

source("test_framework.R")

root.directory = move.wd.to.root()
# Now we are in the R/ directory

# Find all the test files below the root ( *_test.R files )
test.files = list.files(".", "*_test.R", recursive=T)

# Split them on the "/"
path.splits = strsplit(test.files,"/")

# I couldn't figure out how to avoid a loop here
for (i in 1:length(test.files)) {

    dir.path = path.splits[[i]][1:length(path.splits[[i]]) - 1]

    setwd(sprintf("%s/%s", root.directory, dir.path))

    file.path = path.splits[[i]][length(path.splits[[i]])]

    source ( file.path )

    move.wd.to.root()
}
