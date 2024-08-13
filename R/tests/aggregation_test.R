source('R/HELPERS_array_helpers.R')
Rcpp::sourceCpp('src/array_helpers.cpp')

test.arr = array(10*1:24, dim=c(age=4, race=3, sex=2),
                 dimnames=list(age=c('age1','age2','age3','age4'),
                               race=c('black','hispanic','other'),
                               sex=c('female','male')))

test.dv = list(age=c('age1', 'age3'))

test.keep = c('race')

x = array.access(test.arr, test.dv)

# x still has sex dimension, but this is good since we haven't checked
# if we want to remove it or keep it yet (keep.dimensions would tell us)

# let's aggregate the ages together.
# for this, we need to know about the outcome. Is its scale nonnegative number?
outcome = 'new'
# scale = private$i.outcome.info[[outcome]][['metadata']][['scale']]
scale = 'non.negative.number'

margin.to.aggregate = setdiff(1:length(dim(x)), which(names(dim(x)) %in% names(test.dv)))

finished = apply(x, margin.to.aggregate, FUN = sum)

test.arr.2 = array(as.list(3*1:24), dim=c(age=4, race=3, sex=2),
                   dimnames=list(age=c('age1','age2','age3','age4'),
                                 race=c('black','hispanic','other'),
                                 sex=c('female','male')))
test.arr = array(as.list(letters[1:24]), dim=c(age=4, race=3, sex=2),
                 dimnames=list(age=c('age1','age2','age3','age4'),
                               race=c('black','hispanic','other'),
                               sex=c('female','male')))

arr.list = list('a','a','a','a','a','a',
                'b','b','b','b','b','b',
                'c','c','c','c','c','c',
                c('d', 'f'), c('d', 'f'), c('d', 'f'), c('d', 'f'), c('d', 'f'),c('d', 'f'))


test.arr = array(arr.list, dim=c(age=4, race=3, country=1, sex=2),
                 dimnames=list(age=c('age1','age2','age3','age4'),
                               race=c('black','hispanic','other'),
                               country=c('USA'),
                               sex=c('female','male')))

# what I want when I aggregate over the sex margin is an array with 1
# dimension left that is sex, where the first element is a character vector of
# c('a', 'b') and the second element is the vector c('c', 'd', 'f')

# How do I drop a dimensions, such as race, here? If the dimension I am dropping
# is indeed of length 1, then the array list/vector is already in the correct
# arrangement and can just have its dims and dimnames redone.

old.dim = dim(test.arr)
old.dimnames = dimnames(test.arr)

new.dim = old.dim[!(names(old.dim) %in% c('country'))]
new.dimnames = old.dimnames[!(names(old.dimnames) %in% c('country'))]
