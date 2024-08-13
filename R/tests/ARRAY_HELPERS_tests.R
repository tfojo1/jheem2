# testing do_array_overwrite cpp function

# source('R/DATA_MANAGER_data_manager.R')
source('R/HELPERS_array_helpers.R')
Rcpp::sourceCpp('src/array_helpers.cpp')
# Rcpp::sourceCpp('src/andrew_test.cpp')
library(reshape2) #installed to make a test data frame from my array easily

# This yielded no problems
dim.names = list(
    location = c('Alabama'),
    year = as.character(2008:2019),
    age = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
    risk = c("idu")
)

test.arr = array(4+(1:prod(sapply(dim.names, length))), dim=(sapply(dim.names, length)), dimnames=dim.names)
original.arr = test.arr

test.data = reshape2::melt(test.arr)
test.data[] = lapply(test.data, as.character) # melt makes it factors
test.data$value = as.numeric(test.data$value)

dimensions = names(dim.names)

# dst_array.attr("dimnames") is a list of the four dimensions with all their possible keys
# dst_dimensions is a CharacterVector of "location", "year", "age", "risk"
# dst_dims is an IntegerVector of 1, 12, 5, 1 with names
# n_dst_dims is the integer 4
# access_dimensions is a CharacterVector of "location", "year", "age", "risk"
# max_dim_length is an integer: 12
# write_dims will look like dst_dims (1, 12, 5, 1), only is an array, so no names
# write_to_dst_dim_values has size [4][12] and looks like
#       [0, null, null, null, null, ... ], [0,1,2,3,4,5,6,7,8,9,10,11], [0,1,2,3,4,null, null...], [0, null...]
# dimension_values.length() is 4
# "elem", which probably means "elements", IS a CharacterVector
# n_src_dims is just 0
# n_write is 60
# n_before_src is array of length 0...
# n_before_dst is array of length 4 and is [1, 1, 12, 60], some actual algo here
# write_dim_values starts at [0, 0, 0, 0]
# dst_index when k=0 will be 0...

# src_array[src_index] = 1 as it should for my test array
# HERE IT IS??? dst_index was set to 1 instead of 0 at some point by the end of the first k loop.
# at k = 0 and before updates at the end of the loop, what are the variable values?
# n_before_dst
# write_to_dst_dim_values
# write_dim_values are indeed [0,0,0,0] as they should be
# looks like dst_index is turned to 1 on the first j loop.
# INTERESTING... the write_to_dst_dim_values[][] returned 1 instead of 0...
# write_to_dst_dim_values[0][0] = 1. When did that happen?
# WAIT... now it's saying that write_to_dst_dim_values[0][write_dim_values[0]] = 1
# but write_dim_values is still all zeroes.

# this array should have size 2*5*3=30 indices total
Rcpp::sourceCpp('src/array_helpers.cpp')
test.arr = array(1:prod(sapply(dim.names, length)), dim=(sapply(dim.names, length)), dimnames=dim.names)
do_array_overwrite(test.arr, test.data[i, 'value'], as.list(test.data[i,dimensions])) 

i = 1
test.output = do_array_overwrite_part1(test.arr, test.data[i, 'value'], as.list(test.data[i,dimensions])) # mine works
do_array_overwrite(test.arr, test.data[i, 'value'], as.list(test.data[i,dimensions])) # original doesn't
array.access(test.arr, dimension.values = as.list(test.data[i,dimensions])) = test.data[i,'value']

for (i in 1:nrow(test.data)) {
    array.access(test.arr, dimension.values = as.list(test.data[i,dimensions])) = test.data[i,'value']
    #do_array_overwrite(test.arr, test.data[i, 'value'], as.list(test.data[i,dimensions]))
}

# sum(test.arr != original.arr) # zero is a success