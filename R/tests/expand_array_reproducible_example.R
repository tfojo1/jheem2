# Minimal reproducible example of expand.array issue

source('R/HELPERS_array_helpers.R')

arr = array(1:2, dim=2, dimnames=list(year=c('2010', '2011')))
target.dimnames = list(year=c('2010', '2011'), sex=c('male', 'female'))
expanded.arr = expand.array(to.expand = arr,
                            target.dim.names = target.dimnames)

# Error in .Call(<pointer: (nil)>, dst_array, src_array) : 
# NULL value passed as symbol address