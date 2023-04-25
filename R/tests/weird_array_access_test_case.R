

source('R/HELPERS_array_helpers.R')
Rcpp::sourceCpp('src/array_helpers.cpp')


dim.names = list(year=as.character(2000:2001), location='Alabama')

dim.names = list(location='Alabama',
                 year=as.character(2008:2019),
                 age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                 risk = 'idu')

arr = array(1:prod(sapply(dim.names, length)), dim=sapply(dim.names, length), dimnames=dim.names)

arr
array.access(arr, list(location='Alabama', year='2008', age='13-24 years', risk='idu')) = 1000
arr
