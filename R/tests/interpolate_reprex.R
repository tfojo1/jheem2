arr.dimnames = list(year=c('2012', '2013', '2014'), location=c('here', 'there'),sex=c('male', 'female'))
arr = array(1:12, sapply(arr.dimnames, length),arr.dimnames)
arr['2013', 'here', 'female']=NA
arr.list.by.year=lapply(arr.dimnames$year, function(y) {array.access(arr, year=y)})
arr.interpolated = interpolate(arr.list.by.year, arr.dimnames$year, arr.dimnames$year) # didn't work as intended?