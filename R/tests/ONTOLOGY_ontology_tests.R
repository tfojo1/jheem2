ont = ontology(year=as.character(2000:2020),
               age=c('young','old'), race=c('black','other'),
               incomplete.dimensions='year')

ont2 = ont
ont2['year'] = list(2000:2030)
ont2['age'] = list('young')
ont2['race'] = list(c('black','hispanic','other')) #should not work
ont2

ont2 = ont
ont2[['year']] = 2000:2030
ont2[['race']] = 'hispanic' # should not work
ont2$race = 'hispanic'
ont2$sex = 'male' # should not work
ont2[c('year','sex')] = list(2000:2020, c('male','female'))

arr = array(0, dim=sapply(ont, length), dimnames=ont)

class(dimnames(arr))

arr2 = arr[,1,drop=F]
class(dimnames(arr2))