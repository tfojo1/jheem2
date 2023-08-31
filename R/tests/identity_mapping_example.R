# Transformation matrix reproducible example
# of error in getting matrix from identity mapping

from.dimnames = list(year=as.character(2010:2013),
                     sex = c('male', 'female'),
                     language = c('English', 'Spanish'))
to.dimnames = list(year=as.character(c(2011,2012,2013,2010)),
                   sex = c('female', 'male'))

from.ontology = as.ontology(from.dimnames, incomplete.dimensions = 'year')
to.ontology = as.ontology(to.dimnames, incomplete.dimensions = 'year')

mapping1 = get.identity.ontology.mapping()

transformation.matrix1 = mapping1$get.matrix(from.dim.names=from.dimnames,
                                           to.dim.names=to.dimnames)
# This is square but should not be

from.arr = array(1:prod(sapply(from.dimnames, length)), dim=sapply(from.dimnames, length), dimnames=from.dimnames)
to.arr = array(1:prod(sapply(to.dimnames, length)), dim=sapply(to.dimnames, length), dimnames=to.dimnames)

transformation.matrix1 %*% from.arr == as.numeric(rowSums(from.arr, dims=2)[to.dimnames$year,to.dimnames$sex])
