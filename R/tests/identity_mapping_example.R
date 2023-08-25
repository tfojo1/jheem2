# Transformation matrix reproducible example
# of error in getting matrix from identity mapping

from.dimnames = list(year=as.character(2010:2013),
                     sex = c('male', 'female'),
                     language = c('English', 'Spanish'))
to.dimnames = list(year=as.character(2010:2013),
                   sex = c('male', 'female'))

from.ontology = as.ontology(from.dimnames, incomplete.dimensions = 'year')
to.ontology = as.ontology(to.dimnames, incomplete.dimensions = 'year')

mapping1 = get.identity.ontology.mapping()

transformation.matrix1 = mapping1$get.matrix(from.dim.names=from.dimnames,
                                           to.dim.names=to.dimnames)
# This is square but should not be