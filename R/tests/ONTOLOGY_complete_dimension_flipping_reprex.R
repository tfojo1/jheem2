ont = ontology(year=NULL, sex=c('male','female'))
is.complete(ont)
ont$year = '2010'
is.complete(ont)
# year is now complete
# Use case: ontologies' incomplete dimensions are updated during a PUT