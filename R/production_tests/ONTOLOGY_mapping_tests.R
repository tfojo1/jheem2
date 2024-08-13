

ont1 = ontology(year = as.character(2011:2020),
                race = c('black','hispanic','other'),
                incomplete.dimensions = 'year')

ont2 = ontology(year = c('2011-2015','2016-2020'),
                race = c('black','hispanic','other'),
                incomplete.dimensions = 'year')

map = get.ontology.mapping(ont1, ont2, allow.non.overlapping.incomplete.dimensions = F)
map = get.mappings.to.align.ontologies(ont1, ont2, allow.non.overlapping.incomplete.dimensions = T)
# Should give a year mapping one way, and an identity mapping the other way



ont1 = ontology(year = as.character(2011:2020),
                race = c('black','hispanic','other'),
                incomplete.dimensions = 'year')

ont2 = ontology(year = c('2008-2015','2016-2022'),
                race = c('black','hispanic','other'),
                incomplete.dimensions = 'year')

map = get.mappings.to.align.ontologies(ont1, ont2, allow.non.overlapping.incomplete.dimensions = T)
# Should give two identity mappings