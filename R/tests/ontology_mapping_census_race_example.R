
source('R/ONTOLOGY_ontology_mappings.R')

register.ontology.mapping('census.to.jheem.race',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('white', 'hispanic', 'hispanic'),
                                           c('white', 'not hispanic', 'other'),
                                           c('black', 'hispanic', 'hispanic'),
                                           c('black', 'not hispanic', 'black'),
                                           c('american indian or alaska native', 'hispanic', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic', 'other'),
                                           c('asian or pacific islander', 'hispanic', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic', 'other')))

mp = ONTOLOGY.MAPPING.MANAGER$mappings$census.to.jheem.race
x = mp$from.dim.names
y = mp$to.dim.names
get.ontology.mapping(x, y) # NULL
get.mappings.to.align.ontologies(x, y) # NULL
