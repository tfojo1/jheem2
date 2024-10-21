cdc = SURVEILLANCE.MANAGER$ontologies$cdc[setdiff(names(cdc), 'location')]
brfss = SURVEILLANCE.MANAGER$ontologies$brfss[setdiff(names(brfss), 'location')]

source('R/ONTOLOGY_ontology_mappings.R')


register.ontology.mapping('cdc.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'other'),
                                           c('Asian', 'other'),
                                           c('Black/African American', 'black'),
                                           c('Hispanic/Latino', 'hispanic'),
                                           c('Multiracial', 'other'),
                                           c('Native Hawaiian/Other Pacific Islander', 'other'),
                                           c('White', 'other'))
)
register.ontology.mapping('proportion.tested.to.jheem.race',
                          from.dimension = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'other'),
                                           c('Asian', 'other'),
                                           c('Black', 'black'),
                                           c('Hispanic', 'hispanic'),
                                           c('Multiracial', 'other'),
                                           c('Native Hawaiian/Other Pacific Islander', 'other'),
                                           c('Other race', 'other'),
                                           c('White', 'other')))


get.mappings.to.align.ontologies(cdc[c('year','race','sex')], brfss[c('year','race','sex')], align.on.dimensions = 'race', allow.non.overlapping.incomplete.dimensions = T)
get.mappings.to.align.ontologies(cdc[c('year','race')], brfss[c('year','race')], align.on.dimensions = 'race', allow.non.overlapping.incomplete.dimensions = T)

get.mappings.to.align.ontologies(cdc['race'], brfss['race']) # returns mappings
get.mappings.to.align.ontologies(cdc['race'], brfss['race'], align.on.dimensions = 'sex')
get.mappings.to.align.ontologies(cdc, brfss, align.on.dimensions = 'race') # returns NULL for some reason. Also if use allow... = T

get.mappings.to.align.ontologies(cdc[c('sex')], brfss[c('sex')], align.on.dimension = 'race') # returns mappings
get.mappings.to.align.ontologies(cdc[c('sex','risk')], brfss[c('sex','risk')], align.on.dimension = 'sex') # returns mappings


get.mappings.to.align.ontologies(cdc, brfss, allow.non.overlapping.incomplete.dimensions = T) # returning NULL, haven't looked at why but not the bug this is about
get.mappings.to.align.ontologies(cdc['race'], brfss['race'], align.on.dimensions = 'race') # returns mappings
