# MINIMUM REQUIRED SOURCES
source('../jheem2/R/ONTOLOGY_ontology.R')
source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
Rcpp::sourceCpp('../jheem2/src/ontology_mappings.cpp')
source('../jheem2/R/HELPERS_misc_helpers.R')
source('../jheem2/R/HELPERS_dim_names_helpers.R')
source('../jheem2/R/HELPERS_age_year_helpers.R')

# This isn't an issue about year ranges anymore, but about making sure we only have relevant mappings when we end up subsetting our universal ontology in the pull function
# This example describes a situation in which adult.immigration data is pulled with keep.dimensions 'year' and 'race' and dimension values location='C.12060'.
target.as.supplied.in.pull.arguments = ontology(year=as.character(1970:2030),
                                                location='C.12060',
                                                age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                                race=c('black', 'hispanic', 'other'),
                                                sex=c('heterosexual_male', 'msm', 'female'),
                                                incomplete.dimensions = c('year', 'location'))
universal.after.picking.desired.dimensions = ontology(year='2011-2015',
                                                      location=c('C.12060', 'C.12580'),
                                                      race=c('black', 'hispanic', 'other'),
                                                      incomplete.dimensions = c('year', 'location'))

### WITHOUT SOURCING EHE MAPPINGS: A BASIC MAPPING
get.ontology.mapping(from.ontology=target.as.supplied.in.pull.arguments,
                     to.ontology=universal.after.picking.desired.dimensions)

### AFTER SOURCING THE MAPPINGS: A COMBINATION MAPPING
source('../jheem_analyses/applications/EHE/ehe_ontology_mappings.R')
get.ontology.mapping(from.ontology=target.as.supplied.in.pull.arguments,
                     to.ontology=universal.after.picking.desired.dimensions)




# # When we actually generate the universal ontology in the pull function, it would have had sex in it, and the returned mappings would have a sex mapping.
# # I had the idea to just get another mapping after we subset it, but somehow it's deciding to have a sex mapping anyways. Is this a bug?
# 
# # Previously, I had been using the mapping given back from the universal ontology generation code, which told us how to map from the supplied target to this, below:
universal.as.generated = ontology(year='2011-2015',
                                  location=c('C.12060', 'C.12580'),
                                  age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                  race=c('black', 'hispanic', 'other'),
                                  sex=c('heterosexual_male', 'msm', 'female'),
                                  incomplete.dimensions = c('year', 'location'))
# 
# # Clearly, that mapping will have a sex mapping in it, which we won't want anymore after we discard age and sex from the universal ontology.
# # Since it's not necessarily even possible to remove part of a mapping, it is clear we have to just generate a new mapping from the supplied target to the slimmed-down universal.