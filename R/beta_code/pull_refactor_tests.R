# TESTS FOR NEW PULL REFACTOR

# SIMPLEST - count, no mapping, no aggregation
SURVEILLANCE.MANAGER$pull('adult.population', keep.dimensions='year', dimension.values=list(location='C.12580'))

# count, no mapping, with aggregation
SURVEILLANCE.MANAGER$pull('adult.population', keep.dimensions='year', dimension.values=list(location=c('C.12580', 'C.16980')))

# fraction, no mapping, no aggregation
SURVEILLANCE.MANAGER$pull('proportion.tested', keep.dimensions='year', dimension.values=list(location='C.12580'))

# fraction, no mapping, with aggregation
# note that this uses just one source: brfss
SURVEILLANCE.MANAGER$pull('proportion.tested', keep.dimensions='year', dimension.values=list(location=c('C.12580', 'C.16980')))

# count, with mapping and aggregation
JHEEM.ONT = ontology(year=as.character(2010:2013),
                     location = 'C.12580',
                     age = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                     race=c('black','hispanic','other'),
                     sex= c('heterosexual_male', 'msm', 'female'),
                     risk = c('never_IDU', 'active_IDU', 'IDU_in_remission'),
                     incomplete.dimensions = c('year', 'location'))
source("../jheem_analyses/applications/EHE/ehe_ontology_mappings.R")
SURVEILLANCE.MANAGER$pull('adult.population', keep.dimensions=c('year', 'race'), dimension.values=list(location=c('C.12580', 'C.16980')),
                          target.ontology = JHEEM.ONT)

# fraction, with mapping and aggregation
# SURVEILLANCE.MANAGER$pull('proportion.tested', keep.dimensions=c('year', 'race'), dimension.values=list(location=c('C.12580', 'C.16980')),
#                           target.ontology = JHEEM.ONT)
# this might be failing because the AGE fixes Todd made might not be incorportated here

SURVEILLANCE.MANAGER$pull('suppression', keep.dimensions=c('year', 'race'), dimension.values=list(location=c('C.33100', 'C.40900'), year='2018'),
                          target.ontology = JHEEM.ONT)

# We have COV for awareness and variance for proportion.tested.

# fraction's variance, no mapping, no aggregation
SURVEILLANCE.MANAGER$pull('proportion.tested', metric='variance', keep.dimensions='year', dimension.values=list(location='C.12580'))

# fractions' COV, no mapping, no aggregation
SURVEILLANCE.MANAGER$pull('awareness', metric='coefficient.of.variance', keep.dimensions='year', dimension.values=list(location='C.31080'))

# fraction's variance, no mapping, with aggregation
SURVEILLANCE.MANAGER$pull('proportion.tested', metric='variance', keep.dimensions='year', dimension.values=list(location=c('C.12580', 'C.33100')))

# fractions' COV, no mapping, with aggregation
SURVEILLANCE.MANAGER$pull('awareness', metric='coefficient.of.variance', keep.dimensions='year', dimension.values=list(location=c('C.31080', 'C.33100')))

# if we have covA and covB, estA and estB, and dnmA and dnmB...
# then we have sdA = covA * estA and sdB = covB * estB
# and so varA = sdA**2 and varB = sdB**2
# and then to aggregate the variances, we have
# (varA * dnmA**2 + varB * dnmB...**2) / (dnmA**2 + dnmB...**2)
# which is (covA**2 * dnmA**4 + covB**2 * dnmB...**4) / (dnmA**2 + dnmB...**2)

# fraction's COV, with mapping and aggregation
# CANNOT TEST BECAUSE AWARENESS DOESN'T HAVE STRATIFIED COV AND PROP TESTED CAN'T MAP RIGHT NOW
