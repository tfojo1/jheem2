# ontology mapping case 1 failure example
# this reflects what happens during a pull where the previously found common
# ontology is now used as a target ontology, with allow.mapping.to.align.ontologies
# set to FALSE, therefore calling 'get.ontology.mapping' instead of
# 'get.mappings.to.align.ontologies'

register.ontology.mapping('lump.other.risk.and.heterosexual',
                          from.dimensions = 'risk',
                          to.dimensions = 'risk',
                          mappings = rbind(c('msm', 'msm'),
                                           c('idu', 'idu'),
                                           c('msm_idu', 'msm_idu'),
                                           c('heterosexual', 'heterosexual'),
                                           c('other', 'heterosexual'))
)

register.ontology.mapping('jheem.to.cdc.sex.risk',
                          from.dimensions = c('sex', 'risk'),
                          to.dimensions = c('sex', 'risk'),
                          mappings = rbind(c('msm', 'never_IDU', 'male', 'msm'),
                                           c('msm', 'active_IDU', 'male', 'msm_idu'),
                                           c('msm', 'IDU_in_remission', 'male', 'msm_idu'),
                                           c('heterosexual_male', 'never_IDU', 'male', 'heterosexual'),
                                           c('heterosexual_male', 'active_IDU', 'male', 'idu'),
                                           c('heterosexual_male', 'IDU_in_remission', 'male', 'idu'),
                                           c('female', 'never_IDU', 'female', 'heterosexual'),
                                           c('female', 'active_IDU', 'female', 'idu'),
                                           c('female', 'IDU_in_remission', 'female', 'idu'))
)

register.ontology.mapping('cdc.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'other'),
                                           c('Asian', 'other'),
                                           c('Black/African American', 'black'),
                                           c('Hispanic/Latino', 'hispanic'),
                                           c('Multiracial', 'other'),
                                           c('Native Hawaiian/Other Pacific Islander', 'other'),
                                           c('White', 'other')) # is this really what the jheem race categories are?
)

data.ontology = ontology(year = as.character(2010:2013),
                         location = c('24003', '24004'),
                         sex = c('male', 'female'),
                         race = c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
                         incomplete.dimensions = c('year', 'location'))

common.ontology = ontology(year = as.character(2010:2011),
                           location = '24003',
                           age = c('young', 'old'),
                           risk = c('msm', 'msm_idu', 'idu', 'heterosexual', 'other'),
                           sex = c('male', 'female'),
                           race = c('black', 'hispanic', 'other'),
                           incomplete.dimensions = 'year')

get.ontology.mapping(data.ontology, common.ontology)

# What I see during a browser inside do.get.ontology.mapping:
# 'from.has.all.required' is FALSE
# 'from.out.of.alignment.mask' is never initialized and causes error