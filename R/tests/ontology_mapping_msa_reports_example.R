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
                                           c('White', 'other'))
)

register.ontology.mapping('cdc.msa.reports.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'other'),
                                           c('Asian', 'other'),
                                           c('Black/African American', 'black'),
                                           c('Hispanic/Latino', 'hispanic'),
                                           c('White', 'other')))

ont.cdc = ontology(year = as.character(2017:2022),
                   location = c('AK', 'WY'),
                   age = c('young', 'old'),
                   race = c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
                   sex = c('male', 'female'),
                   risk = c('msm', 'idu', 'msm_idu', 'heterosexual', 'other'),
                   incomplete.dimensions = c('year', 'location'))
ont.cdc.msa.reports = ontology(year = as.character(1992:2006),
                               location = c('C.12580'),
                               age = c('young', 'old'),
                               race = c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'White'),
                               sex = c('male', 'female'),
                               risk = c('msm', 'idu', 'msm_idu', 'heterosexual', 'other'),
                               incomplete.dimensions = c('year', 'location'))
mmm = get.mappings.to.align.ontologies(ont.cdc, ont.cdc.msa.reports, allow.non.overlapping.incomplete.dimensions = T)

# both mappings are "A combination of 2 ontology mappings ('lump.other.risk.and.heterosexual' and 'cdc.to.jheem.race') over dimensions 'race' and 'risk'", only the second mapping has
# 'cdc.msa.reports.to.jheem.race'

# Don't we just need each mapping to be a basic ontology mapping over 'race'?