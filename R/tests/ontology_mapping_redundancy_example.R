# To use: try with only the sex-sex mapping registered, then try again with the sex-risk-sex-risk mapping registered as well.

ONTOLOGY.MAPPING.MANAGER = new.env()
ONTOLOGY.MAPPING.MANAGER$mappings=list()

# register.ontology.mapping('jheem.to.cdc.sex.risk',
#                           from.dimensions = c('sex', 'risk'),
#                           to.dimensions = c('sex', 'risk'),
#                           mappings = rbind(c('msm', 'never_IDU', 'male', 'msm'),
#                                            c('msm', 'active_IDU', 'male', 'msm_idu'),
#                                            c('msm', 'IDU_in_remission', 'male', 'msm_idu'),
#                                            c('heterosexual_male', 'never_IDU', 'male', 'heterosexual'),
#                                            c('heterosexual_male', 'active_IDU', 'male', 'idu'),
#                                            c('heterosexual_male', 'IDU_in_remission', 'male', 'idu'),
#                                            c('female', 'never_IDU', 'female', 'heterosexual'),
#                                            c('female', 'active_IDU', 'female', 'idu'),
#                                            c('female', 'IDU_in_remission', 'female', 'idu'))
# )

register.ontology.mapping('jheem.to.cdc.sex',
                          from.dimensions = 'sex',
                          to.dimensions = 'sex',
                          mappings = rbind(c('heterosexual_male', 'male'),
                                           c('msm', 'male'),
                                           c('female', 'female')))

ont1 = ontology(sex = c('heterosexual_male', 'msm', 'female'),
                risk = c('never_IDU', 'active_IDU', 'IDU_in_remission'))
ont2 = ontology(sex = c('male', 'female'))
mp = get.mappings.to.align.ontologies(ont1, ont2, allow.non.overlapping.incomplete.dimensions = T)
mp1 = mp[[1]]

# When the likelihood doesn't need the simulation's 'risk' because the data requested doesn't have it, then a matrix should be made without 'risk'
from.dimnames = ont1['sex']
to.dimnames = ont2
mp1$get.matrix(from.dim.names = from.dimnames, to.dim.names = to.dimnames) # Will fail if the mapping with risk was used

