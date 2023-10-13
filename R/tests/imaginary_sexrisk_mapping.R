ont1 = ontology(sex = c('male', 'female'),
                risk = c('idu', 'heterosexual'))
ont2 = ontology(sexrisk = c('male_idu', 'male_heterosexual', 'female_idu', 'female_heterosexual'))

register.ontology.mapping('imaginary.sex.risk',
                          from.dimensions = c('sex', 'risk'),
                          to.dimensions = c('sexrisk'),
                          mappings = rbind(c('male', 'idu', 'male_idu'),
                                           c('male', 'heterosexual', 'male_heterosexual'),
                                           c('female', 'idu', 'female_idu'),
                                           c('female', 'heterosexual', 'female_heterosexual'))
)
get.ontology.mapping(ont1, ont2)