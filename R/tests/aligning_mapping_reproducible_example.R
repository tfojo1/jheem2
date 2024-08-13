
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

with.complete = ontology(location="MD",
                         risk = c("never_IDU", "active_IDU", "IDU_in_remission"),
                         sex = c("heterosexual_male", "msm", "female"))
with.incomplete = ontology(location="MD",
                           risk = c("never_IDU", "active_IDU", "IDU_in_remission"),
                           sex = c("heterosexual_male", "msm", "female"),
                           incomplete.dimensions = 'location')
target = ontology(location=c("MD", "AZ"),
                  risk=c("msm", "idu", "msm_idu", "heterosexual"),
                  sex=c("male", "female"),
                  incomplete.dimensions = 'location')

map.from.complete = get.mappings.to.align.ontologies(with.complete, target) # success
map.to.complete = get.mappings.to.align.ontologies(target, with.complete) # failure
map.from.incomplete = get.mappings.to.align.ontologies(with.incomplete, target) # success
map.to.incomplete = get.mappings.to.align.ontologies(target, with.incomplete) # success

print(paste0("with.complete --> target: ", !is.null(map.from.complete)))
print(paste0("target --> with.complete: ", !is.null(map.to.complete)))
print(paste0("with.incomplete --> target: ", !is.null(map.from.incomplete)))
print(paste0("target --> with.incomplete: ", !is.null(map.to.incomplete)))
