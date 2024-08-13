# source('R/LIKELIHOODS_basic_likelihood.R')
# source('R/LIKELIHOODS_main.R')
# source('R/LIKELIHOODS_joint_likelihood.R')
# source('R/LIKELIHOODS_bernoulli_likelihood.R')
# source('R/LIKELIHOODS_nested_proportion_likelihood.R')
# 
# source('R/JHEEM_entity.R')
# 
# source('R/SPECIFICATION_scales.R')
# source('R/HELPERS_misc_helpers.R')
# source('R/DATA_MANAGER_data_manager.R')
# source('R/ONTOLOGY_ontology.R')
# source('R/ONTOLOGY_ontology_mappings.R')
# source('R/HELPERS_array_helpers.R')
# source('R/HELPERS_dim_names_helpers.R')
# source('R/SPECIFICATION_model_specification.R') #has the outcome.metadata object definition
# Rcpp::sourceCpp('src/array_helpers.cpp')
# Rcpp::sourceCpp('src/ontology_mappings.cpp')
# Rcpp::sourceCpp('src/likelihood_helpers.cpp')
# Rcpp::sourceCpp('src/nested_proportion_likelihood.cpp')
# library(locations)
# 
# source('R/tests/make_dummy_sim.R')
# # #
source('../jheem_analyses/applications/EHE/ehe_specification.R')
# 
# # surveillance.manager
# # Takes about 20 seconds
surveillance.manager = load.data.manager('../jheem_analyses/cached/surveillance.manager_111623.rdata')
#
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

# p.biases = get.p.bias.estimates(surveillance.manager,
#                                 dimensions = c('age', 'sex', 'risk'),
#                                 levels.of.stratification = c(0,1),
#                                 outcome.for.p = 'suppression',
#                                 outcome.for.n = 'diagnosed.prevalence',
#                                 sub.location.type = 'COUNTY',
#                                 super.location.type = 'STATE')
# 
# 
adjust.array = function(arr, version)
{
    # return an array that partitions IDU into 25% active and 75% remission
    partitioning.arr.dimnames = list(risk = c('active_IDU', 'IDU_in_remission'))
    partitioning.arr = array(c(0.25, 0.75), dim =sapply(partitioning.arr.dimnames, length), partitioning.arr.dimnames)

    modified = array.access(arr, dimnames(partitioning.arr))
    modified = modified * expand.array(partitioning.arr, dimnames(modified))
    array.access(arr, dimnames(modified)) = modified
    arr

}


instr = create.nested.proportion.likelihood.instructions(outcome.for.data = 'suppression',
                                                         denominator.outcome.for.data = 'diagnosed.prevalence',
                                                         outcome.for.sim = 'proportion.tested',
                                                         denominator.outcome.for.sim = 'diagnosed.prevalence',
                                                         location.types = c('STATE', 'COUNTY'),
                                                         minimum.geographic.resolution.type = 'COUNTY',
                                                         dimensions = c('sex', 'risk', 'race'),
                                                         levels.of.stratification = c(0,1),
                                                         from.year = 2010,
                                                         to.year = 2021,
                                                         omit.years = 2017,
                                                         measurement.error.sd = 0.05, #?
                                                         within.location.p.error.correlation = 0.5,
                                                         within.location.n.error.correlation = 0.5,
                                                         weights = list(),
                                                         partitioning.function = adjust.array,
                                                         p.bias.inside.location = 0.01,
                                                         p.bias.outside.location = 0.02,
                                                         p.bias.sd.inside.location = 0.1,
                                                         p.bias.sd.outside.location = 0.15)

lik = instr$instantiate.likelihood(version = 'ehe',
                                   location = 'C.12580',
                                   data.manager = surveillance.manager,
                                   throw.error.if.no.data = F)
calculated.likelihood=lik$compute(sim=make.dummy.sim(version='ehe',
                                                     location='C.12580',
                                                     from.year=2010,
                                                     to.year=2021),
                                  check.consistency = F,
                                  log = T,
                                  debug = T)
