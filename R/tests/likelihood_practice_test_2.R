# source('R/LIKELIHOODS_basic_likelihood.R')
# source('R/LIKELIHOODS_main.R')
# source('R/LIKELIHOODS_joint_likelihood.R')
# source('R/LIKELIHOODS_bernoulli_likelihood.R')
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
# library(locations)
# 
# source('R/tests/make_dummy_sim.R')

source('../jheem_analyses/applications/EHE/ehe_specification.R')

# surveillance.manager
# Takes about 20 seconds
# surveillance.manager = load.data.manager('../jheem_analyses/cached/surveillance.manager_102323.rdata')
#
# register.ontology.mapping('lump.other.risk.and.heterosexual',
#                           from.dimensions = 'risk',
#                           to.dimensions = 'risk',
#                           mappings = rbind(c('msm', 'msm'),
#                                            c('idu', 'idu'),
#                                            c('msm_idu', 'msm_idu'),
#                                            c('heterosexual', 'heterosexual'),
#                                            c('other', 'heterosexual'))
# )
# 
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
# 
# register.ontology.mapping('cdc.to.jheem.race',
#                           from.dimensions = 'race',
#                           to.dimensions = 'race',
#                           mappings = rbind(c('American Indian/Alaska Native', 'other'),
#                                            c('Asian', 'other'),
#                                            c('Black/African American', 'black'),
#                                            c('Hispanic/Latino', 'hispanic'),
#                                            c('Multiracial', 'other'),
#                                            c('Native Hawaiian/Other Pacific Islander', 'other'),
#                                            c('White', 'other')) # is this really what the jheem race categories are?
# )
# register.ontology.mapping('cdc.msa.reports.to.jheem.race',
#                           from.dimensions = 'race',
#                           to.dimensions = 'race',
#                           mappings = rbind(c('American Indian/Alaska Native', 'other'),
#                                            c('Asian', 'other'),
#                                            c('Black/African American', 'black'),
#                                            c('Hispanic/Latino', 'hispanic'),
#                                            c('White', 'other')))


# register.basic.likelihood.instructions(code = 'a-b.1',
#                                        description = 'my first instructions',
#                                        dimensions = c('age', 'sex', 'risk', 'race'),
#                                        outcome.for.data = 'diagnoses',
#                                        outcome.for.sim = 'new',
#                                        stratifications = list(
#                                            # character()),#,
#                                            'sex','risk','race',
#                                            c('age','sex'), c('race','sex'), c('risk','sex'), c('risk','race')),
#                                            # c('risk', 'sex')),
#                                        years = 2009:2013,
#                                        correlation.different.years = 0.5,
#                                        correlation.different.strata = 0.1,
#                                        correlation.different.sources = 0.3,
#                                        correlation.same.source.different.details = 0.3,
#                                        measurement.error.coefficient.of.variance = 0.5,
#                                        weights = create.likelihood.weights(5,
#                                                                            create.likelihood.subweights(2, list(race='hispanic')),
#                                                                            create.likelihood.subweights(3, list(risk='msm_idu'))))


# Bernoulli.instr = create.bernoulli.likelihood.instructions(outcome.for.sim = 'new',
#                                                            dimensions = c('age', 'sex', 'risk', 'race'),
#                                                            levels.of.stratification = c(0,1),
#                                                            years = c(2002, 2012),
#                                                            probability.decreasing = 0.95,
#                                                            weights = list(2))
# Bernoulli.instr.reg = register.likelihood.instructions(Bernoulli.instr, 'bern1', 'Bernoulli likelihood instructions')
# Bernoulli.like = Bernoulli.instr.reg$instantiate.likelihood(version = 'ehe',
#                                                             location = "MD")
# Bern.comp = Bernoulli.like$compute(sim=make.dummy.sim(version='ehe',
#                                                       location='MD',
#                                                       from.year=2000,
#                                                       to.year=2023),
#                                    check.consistency = T,
#                                    log = T)

# my.instr = create.basic.likelihood.instructions(outcome.for.data = 'diagnosed.prevalence',
#                                                 outcome.for.sim = 'diagnosed.prevalence',
#                                                 denominator.outcome.for.sim = 'population',
#                                                 # dimensions = c('age', 'race'),
#                                                 dimensions = c('age', 'sex', 'risk', 'race'),
#                                                 levels.of.stratification = c(0,1), # NOTE: IF WE HAVE ONLY 2 DIMENSIONS, 0 IS THE SAME AS 2! WE DON'T WANT TO PULL TWICE FOR THE SAME STRAT.
#                                                 omit.years = 2020,
#                                                 measurement.error.coefficient.of.variance = 0.5,
#                                                 weights = list(4,
#                                                                c(3,5),
#                                                                create.likelihood.weights(8, dimension.values = list(race=c('hispanic', 'black'), sex='male')),
#                                                                create.likelihood.weights(1/2, dimension.values = list(risk=c('idu'))),
#                                                                create.likelihood.weights(5, dimension.values = list(race='hispanic', age = c('13-24 years'))),
#                                                                c(1/3)),
#                                                 equalize.weight.by.year = T)

my.instr = create.basic.likelihood.instructions(outcome.for.data = 'diagnosed.prevalence',
                                                outcome.for.sim = 'diagnosed.prevalence',
                                                denominator.outcome.for.sim = 'population',
                                                # dimensions = c('age', 'race'),
                                                dimensions = c('age', 'sex', 'risk', 'race'),
                                                levels.of.stratification = c(0,1), # NOTE: IF WE HAVE ONLY 2 DIMENSIONS, 0 IS THE SAME AS 2! WE DON'T WANT TO PULL TWICE FOR THE SAME STRAT.
                                                omit.years = 2020,
                                                measurement.error.coefficient.of.variance = 0.5,
                                                weights = list(4,
                                                               c(3,5),
                                                               create.likelihood.weights(8, dimension.values = list(race=c('hispanic', 'black'), sex='male')),
                                                               create.likelihood.weights(1/2, dimension.values = list(risk=c('idu'))),
                                                               create.likelihood.weights(5, dimension.values = list(race='hispanic', age = c('13-24 years'))),
                                                               c(1/3)))
my.instr.ar1 = create.basic.likelihood.instructions(outcome.for.data = 'diagnosed.prevalence',
                                                outcome.for.sim = 'diagnosed.prevalence',
                                                denominator.outcome.for.sim = 'population',
                                                # dimensions = c('age', 'race'),
                                                dimensions = c('age', 'sex', 'risk', 'race'),
                                                levels.of.stratification = c(0,1), # NOTE: IF WE HAVE ONLY 2 DIMENSIONS, 0 IS THE SAME AS 2! WE DON'T WANT TO PULL TWICE FOR THE SAME STRAT.
                                                omit.years = 2020,
                                                measurement.error.coefficient.of.variance = 0.5,
                                                observation.correlation.form = 'autoregressive.1',
                                                weights = list(4,
                                                               c(3,5),
                                                               create.likelihood.weights(8, dimension.values = list(race=c('hispanic', 'black'), sex='male')),
                                                               create.likelihood.weights(1/2, dimension.values = list(risk=c('idu'))),
                                                               create.likelihood.weights(5, dimension.values = list(race='hispanic', age = c('13-24 years'))),
                                                               c(1/3)))

# my.instr = create.basic.likelihood.instructions(outcome.for.data = '')
register.likelihood.instructions(my.instr,
                                 'v1',
                                 description='basic likelihood that calibrates diagnoses')

instr = LIKELIHOOD.INSTRUCTIONS.MANAGER[['v1']]
register.likelihood.instructions(my.instr.ar1,
                                 'v2',
                                 description='ar1')
# 
# prev.instr = create.basic.likelihood.instructions(outcome.for.data = 'prevalence',
#                                                   outcome.for.sim = 'diagnosed.prevalence',
#                                                   denominator.outcome.for.sim = 'population',
#                                                   dimensions = c('age', 'race'),
#                                                   levels.of.stratification = c(0,1),
#                                                   measurement.error.coefficient.of.variance = 0.67,
#                                                   weight = list(1/16 * 1/4))
# 
# joint.instr = join.likelihood.instructions(my.instr, prev.instr)
# # duplicate.instr = join.likelihood.instructions(joint.instr, my.instr)
# 
like = instr$instantiate.likelihood(version = 'ehe',
                                    location = 'C.12580',
                                    throw.error.if.no.data = FALSE)
like.ar1 = my.instr.ar1$instantiate.likelihood(version = 'ehe',
                                               location='C.12580',
                                               throw.error.if.no.data = F)
# calculated.likelihood = like$compute(sim=make.dummy.sim(version='ehe',
#                                                         location='C.12580',
#                                                         from.year=2006,
#                                                         to.year=2022),
#                                      check.consistency = F,
#                                      log=T,
#                                      debug=T)
calculated.likelihood = like$compute(sim, check.consistency = F, log=T, debug=F)
ar1.lik.compute = like.ar1$compute(sim, check.consistency = F, log=T, debug=F)
# # profvis::profvis(joint.instr$instantiate.likelihood(version = 'ehe',
#                                                            # location = "MD",
#                                                            # data.manager = copied.surveillance.manager,
#                                                            # throw.error.if.no.data = FALSE))
# register.likelihood.instructions(joint.instr,
#                                  'joint',
#                                  description='joint likelihood built from two basic likelihoods')
# like = joint.instr$instantiate.likelihood(version = 'ehe',
#                                           location = "MD",
#                                           data.manager = surveillance.manager,
#                                           throw.error.if.no.data = FALSE)
# 
# # profvis::profvis(like$compute(sim=make.dummy.sim(version='ehe',
# #                    location='MD',
# #                    from.year=2006,
# #                    to.year=2023),
# # check.consistency = F,
# # log = T))
# 
# 
# calculated.joint.likelihood=like$compute(sim=make.dummy.sim(version='ehe',
#                                                             location='MD',
#                                                             from.year=2006,
#                                                             to.year=2023),
#                                          check.consistency = F,
#                                          log = T)
