source('R/LIKELIHOODS_basic_likelihood.R')
source('R/LIKELIHOODS_main.R')

source('R/JHEEM_entity.R')

source('R/SPECIFICATION_scales.R')
source('R/HELPERS_misc_helpers.R')
source('R/DATA_MANAGER_data_manager.R')
source('R/ONTOLOGY_ontology.R')
source('R/ONTOLOGY_ontology_mappings.R')
source('R/HELPERS_array_helpers.R')
source('R/HELPERS_dim_names_helpers.R')
source('R/SPECIFICATION_model_specification.R') #has the outcome.metadata object definition
Rcpp::sourceCpp('src/array_helpers.cpp')
Rcpp::sourceCpp('src/ontology_mappings.cpp')
Rcpp::sourceCpp('src/likelihood_helpers.cpp')
library(locations)

source('R/tests/make_dummy_sim.R')

source('../jheem_analyses/applications/EHE/ehe_specification.R')

# surveillance.manager
# Takes about 20 seconds
copied.surveillance.manager = load.data.manager('../jheem_analyses/cached/surveillance.manager_080923.rdata')

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

my.instr = create.basic.likelihood.instructions(outcome.for.data = 'diagnoses',
                                                outcome.for.sim = 'new',
                                                denominator.outcome.for.sim = 'population',
                                                dimensions = c('age', 'race'),
                                                # dimensions = c('age', 'sex', 'risk', 'race'),
                                                levels.of.stratification = c(0,1), # NOTE: IF WE HAVE ONLY 2 DIMENSIONS, 0 IS THE SAME AS 2! WE DON'T WANT TO PULL TWICE FOR THE SAME STRAT.
                                                measurement.error.coefficient.of.variance = 0.5,
                                                weights = list(4,
                                                               c(3,5),
                                                               create.likelihood.weights(8, dimension.values = list(race=c('hispanic', 'black'), sex='male')),
                                                               create.likelihood.weights(1/2, dimension.values = list(risk=c('idu'))),
                                                               create.likelihood.weights(5, dimension.values = list(race='hispanic', age = c('13-24 years'))),
                                                               c(1/3)))
register.likelihood.instructions(my.instr,
                                 'v1',
                                 description='basic likelihood that calibrates diagnoses')

instr = LIKELIHOOD.INSTRUCTIONS.MANAGER[['v1']]

prev.instr = create.basic.likelihood.instructions(outcome.for.data = 'prevalence',
                                                  outcome.for.sim = 'diagnosed.prevalence',
                                                  denominator.outcome.for.sim = 'population',
                                                  dimensions = c('age', 'race'),
                                                  levels.of.stratification = c(0,1),
                                                  measurement.error.coefficient.of.variance = 0.67,
                                                  weight = list(1/16 * 1/4))

joint.instr = join.likelihood.instructions(my.instr, prev.instr)

like = instr$instantiate.likelihood(version = 'ehe',
                                    location = 'MD',
                                    data.manager = copied.surveillance.manager,
                                    throw.error.if.no.data = FALSE)

like$compute(sim=make.dummy.sim(version='ehe',
                                location='MD',
                                from.year=2009,
                                to.year=2013),
             check.consistency = F,
             log = T)


