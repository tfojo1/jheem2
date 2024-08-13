# test = R6::R6Class(
#     'test',
#     public = list(
#         initialize = function(){
#             private$i.x = 1
#         }
#     ),
#     active = list(
#         x = function(value) {
#             if (missing(value))
#                 private$i.x
#             else
#                 private$i.x = value
#         }
#     ),
#     private = list(
#         i.x = NULL
#     )
# )
# 
# t1 = test$new()
# t1$x = list()
# t1$x + 2

source('R/LIKELIHOODS_basic_likelihood.R')
source('R/LIKELIHOODS_main.R')
source('R/JHEEM_simulation_metadata.R')
source('R/JHEEM_simulation.R')
source('R/JHEEM_entity.R')
source('R/VERSIONS_version_manager.R')

my.sim = JHEEM.SIMULATION$new()
sim.pop.data = my.sim$get(
    outcomes = 'population',
    years = 2010:2020,
    keep.dimensions = c('age', 'sex', 'race'),
    dimension.values = list()) # dimension.values doesn't do anything currently
sim.new.data = my.sim$get(
    outcomes = 'new',
    years = 2010:2020,
    keep.dimensions = c('age', 'sex', 'race'),
    dimension.values = list())
metadata = get.simulation.metadata('2.0', 'Baltimore')
sim.ontology = metadata$get.ontology(
    outcomes = c('population', 'new'),
    years = 2010:2020,
    keep.dimensions = c('age', 'sex', 'race'),
    dimension.values = list())

my.likelihood.instructions = register.basic.likelihood.instructions(
    outcome.for.data = 'new',
    outcome.for.sim = 'new',
    years = NULL,
    # stratifications
    correlation.different.years = 0.5,
    correlation.different.stratifications = 0.1,
    correlation.different.sources = 0.2,
    correlation.same.source.different.details = 0.3,
    # observation.correlation.form
    measurement.error.coefficient.of.variance = 0.1, # a wild guess
    weights = list() # hopefully won't matter for now?
)

# This likelihood instance should check the metadata
my.likelihood.instance = instantiate.likelihood(
    instructions.code = '',
    version = '',
    location = '')

# practice making a matrix out of the stratifications list?
stratifications=list(character(),
                     'age','sex','risk','race',
                     c('age','sex'), c('race','sex'), c('risk','sex'), c('risk','race'))
years = as.character(2010:2020)
sim.ontology.dimnames = metadata$get.ontology(
    outcomes = 'population', # does it matter?
    years = 2010:2030,
    keep.dimensions = c('age', 'sex', 'risk', 'race'),
    dimension.values = list())
# the matrix rownames should be the product of year and the combinations in the list elements
# total2010, total2011, age1.2010, age2.2010, sex1.2010, sex2.2011, age1.sex1.2010, age1.sex1.2011, etc.
# the number of rows is calculated as N years * [ 1 (for character()) + prod(sapply(element, length(sim.ontology.dimnames[element]))) ]
num.matrix.rows = length(years) * sum(sapply(stratifications, function(strat) {
    if (length(strat) > 0) {
        prod(sapply(strat, function(dimension) {
            length(sim.ontology.dimnames[[dimension]])
        }))
    }
    else
        1
    
}))

correlation.matrix = diag(1, nrow=num.matrix.rows, ncol=num.matrix.rows)
# this will of course need to be filled in

# now let me make the obs.vector, which will be the same length as the matrix. This will be for "new"
# obs.new.vector = unlist(lapply(stratifications, function(strat) {
#     # return one value for each combination of dimensions unless the strat has length 0
#     rv = NULL
#     combinations = NULL
#     if (length(strat) > 0) {
#         combinations = 1
#     }
#     1
# }))

# for c('sex', 'race') we'd have 99 combinations. For a given combination, our pull query would look like:
# data.manager$pull('new', ontology='CDC' although this is jheem, dimension.values = list(sex='sex1', race='race1', year='year'), target.ontology='jheem')

obs.new.vector = rep(2, num.matrix.rows)
obs.pop.vector = rep(12, num.matrix.rows)

# we'll also have a sim.new.vector and a sim.pop.vector
# between these will be a mapping
# our stratifications will be in the jheem ontology. The data will be given back to us in CDC but
# we'll have a mapping that comes along with each pull.
# so a pull for jheem's male 2010 will come back as 

# example mapping matrix M
M = rbind(
    c(rep(0,7), 1, 1),
    c(rep(0,9)),
    c(rep(0,6), 1, rep(0,2)),
    c(rep(0,9)),
    c(0, 1, 1, rep(0,6)),
    c(rep(0,4), 1, 1, rep(0,3)),
    c(1, rep(0,8)),
    c(rep(0,3), 1, rep(0,5))
)
z = rep(1,9)
Tau = rbind(
    c(1, rep(0.5, 8)),
    c(0, 1, rep(0.5, 7)),
    c(rep(0,2), 1, rep(0.5, 6)),
    c(rep(0,3), 1, rep(0.5, 5)),
    c(rep(0,4), 1, rep(0.5, 4)),
    c(rep(0,5), 1, rep(0.5, 3)),
    c(rep(0,6), 1, rep(0.5, 2)),
    c(rep(0,7), 1, 0.5),
    c(rep(0,8), 1)
)
# Mapping our covariance matrix
M %*% Tau %*% t(M)
