# May require sourcing some other ones...

source('R/tests/make_dummy_sim.R')
source('R/JHEEM_simulation.R')
source('R/VERSIONS_version_manager.R')

source('../jheem_analyses/applications/EHE/ehe_specification.R')

sim = make.dummy.sim(version='ehe',
                     location='MD',
                     from.year=1970,
                     to.year=2025)
sim.numerator.data = sim$get(outcome='new',
                             keep.dimensions = c('year', 'age', 'sex', 'race', 'risk'))


# Error in array.access(private$i.data[[outcome]], dimension.values) :
#     Error in array.access(): Elements of ... which are lists must be NAMED


sim.denominator.data = sim$get(outcome='population',
                               keep.dimensions = c('year', 'sex'))
# Error getting simulation results: For the 'population' outcome, 'dimension 'year' must have dimension.values specified (it is NULL in the ontology and must be specified in the get() call)

sim.denominator.data = sim$get(outcome='population',
                               keep.dimensions = c('year', 'sex'),
                               dimension.values = list(year=as.character(2008:2021)))
# Invalid values fir dimension 'year' in 'dimension.values': '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021'
# Problem traces to check.array.access.arguments(arr = arr, dimension.values = dimension.values)