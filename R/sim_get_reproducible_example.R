# May require sourcing some other ones...

source('R/tests/make_dummy_sim.R')
source('R/JHEEM_simulation.R')
source('R/VERSIONS_version_manager.R')

source('../jheem_analyses/applications/EHE/ehe_specification.R')

sim = make.dummy.sim(version='ehe',
                     location='MD',
                     from.year=2009,
                     to.year=2013)
sim.numerator.data = sim$get(outcome='new',
                             keep.dimensions = c('year', 'age', 'sex', 'race', 'risk'))


# Error in array.access(private$i.data[[outcome]], dimension.values) :
#     Error in array.access(): Elements of ... which are lists must be NAMED