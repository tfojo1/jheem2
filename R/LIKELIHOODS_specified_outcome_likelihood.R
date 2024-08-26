

create.specified.outcome.likelihood.instructions <- function(outcome.for.sim,
                                                           outcome.value, # an array or a scalar value. If an array, must have dimnames set
                                                           additional.dimension.values,
                                                           measurement.error.term,
                                                           measurement.error.type)
{
    
    
    outcome.value = expand.array(outcome.value,
                                 target.dim.names = c(dimnames(outcome.value), additional.dimension.values))
}



# dep.ratio = array(c(3,3.2,3.1),
#                   dim = c(race=3),
#                   dimnames = list(race=c('black','hispanic','white','other')))
# 
# dep.ratio.total.for.several.years.instr = create.specified.outcome.likelihood.instructions(outcome.for.sim = 'depression.prevalence.ratio',
#                                                                                            outcome.value = 3.1,
#                                                                                            additional.dimension.values = list(year=2010:2020,
#                                                                                                                               sex=c('male','female'))
# )