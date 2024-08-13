

make.dummy.sim <- function(version='ehe',
                           location='c.12580',
                           from.year=1970,
                           to.year=2020)
{
    metadata = get.simulation.metadata(version = version,
                                       location = location,
                                       from.year = from.year,
                                       to.year = to.year)
    
    outcome.numerators = lapply(metadata$outcomes, function(outcome){
        ont = metadata$outcome.ontologies[[outcome]]
        
        array(1:prod(sapply(ont, length)),
              dim=sapply(ont, length),
              dimnames=ont)
    })
    names(outcome.numerators) = metadata$outcomes
    
    outcome.denominators = lapply(metadata$outcomes, function(outcome){
        if (metadata$outcome.metadata[[outcome]]$scale=='non.negative.number')
            NULL
        else
            outcome.numerators[[outcome]]*1.5
    })
    names(outcome.denominators) = metadata$outcomes
    
    JHEEM.SIMULATION$new(version = version,
                         location = location,
                         from.year = from.year,
                         to.year = to.year,
                         outcome.numerators = outcome.numerators,
                         outcome.denominators = outcome.denominators,
                         parameters = NULL)
}
