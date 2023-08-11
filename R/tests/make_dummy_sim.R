

make.dummy.sim <- function(version='ehe',
                           location='c.12580',
                           from.year=1970,
                           to.year=2020)
{
    metadata = get.simulation.metadata(version = version,
                                       location = location,
                                       from.year = from.year,
                                       to.year = to.year)
    
    outcome.data = lapply(metadata$outcomes, function(outcome){
        ont = metadata$outcome.ontologies[[outcome]]
        
        array(1:prod(sapply(ont, length)),
              dim=sapply(ont, length),
              dimnames=ont)
    })
    names(outcome.data) = metadata$outcomes
    
    JHEEM.SIMULATION$new(version = version,
                         location = location,
                         from.year = from.year,
                         to.year = to.year,
                         outcome.data = outcome.data)
}