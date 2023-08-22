


obs.vector
metadata = data.frame(
    year,
    stratum,
    source,
    details
)
transformation.matrix

sim.ontology = sim.metadata$outcome.ontologies[outcome.for.simulation]
for (strat in stratifications)
{
    data = pull(...,
                target.ontology = sim.ontology,
                allow.mapping.to.target.ontology = T)
    mapping = attr(data, 'mapping')
    
    one.obs.vector = as.numeric(data)
    one.metadata
    one.transformation.matrix = mapping$get.matrix(from.dim.names=sim.ontology, to.dim.names=dimnames(data))
    
    
    remove.mask = is.na(one.obs.vector)
    one.obs.vector = one.obs.vector[!remove.mask]
    one.metadata = one.metadata[!remove.mask,]
    one.transformation.matrix = one.transformation.matrix[!remove.mask,]
    
    obs.vector = c(obs.vector, one.obs.vector)
    metadata = rbind(metadata, one.metadata)
    transformation.matrix = rbind(transformation.matrix, one.transformation.matrix)
}

# n observations
year #vector of length n
stratum #vector of length n
source #vector of length n
details #vector of length n


cor.mat = sapply(1:n, function(i){
    sapply(1:n, function(j){
        
        correlation.different.years ^ (year[i] != year[j]) *
            correlation.different.strata ^ (stratum[i] != stratum[j]) *
            correlation.different.source ^ (source[i] != source[j]) *
            correlation.same.source.different.details ^ (source[i] == source[j] && details[i] != details[j])
        
    })
})