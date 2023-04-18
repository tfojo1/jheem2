
source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
source('../jheem2/R/ONTOLOGY_ontology.R')
Rcpp::sourceCpp("../jheem2/src/ontology_mappings.cpp")

map = create.ontology.mapping(
    mappings = rbind(
        c("black", "hispanic"),
        c("hispanic", "black"),
        c("other", "other")
    ),
    
    from.dimensions = 'race',
    to.dimensions = 'race'
)
