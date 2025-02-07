
source('R/tests/source_jheem2_package.R')
source('../jheem_analyses/applications/SHIELD/shield_ontology_mappings.R')

brfss = ontology(age=c("18-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65+ years"))
shield = ontology(age=c("18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-64 years", "65+ years"))
aligning = ontology(age=c("18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65+ years"))
get.mappings.to.align.ontologies(brfss, shield) # NULL
get.mappings.to.align.ontologies(brfss, aligning) # works
get.mappings.to.align.ontologies(shield, aligning) # works

examine.recent.failed.ontology.mapping()


# Could also do
examine.get.mappings.to.align.ontologies(brfss, shield)
