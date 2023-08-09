
# Sourcing this file is an alternative to a call to 
#   library(jheem2)
# For testing

source('../jheem2/R/HELPERS_misc_helpers.R')
source('../jheem2/R/HELPERS_dim_names_helpers.R')
source('../jheem2/R/HELPERS_array_helpers.R')
source('../jheem2/R/HELPERS_age_race_helpers.R')
Rcpp::sourceCpp('../jheem2/src/array_helpers.cpp')

source('../jheem2/R/ONTOLOGY_ontology.R')
source('../jheem2/R/ONTOLOGY_ontology_mappings.R')

source('../jheem2/R/SPECIFICATION_scales.R')
source('../jheem2/R/SPECIFICATION_model_specification.R')
source('../jheem2/R/SPECIFICATION_compiled_specification.R')

source('../jheem2/R/SPECIFICATION_functional_forms.R')
source('../jheem2/R/SPECIFICATION_functional_form_alphas.R')
Rcpp::sourceCpp('../jheem2/src/functional_forms.cpp')
source('../jheem2/R/SPECIFICATION_links.R')
source('../jheem2/R/SPECIFICATION_evaluatable_value.R')

source('../jheem2/R/VERSIONS_version_manager.R')

source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
Rcpp::sourceCpp('../jheem2/src/ontology_mappings.cpp')

source('../jheem2/R/JHEEM_entity.R')
source('../jheem2/R/JHEEM_engine.R')
source('../jheem2/R/SPECIFICATION_metadata.R')

Rcpp::sourceCpp('../jheem2/src/misc_helpers.cpp')