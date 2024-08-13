source('R/SPECIFICATION_scales.R')
source('R/HELPERS_misc_helpers.R')
source('R/DATA_MANAGER_data_manager.R')
source('R/ONTOLOGY_ontology.R')
source('R/ONTOLOGY_ontology_mappings.R')
source('R/HELPERS_array_helpers.R')
source('R/HELPERS_dim_names_helpers.R')
source('R/SPECIFICATION_model_specification.R') #has the outcome.metadata object definition
# source('R/LOCATIONS_location_manager.R')
Rcpp::sourceCpp('src/array_helpers.cpp')
Rcpp::sourceCpp('src/ontology_mappings.cpp')

error.prefix = "test.choosing.stratifications: "

# ---Test whether the correct stratification will be chosen among many---

# --Initialize data manager--

data.manager = create.data.manager('test', description='a data manager to test with')
data.manager$register.outcome(
    'new',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'New Diagnoses',
        axis.name = 'New Diagnoses (n)',
        units = 'cases',
        description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

data.manager$register.ontology(
    'CDC_with_races',
    ont=ontology(
        location=NULL,
        year=NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('black','hispanic','other'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')))

data = array(
    1,
    dim=c(location=4, year=2, age=5, race=3, sex=2, risk=5),
    dimnames=list(
        location=c('AZ', 'AL', 'SC', 'NorC'),
        year=c('2008', '2009'),
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('black','hispanic','other'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    )
)

put.data(
    data.manager,
    data = data,outcome = 'new',
    source = 'cdc',
    ontology.name = 'CDC_with_races',
    dimension.values = list(location=c('AZ', 'NorC')),
    url = 'www.cdc.gov',
    details = 'CDC reporting'
)
