
source('R/SPECIFICATION_scales.R')
source('R/HELPERS_misc_helpers.R')
source('R/DATA_MANAGER_data_manager.R')
source('R/ONTOLOGY_ontology.R')
source('R/HELPERS_array_helpers.R')
source('R/HELPERS_dim_names_helpers.R')
source('R/SPECIFICATION_model_specification.R') #has the outcome.metadata object definition
Rcpp::sourceCpp('src/array_helpers.cpp')

DATA.ROOT.DIR = '../../../Ending HIV/Ending_HIV/cleaned_data/'

data = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_age.female.risk.csv/'))



data.manager = create.data.manager('test', description='a data manager to test with')
data.manager$register.outcome('new', 
                              metadata = create.outcome.metadata(scale = 'non.negative.number',
                                                                 display.name = 'New Diagnoses',
                                                                 axis.name = 'New Diagnoses (n)',
                                                                 units = 'cases',
                                                                 description = "New HIV Cases Diagnosed in a Year"))
data.manager$register.outcome('prevalence_diagnosed',
                              metadata = create.outcome.metadata(scale = 'non.negative.number',
                                                                 display.name = "Prevalence (Diagnosed)",
                                                                 axis.name = "People with Diagnosed HIV (n)",
                                                                 units = 'people',
                                                                 description = "Estimated Number of People with HIV aware of their Diagnosis"))
data.manager$register.outcome('mortality', 
                              metadata = create.outcome.metadata(scale = 'non.negative.number',
                                                                 display.name = 'Mortality',
                                                                 axis.name = 'Deaths (n)',
                                                                 units = 'deaths',
                                                                 description = "Number of Deaths among People with Diagnosed HIV"))

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

data.manager$register.ontology('CDC_bho',
                               ont=ontology(location=NULL,
                                            year=NULL,
                                            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
                                            race=c('black','hispanic','other'),
                                            sex=c('male','female'),
                                            risk=c('msm','idu','msm_idu','heterosexual','other')))

outcome.mappings = c('HIV deaths'='mortality',
                     'HIV diagnoses'='new',
                     'HIV prevalence'='prevalence_diagnosed')

risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other')

data$outcome = outcome.mappings[data$Indicator]
names(data)[names(data)=='Year'] = 'year'
data$year = as.character(data$year)
names(data)[names(data)=='Geography'] = 'location'
data$age = paste0(data$Age.Group, " years")
data$risk = risk.mappings[data$Transmission.Category]
data$Cases[data$Cases=='Data suppressed'] = NA
data$value = as.numeric(gsub(",", '', data$Cases))

data.manager$put.long.form(data = data,
                           ontology.name = 'CDC_bho',
                           source = 'cdc',
                           dimension.values = list(sex='female'),
                           url = 'www.example_url.gov',
                           details = 'CDC Reporting')

