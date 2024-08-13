source('R/LIKELIHOODS_basic_likelihood.R')
source('R/LIKELIHOODS_main.R')
source('R/JHEEM_simulation_metadata.R')

source('R/JHEEM_entity.R')

source('R/SPECIFICATION_scales.R')
source('R/HELPERS_misc_helpers.R')
source('R/DATA_MANAGER_data_manager.R')
source('R/ONTOLOGY_ontology.R')
source('R/ONTOLOGY_ontology_mappings.R')
source('R/HELPERS_array_helpers.R')
source('R/HELPERS_dim_names_helpers.R')
source('R/SPECIFICATION_model_specification.R') #has the outcome.metadata object definition
Rcpp::sourceCpp('src/array_helpers.cpp')
Rcpp::sourceCpp('src/ontology_mappings.cpp')
Rcpp::sourceCpp('src/likelihood_helpers.cpp')
library(locations)

prepare.data.manager = function(data.manager) {
    
    DATA.ROOT.DIR = '../../v1_data/'
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
        'cdc',
        ont = ontology(
            location=NULL,
            year=NULL,
            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
            race=c('black','hispanic', 'white', 'AAPI', 'other'),
            sex=c('male','female'),
            risk=c('msm','idu','msm_idu','heterosexual','other'))
    )
    
    # The JHEEM ontology, which will be used as a target.ontology, will be defined below
    # so that we can set its NULL dimensions to be whatever the data has.
    # Normally it these dimensions would be set when data is put with the ontology.
    
    register.ontology.mapping(
        'lump.other.risk.and.heterosexual',
        from.dimensions = 'risk',
        to.dimensions = 'risk',
        mappings = rbind(
            c('msm', 'msm'),
            c('idu', 'idu'),
            c('msm_idu', 'msm_idu'),
            c('heterosexual', 'heterosexual'),
            c('other', 'heterosexual')
        )
    )
    
    register.ontology.mapping(
        'jheem.to.cdc.sex.risk',
        from.dimensions = c('sex', 'risk'),
        to.dimensions = c('sex', 'risk'),
        mappings = rbind(
            c('msm', 'never_IDU', 'male', 'msm'),
            c('msm', 'active_IDU', 'male', 'msm_idu'),
            c('msm', 'IDU_in_remission', 'male', 'msm_idu'),
            c('heterosexual_male', 'never_IDU', 'male', 'heterosexual'),
            c('heterosexual_male', 'active_IDU', 'male', 'idu'),
            c('heterosexual_male', 'IDU_in_remission', 'male', 'idu'),
            c('female', 'never_IDU', 'female', 'heterosexual'),
            c('female', 'active_IDU', 'female', 'idu'),
            c('female', 'IDU_in_remission', 'female', 'idu')
        )
    )
    
    # --Process data--
    
    data.list = list(
        data.1 = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_race.male.risk.csv/')),
        data.2 = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_male.risk.csv'))
    )
    
    outcome.mappings = c('HIV diagnoses'='new')
    
    risk.mappings = c('Heterosexual contact' = 'heterosexual',
                      'Injection drug use' = 'idu',
                      'Other' = 'other',
                      'Male-to-male sexual contact' = 'msm',
                      'Male-to-male sexual contact and injection drug use' = 'msm_idu')
    
    race.mappings = c('American Indian/Alaska Native' = 'AAPI',
                      'Asian' = 'AAPI',
                      'Black/African American' = 'black',
                      'Hispanic/Latino' = 'hispanic',
                      'Native Hawaiian/Other Pacific Islander' = 'AAPI',
                      'White' = 'white',
                      'Multiple races' = 'other'
    )
    
    STATE.MAPPINGS = c(
        'Alabama' = 'AL',
        'Alaska' = 'AK',
        'Arizona' = 'AZ',
        'Arkansas' = 'AR',
        'California' = 'CA',
        'Colorado' = 'CO',
        'Connecticut' = 'CT',
        'Delaware' = 'DE',
        'District of Columbia' = 'DC',
        'Florida' = 'FL',
        'Georgia' = 'GA',
        'Hawaii' = 'HI',
        'Idaho' = 'ID',
        'Illinois' = 'IL',
        'Indiana' = 'IN',
        'Iowa' = 'IA',
        'Kansas' = 'KS',
        'Kentucky' = 'KY',
        'Louisiana' = 'LA',
        'Maine' = 'ME',
        'Maryland' = 'MD',
        'Massachusetts' = 'MA',
        'Michigan' = 'MI',
        'Minnesota' = 'MN',
        'Mississippi' = 'MS',
        'Missouri' = 'MO',
        'Montana' = 'MT',
        'Nebraska' = 'NE',
        'Nevada' = 'NV',
        'New Hampshire' = 'NH',
        'New Jersey' = 'NJ',
        'New Mexico' = 'NM',
        'New York' = 'NY',
        'North Carolina' = 'NC',
        'North Dakota' = 'ND',
        'Oklahoma' = 'OK',
        'Ohio' = 'OH', #
        'Oregon' = 'OR',
        'Pennsylvania' = 'PA',
        'Rhode Island' = 'RI',
        'South Carolina' = 'SC',
        'South Dakota' = 'SD',
        'Tennessee' = 'TN',
        'Texas' = 'TX',
        'Utah' = 'UT',
        'Vermont' = 'VT',
        'Virginia' = 'VA',
        'Washington' = 'WA',
        'West Virginia' = 'WV',
        'Wisconsin' = 'WI',
        'Wyoming' = 'WY'
    )
    
    # record possible values for the incomplete dimensions, year and location
    locations = c()
    years = c()
    
    for (data in data.list) {
        
        # This introduces NAs for the other outcomes, but we remove that data anyways
        data$outcome = outcome.mappings[data$Indicator]
        data = data[!is.na(data$outcome),]
        
        names(data)[names(data)=='Year'] = 'year'
        data$year = as.character(data$year)
        names(data)[names(data)=='Geography'] = 'location'
        data$location = STATE.MAPPINGS[data$location]
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
        data$risk = risk.mappings[data$Transmission.Category]
        if (length(unique(data$race)) > 1)
            data$race = race.mappings[data[['Race.Ethnicity']]]
        
        # MIGHT NEED TO CONVERT SOME TO NA
        data$Cases[data$Cases %in% c("Data suppressed")] = NA
        data$value = as.numeric(gsub(",", '', data$Cases))
        
        data.manager$put.long.form(
            data = data,
            ontology.name = 'cdc',
            source = 'cdc',
            dimension.values = list(sex='male'),
            url = 'www.example.gov',
            details = 'CDC Reporting')
        
        # record years and locations
        locations = union(locations, unlist(unique(data['location'])))
        years = union(years, unlist(unique(data['year'])))
        
    }
    
    data.manager$register.ontology(
        'jheem',
        ont = ontology(
            location=locations,
            year=years,
            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
            race=c('black','hispanic','other'),
            sex=c('msm', 'heterosexual_male', 'female'),
            risk=c('never_IDU', 'active_IDU', 'IDU_in_remission'),
            incomplete.dimensions = c('location', 'year')
        )
    )
}

my.data.manager = create.data.manager('test', description='a data manager to test with')
prepare.data.manager(my.data.manager)

copied.data.manager = copy(my.data.manager, 'copied data manager', 'A copy of my.data.manager')

# register.basic.likelihood.instructions(
#     code = 'a-b.1',
#     description = 'my first instructions',
#     dimensions = c('age', 'sex', 'risk', 'race'),
#     outcome.for.data = 'new',
#     outcome.for.sim = 'new',
#     years = NULL,
#     correlation.different.years = 0.5,
#     correlation.different.strata = 0.1,
#     correlation.different.sources = 0.3,
#     correlation.same.source.different.details = 0.3,
#     measurement.error.coefficient.of.variance = 0.5,
#     weights = list()
# )
# 
# instr = LIKELIHOOD.INSTRUCTIONS.MANAGER[['a-b.1']]
# like = instr$instantiate.likelihood(
#     version = '2.0',
#     location = 'MD',
#     data.manager = my.data.manager
# )
# compute currently is benchmarking R version vs. fast
