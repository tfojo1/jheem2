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
library(locations)

DATA.ROOT.DIR = '../../v1_data/'

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

# load('R/sysdata.rda')

# would use library(jheem2)

test.proportion.aggregation = function(browse=F) {
    
    error.prefix = "test.proportion.aggregation: "
    
    # ---Test whether rates of suppression can be aggregated across sex---
    
    # --Initialize data manager--
    
    data.manager = create.data.manager('test', description='a data manager to test with')
    data.manager$register.outcome(
        'suppression',
        metadata = create.outcome.metadata(
            scale = 'rate',
            display.name = 'Suppression',
            axis.name = 'Suppression (n)',
            units = 'rate',
            description = "Proportion of Diagnosed PWH Who Are Suppressed"),
        denominator.outcome = 'prevalence_diagnosed')
    data.manager$register.outcome(
        'prevalence_diagnosed',
        metadata = create.outcome.metadata(
            scale = 'non.negative.number',
            display.name = "Prevalence (Diagnosed)",
            axis.name = "People with Diagnosed HIV (n)",
            units = 'people',
            description = "Estimated Number of People with HIV aware of their Diagnosis"))
    
    data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')
    
    data.manager$register.ontology(
        'CDC_bho',
        ont=ontology(
            location=NULL,
            year=NULL,
            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
            race=c('black','hispanic','other'),
            sex=c('male','female'),
            risk=c('msm','idu','msm_idu','heterosexual','other')))
    
    # --Process data--
    
    data = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/sle_17.19_age.sex.csv/'))
    
    outcome.mappings = c('HIV viral suppression'='suppression')
    
    # This introduces NAs for the other outcomes, but we remove that data anyways
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'
    data$year = as.character(data$year)
    names(data)[names(data)=='Geography'] = 'location'
    data$location = STATE.MAPPINGS[data$location]
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
    data$age = paste0(data$Age.Group, " years")
    
    # Percentage will be one outcome
    data$Percent[data$Percent %in% c('Data not available', 'Data suppressed')] = NA
    data$value = as.numeric(data$Percent)
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'CDC_bho',
        source = 'cdc',
        dimension.values = list(),
        url = 'www.sle1719agesex.gov',
        details = 'CDC Reporting')
    
    # Prevalence will be another outcome which I'm *assuming* is represented by "population" in this dataset
    data$outcome = 'prevalence_diagnosed'
    data$value = as.numeric(gsub(",", '', data$Population))
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'CDC_bho',
        source = 'cdc',
        dimension.values = list(),
        url = 'www.sle1719agesex.gov',
        details = 'CDC Reporting')

    # --Call pull function--
    
    # This should require no aggregation
    pull.test.1 = data.manager$pull(
        outcome = 'suppression',
        keep.dimensions = c('location', 'year', 'age', 'sex'))
    
    # Aggregate over sex
    pull.test.2 = data.manager$pull(
        outcome = 'suppression',
        keep.dimensions = c('location', 'year', 'age'),
        dimension.values = list(sex=c('male', 'female'))
    )
    
    if (is.null(pull.test.1)) print(paste0(error.prefix, "pull.test.1 failed"))
    if (is.null(pull.test.2)) print(paste0(error.prefix, "pull.test.2 failed"))
    
    # specific values that should be present
    if (pull.test.1[['AK', '2018', '13-24 years', 'male', 1]] != 73.3)
        print(paste0(error.prefix, "pull.test.1 failed"))
    if (pull.test.2[['AK', '2018', '13-24 years', 1]] != 79.975)
        print(paste0(error.prefix, "pull.test.2 failed"))
    
    else print(paste0(error.prefix, "All tests passed"))
    
    if (browse) browser()
    
}

test.choosing.stratifications = function(browse=F) {
    
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
        'CDC_bho',
        ont=ontology(
            location=NULL,
            year=NULL,
            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
            race=c('black','hispanic','other'),
            sex=c('male','female'),
            risk=c('msm','idu','msm_idu','heterosexual','other')))
    
    # --Process data--
    
    data.list = list(
        data.1 = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_age.male.risk.csv/')),
        data.2 = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_male.risk.csv/'))
    )
    
    outcome.mappings = c('HIV diagnoses'='new')
    
    risk.mappings = c('Heterosexual contact' = 'heterosexual',
                      'Injection drug use' = 'idu',
                      'Other' = 'other',
                      'Male-to-male sexual contact' = 'msm',
                      'Male-to-male sexual contact and injection drug use' = 'msm_idu')
    
    data.list[['data.1']]['age'] = paste0(data.list[['data.1']][['Age.Group']], " years")

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
        
        # MIGHT NEED TO CONVERT SOME TO NA
        data$value = as.numeric(gsub(",", '', data$Cases))
        
        data.manager$put.long.form(
            data = data,
            ontology.name = 'CDC_bho',
            source = 'cdc',
            dimension.values = list(),
            url = 'www.example.gov',
            details = 'CDC Reporting')
        
    }
    
    
    # --Call pull function--
    
    if (browse) browser()
    
    # This should require no aggregation
    pull.test.1 = data.manager$pull(
        outcome = 'new',
        keep.dimensions = c('location', 'year', 'age', 'sex', 'risk'))
    
    # Aggregate over age
    pull.test.2 = data.manager$pull(
        outcome = 'new',
        keep.dimensions = c('location', 'year', 'sex', 'risk'),
        dimension.values = list(sex=c('male', 'female'))
    )
    
    
    if (is.null(pull.test.1)) print(paste0(error.prefix, "pull.test.1 failed"))
    if (is.null(pull.test.2)) print(paste0(error.prefix, "pull.test.2 failed"))
    
    # specific values that should be present
    if (pull.test.1[['AL', '2008', '13-24 years', 'male', 'idu', 1]] != 2)
        print(paste0(error.prefix, "pull.test.1 failed"))
    if (pull.test.2[['AL', '2008', 'male', 'idu', 1]] != 26)
        print(paste0(error.prefix, "pull.test.2 failed"))
    
    else print(paste0(error.prefix, "All tests passed"))
    
    
}

test.target.ontology = function(browse=F) {
    
    # @@@ INCOMPLETE
    
    error.prefix = "test.target.ontology: "
    
    # ---Test data will be mapped properly to a target ontology ---
    
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
        'CDC_bho',
        ont=ontology(
            location=NULL,
            year=NULL,
            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
            race=c('black','hispanic','other'),
            sex=c('male','female'),
            risk=c('msm','idu','msm_idu','heterosexual','other')))
    
    # --Process data--
    
    data.list = list(
        data.1 = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_age.male.risk.csv/')),
        data.2 = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_male.risk.csv/'))
    )
    
    outcome.mappings = c('HIV diagnoses'='new')
    
    risk.mappings = c('Heterosexual contact' = 'heterosexual',
                      'Injection drug use' = 'idu',
                      'Other' = 'other',
                      'Male-to-male sexual contact' = 'msm',
                      'Male-to-male sexual contact and injection drug use' = 'msm_idu')
    
    data.list[['data.1']]['age'] = paste0(data.list[['data.1']][['Age.Group']], " years")
    
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
        
        data$value = as.numeric(gsub(",", '', data$Cases))
        
        data.manager$put.long.form(
            data = data,
            ontology.name = 'CDC_bho',
            source = 'cdc',
            dimension.values = list(),
            url = 'www.example.gov',
            details = 'CDC Reporting')
        
    }
    
    
    # --Call pull function--
    
    if (browse) browser()
    
    # This should require no aggregation
    pull.test.1 = data.manager$pull(
        outcome = 'new',
        keep.dimensions = c('location', 'year', 'age', 'sex', 'risk'))
    
    # Aggregate over age
    pull.test.2 = data.manager$pull(
        outcome = 'new',
        keep.dimensions = c('location', 'year', 'sex', 'risk'),
        dimension.values = list(sex=c('male', 'female'))
    )
    
    
    if (is.null(pull.test.1)) print(paste0(error.prefix, "pull.test.1 failed"))
    if (is.null(pull.test.2)) print(paste0(error.prefix, "pull.test.2 failed"))
    
    # specific values that should be present
    if (pull.test.1[['AL', '2008', '13-24 years', 'male', 'idu', 1]] != 2)
        print(paste0(error.prefix, "pull.test.1 failed"))
    if (pull.test.2[['AL', '2008', 'male', 'idu', 1]] != 26)
        print(paste0(error.prefix, "pull.test.2 failed"))
    
    else print(paste0(error.prefix, "All tests passed"))
    
}

test.argument.validation = function(browse=F) {
    
    error.prefix = "test.argument.validation: "
    
    # ---Test validation of arguments---
    
    # --Create data manager same as in test.proportion.aggregation
    
    data.manager = create.data.manager('test', description='a data manager to test with')
    data.manager$register.outcome(
        'suppression',
        metadata = create.outcome.metadata(
            scale = 'rate',
            display.name = 'Suppression',
            axis.name = 'Suppression (n)',
            units = 'rate',
            description = "Proportion of Diagnosed PWH Who Are Suppressed"),
        denominator.outcome = 'prevalence_diagnosed')
    data.manager$register.outcome(
        'prevalence_diagnosed',
        metadata = create.outcome.metadata(
            scale = 'non.negative.number',
            display.name = "Prevalence (Diagnosed)",
            axis.name = "People with Diagnosed HIV (n)",
            units = 'people',
            description = "Estimated Number of People with HIV aware of their Diagnosis"))
    
    data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')
    
    data.manager$register.ontology(
        'CDC_bho',
        ont=ontology(
            location=NULL,
            year=NULL,
            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
            race=c('black','hispanic','other'),
            sex=c('male','female'),
            risk=c('msm','idu','msm_idu','heterosexual','other')))
    
    # --Process data--
    
    data = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/sle_17.19_age.sex.csv/'))
    
    outcome.mappings = c('HIV viral suppression'='suppression')
    
    # This introduces NAs for the other outcomes, but we remove that data anyways
    data$outcome <- outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'
    data$year = as.character(data$year)
    names(data)[names(data)=='Geography'] = 'location'
    data$location = STATE.MAPPINGS[data$location]
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
    data$age = paste0(data$Age.Group, " years")
    
    # Percentage will be one outcome
    data$Percent[data$Percent %in% c('Data not available', 'Data suppressed')] = NA
    data$value = as.numeric(data$Percent)
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'CDC_bho',
        source = 'cdc',
        dimension.values = list(),
        url = 'www.sle1719agesex.gov',
        details = 'CDC Reporting')
    
    # Prevalence will be another outcome which I'm *assuming* is represented by "population" in this dataset
    data$outcome = 'prevalence_diagnosed'
    data$value = as.numeric(gsub(",", '', data$Population))
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'CDC_bho',
        source = 'cdc',
        dimension.values = list(),
        url = 'www.sle1719agesex.gov',
        details = 'CDC Reporting')
    
    # --Tests--
    
    overall.pass = T
    
    # Tests that should fail
    tests.failed = 0
    tests.failed.set = c()
    num.failure.tests = 14
    
    sapply(1:num.failure.tests, function(test.index) {
        tryCatch(
            {
                switch(
                    test.index,
                    
                    # 1
                    test.outcome.fail.1 = data.manager$pull(
                        outcome = 'unregistered.outcome'
                    ),
                    
                    # 2
                    test.outcome.fail.2 = data.manager$pull(
                        outcome = c("not", "single")
                    ),
                    
                    # 3
                    test.outcome.fail.3 = data.manager$pull(
                        outcome = NA
                    ),
                    
                    # 4
                    test.outcome.fail.4 = data.manager$pull(
                        outcome = ''
                    ),
                    
                    # 5
                    test.keep.dimensions.fail.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        keep.dimensions = c(NA)
                    ),
                    
                    # 6
                    test.keep.dimensions.fail.2 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        keep.dimensions = c('age', 'age')
                    ),
                    
                    # 7
                    test.dimension.values.fail.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        dimension.values = list(non.existent = 'dimension')
                    ),
                    
                    # 8
                    test.sources.fail.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        sources = list()
                    ),
                    
                    # 9
                    test.sources.fail.2 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        sources = character()
                    ),
                    
                    # 10
                    test.sources.fail.3 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        sources = 'unregistered.source'
                    ),
                    
                    # 11
                    test.sources.fail.4 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        sources = c(NA)
                    ),
                    
                    # 12
                    test.target.ontology.fail.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        target.ontology = list(
                            not = c('a real', 'ontology')
                        )
                    ),
                    
                    # 13
                    test.target.ontology.fail.2 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        target.ontology = ontology(
                            location='MD',
                            year=as.character(2008:2020),
                            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
                            race=c('black','hispanic','other'),
                            sex=c('male','female'),
                            incomplete.dimensions = c('location','year')),
                        dimension.values = list(risk = c('msm', 'msm_idu'))
                    ),
                    
                    # 14
                    test.target.ontology.fail.3 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        target.ontology = ontology(
                            location='Maryland',
                            year=NULL,
                            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
                            race=c('black','hispanic','other'),
                            sex=c('male','female'),
                            incomplete.dimensions = c('location','year')),
                        keep.dimensions = c('risk')
                    )
                )
            },
            error = function(e) {
                tests.failed.set <<- union(tests.failed.set, test.index)
                tests.failed <<- tests.failed + 1
            }
        )
    })
    if (tests.failed != num.failure.tests) {
        print(paste0(error.prefix, (num.failure.tests - tests.failed), "/", num.failure.tests, " tests that should have failed passed."))
        print(paste0("The following tests failed: ", setdiff(1:num.failure.tests, tests.failed.set)))
        overall.pass = F
    }
        
    
    # tests that should pass
    tests.failed = 0
    num.success.tests = 6
    sapply(1:num.success.tests, function(test.index) {
        tryCatch(
            {
                switch(
                    test.index,
                    
                    # 1
                    test.outcome.pass.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed'
                    ),
                    
                    # 2
                    test.keep.dimensions.pass.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        keep.dimensions = c('age')
                    ),
                    
                    # 3
                    test.dimension.values.pass.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        dimension.values = list(age = '25-34 years')
                    ),
                    
                    # 4
                    test.sources.pass.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        sources = c('cdc')
                    ),
                    
                    # 5
                    test.sources.pass.2 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        sources = NULL
                    ),
                    
                    # 6
                    test.target.ontology.pass.1 = data.manager$pull(
                        outcome = 'prevalence_diagnosed',
                        target.ontology = ontology(
                            location='MD',
                            year=as.character(2008:2020),
                            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
                            race=c('black','hispanic','other'),
                            sex=c('male','female'),
                            risk=c('msm','idu','msm_idu','heterosexual','other'),
                            incomplete.dimensions = c('location','year'))
                    )
                )
            },
            error = function(e) {
                # print(paste0("generated error: ", e$message))
                tests.failed <<- test.failed + 1
            }
        )
    })
    if (tests.failed > 0) {
        print(paste0(error.prefix, tests.failed, "/", num.success.tests, " tests that should have passed failed."))
        overall.pass = F
    }
    
    if (overall.pass) print(paste0(error.prefix, "All tests successful"))
    
}

test.common.ontology = function(browse=F) {
    
    error.prefix = "test.common.ontology: "
    
    # ---Test using a common ontology---
    
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
        data.1 = read.csv(file.path(DATA.ROOT.DIR, 'hiv_surveillance/state/npm_08.19_race.male.risk.csv')),
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
    
    # --Register target.ontology--

    data.manager$register.ontology(
        'jheem',
        ont = ontology(
            location=locations,
            year=years,
            age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
            race=c('black','hispanic','other'),
            sex=c('msm', 'heterosexual_male', 'female'),
            risk=c('never_IDU', 'active_IDU', 'IDU_in_remission')
        )
    )

    # --Call pull function--

    pull.test.1 = data.manager$pull(
        outcome = 'new',
        keep.dimensions = c('sex', 'risk'),
        dimension.values = list(location='MD', year='2008'),
        target.ontology = data.manager$get.registered.ontology("jheem"),
        allow.mapping.from.target.ontology = T
    )
    pull.test.2 = data.manager$pull(
        outcome = 'new',
        keep.dimensions = c('sex', 'risk', 'race'),
        dimension.values = list(location='MD', year='2008', sex='msm', risk='IDU_in_remission'),
        target.ontology = data.manager$get.registered.ontology("jheem")
    )
    if (!is.null(pull.test.2)) {
        print("pull.test.2 failed to produce NULL")
    }
    if (browse) browser()
    
    ### Tests whether the patch for allowing multiple dimension values per dimension
    pull.test.3 = data.manager$pull(
        outcome = 'new',
        keep.dimensions = c('sex', 'risk'),
        dimension.values = list(location=c('MD', 'AZ'), year='2008'),
        target.ontology = data.manager$get.registered.ontology("jheem"),
        allow.mapping.from.target.ontology = T
    )
    if (is.null(pull.test.1) || !is.null(pull.test.2) || is.null(pull.test.3)) {
        print(paste0(error.prefix, "Some or all tests failed"))
    } else {
        print(paste0(error.prefix, "All tests passed"))
    }
    
}

# ----MAIN----
test.proportion.aggregation()
test.choosing.stratifications(browse=F)
test.argument.validation()
test.common.ontology(browse=F)

# jheem.ontology = ontology(
#     location='MD',
#     year=as.character(2008:2020),
#     age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
#     race=c('black','hispanic','other'),
#     sex=c('msm', 'heterosexual_male', 'female'),
#     risk=c('never_IDU', 'active_IDU', 'IDU_in_remission'))
# 
# cdc.ontology = ontology(
#     location='MD',
#     year=as.character(2008:2020),
#     age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
#     race=c('black','hispanic', 'white', 'AAPI', 'other'),
#     sex=c('male','female'),
#     risk=c('msm','idu','msm_idu','heterosexual','other'))

