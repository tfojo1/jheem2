data.manager = create.data.manager('surveillance', description='surveillance data manager')

data.manager$register.outcome(
    'adult.population',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Adult Population',
        axis.name = 'Adult Population',
        units = 'population',
        description = "Adult Population Estimate, Ages 13 and over"))

data.manager$register.outcome(
    'diagnoses',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'New Diagnoses',
        axis.name = 'New Diagnoses (n)',
        units = 'cases',
        description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.outcome(
    'hiv.deaths',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'HIV Deaths',
        axis.name = 'HIV Deaths (n)',
        units = 'cases',
        description = "HIV Deaths"))

data.manager$register.outcome(
    'diagnosed.prevalence', #Changing this from prevalence to diagnosed.prevalence bc CDC's prevalence only includes people who know their status#
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Diganosed Prevalence',
        axis.name = 'Diganosed Prevalence (n)',
        units = 'cases',
        description = "Diagnosed HIV Prevalence"))

data.manager$register.outcome(
    'linkage_1mo',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Linkage (1 month)',
        axis.name = 'Proportion Linked (1 month)',
        units = '%',
        description = "Linkage to HIV care within 1 Month"), denominator.outcome = 'diagnoses')

data.manager$register.outcome(
    'engagement', #changed from receipt to engagement
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Engagement in Care',
        axis.name = 'Proportion of Engaged in Care',
        units = '%',
        description = "Engagement in  HIV medical care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
    'suppression', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Viral Suppression',
        axis.name = 'Proportion Virally Suppressed',
        units = '%',
        description = "HIV Viral Suppression"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(  #Adding this as a denominator value for awareness/knowledge of status- bc the cdc denominator is an estimated prevalence value of both known and unknown status#
    'total.prevalence',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Total Prevalence',
        axis.name = 'Total Prevalence',
        units = 'cases',
        description = "Estimated Prevalence of Known and Unknown Status"))

data.manager$register.outcome(
    'awareness', #changed from knowledge to awareness
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Awareness of HIV Status',
        axis.name = 'Proportion Aware of HIV Status',
        units = '%',
        description = "Awareness of HIV Status"), denominator.outcome = 'total.prevalence') #creating outcome that is pop values for awarness#

data.manager$register.outcome(
    'prep', 
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Prescribed PrEP',
        axis.name = 'Number Prescribed PrEP',
        units = 'cases',
        description = "Number Prescribed PrEP"))

data.manager$register.outcome(
    'prep.indications', 
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'PrEP Indications',
        axis.name = 'Number with PrEP Indications',
        units = 'cases',
        description = "Estimated Number of Persons with PrEP Indications"))

data.manager$register.outcome(
    'aids.diagnosed.prevalence', #changed from aids.diagnosis to aids.diagnosed.prevalence
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'AIDS Diagnosed Prevalence',
        axis.name = 'AIDS Diagnosed Prevalence',
        units = 'cases',
        description = "AIDS Diagnosed Prevalence"))

data.manager$register.outcome(
    'aids.diagnoses',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'AIDS Diagnoses',
        axis.name = 'AIDS Diagnoses',
        units = 'cases',
        description = "AIDS Diagnoses"))

data.manager$register.outcome(
    'gonorrhea',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Gonorrhea',
        axis.name = 'Gonorrhea',
        units = 'cases',
        description = "Gonorrhea"))

data.manager$register.outcome(
    'ps.syphilis',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Primary and Secondary Syphilis',
        axis.name = 'Primary and Secondary Syphilis',
        units = 'cases',
        description = "Primary and Secondary Syphilis"))

data.manager$register.outcome(
    'early.syphilis',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Early, non-primary, non-Secondary Syphilis',
        axis.name = 'Early, non-primary, non-Secondary Syphilis',
        units = 'cases',
        description = "Early, non-primary, non-Secondary Syphilis"))

data.manager$register.outcome(
    'congenital.syphilis',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Congenital Syphilis',
        axis.name = 'Congenital Syphilis',
        units = 'cases',
        description = "Congenital Syphilis"))

data.manager$register.outcome(
    'heroin', #can change to heroin use but leave display name the same#
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Heroin Use in the Past Year',
        axis.name = 'Heroin Use in the Past Year (n)',
        units = 'cases',
        description = "Heroin Use in the Past Year"))

data.manager$register.outcome(
    'cocaine', 
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Cocaine use in the Past Year',
        axis.name = 'Cocaine use in the Past Year (n)',
        units = 'cases',
        description = "Cocaine Use in the Past Year"))

data.manager$register.outcome(
    'hiv.tests',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'HIV Tests',
        axis.name = 'HIV Tests (n)',
        units = 'cases',
        description = "Count of CDC Funded HIV Tests"))

data.manager$register.outcome(
    'hiv.test.positivity', #This was newly.diagnosed.positives, changing it to hiv.test.positivity put this in as a percentage not a count#
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'HIV Test Positivity',
        axis.name = 'HIV Test Positivity',
        units = '%',
        description = "HIV Test Positivity"), denominator.outcome = 'hiv.tests')

data.manager$register.outcome(
    'linkage_3mo',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Linkage (3 months)',
        axis.name = 'Proportion Linked (3 months)',
        units = '%',
        description = "Linkage to HIV care within 3 Months"), denominator.outcome = 'diagnoses')

data.manager$register.outcome(
    'retention',  #Defined as:Individuals with ≥2 tests (CD4 or VL) ≥3 months apart#
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Retention',
        axis.name = 'Proportion Retained in Care',
        units = '%',
        description = "Retention in Care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
    'retention.of.engaged', #Defined as >=2 tests (CD4 or VL) divided by >=1 test
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Retention of Engaged',
        axis.name = 'Retention of Engaged',
        units = '%',
        description = "Retention of Engaged in Care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
    'proportion.tested.n',           #Will have option in code to make this only people at risk or everyone#
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Denominator value for proportion tested in past year',
        axis.name = 'Denominator value for proportion tested in past year',
        units = '%',
        description = "Denominator value for proportion tested in past year"))

data.manager$register.outcome(
    'proportion.tested', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion Tested in Past Year',
        axis.name = 'Proportion Tested in Past Year',
        units = '%',
        description = "Proportion of People who have received an HIV test in the last year"), denominator.outcome = 'proportion.tested.n')

data.manager$register.outcome(
    'proportion.msm', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion of MSM',
        axis.name = 'Proportion of MSM',
        units = '%',
        description = "Proportion of Men who have sex with Men"), denominator.outcome = 'adult.population')

data.manager$register.outcome(
    'unweighted.denominator',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'unweighted.denominator',
        axis.name = 'unweighted.denominator)',
        units = 'cases',
        description = "BRFSS Unweighted Denominator Value"))

data.manager$register.outcome(
    'immigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Immigration',
        axis.name = 'Immigration',
        units = 'population',
        description = "Metro Immigration"))

data.manager$register.outcome(
    'emigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Emigration',
        axis.name = 'Emigration',
        units = 'population',
        description = "Metro Emigration"))

data.manager$register.outcome(
    'adult.immigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Adult Immigration',
        axis.name = 'Adult Immigration',
        units = 'population',
        description = "Metro Immigration Ages 13+"))

data.manager$register.outcome(
    'adult.emigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Emigration',
        axis.name = 'Emigration',
        units = 'population',
        description = "Metro Emigration Ages 13+"))

data.manager$register.source('aidsvu', full.name = "AIDS Vu", short.name='aidsvu')

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='cdc')

data.manager$register.source('nsduh', full.name = "National Survey on Drug Use and Health", short.name='nsduh')

data.manager$register.source('lhd', full.name = "Local Health Department", short.name='lhd')

data.manager$register.source('brfss', full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss')

data.manager$register.source('emory', full.name = "Emory University", short.name='emory')

data.manager$register.source('census', full.name = "US Census Bureau", short.name='census')

data.manager$register.ontology(
    'cdc',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

data.manager$register.ontology(
    'aidsvu',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('under 25 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('black', 'hispanic', 'white'),
        sex=c('male','female')
        
    ))

data.manager$register.ontology(
    'cdc.msa.reports',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
        race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'White'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
        
    ))

#This is for the Atlas Plus STI data, creating a separate ontology bc age groups are different#
data.manager$register.ontology(
    'cdc.sti',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('0-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-54 years', '55-64 years', '65+ years', 'unknown'),
        race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White', 'Unknown'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

#Create a separate ontology for early syphilis
data.manager$register.ontology(
    'cdc.syphilis',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('0-14 years', '15-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', '65+ years', 'unknown'), 
        race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White', 'Unknown'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

data.manager$register.ontology(
    'nsduh',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('12 or Older', '12 to 17', '18 or Older', '18 to 25', '26 or Older'))
)

data.manager$register.ontology(
    'lhd',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
        race=c('black', 'hispanic', 'other'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual')
    ))

data.manager$register.ontology(
    'brfss',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('18-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years'),
        race=c('White', 'Black', 'American Indian/Alaska Native', 'Asian', 'Native Hawaiian/Other Pacific Islander', 'Other race', 'Multiracial', 'Hispanic'),
        sex=c('male','female'),
        risk=c('msm')
    ))

data.manager$register.ontology(
    'emory',
    ont = ontology(
        year= NULL,
        location= NULL
    ))

data.manager$register.ontology(
    'census.immigration',
    ont = ontology(
        year= NULL,
        location= NULL,
        age = c("1-4 years", "5-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                "65-69 years", "70-74 years", "75+ years"),
        race=c("Hispanic or Latino", "White, Non-Hispanic", "Black", "Other"),
        sex=c('male','female')
    ))


#library(readxl)

################################################################################
###Read in Aids Vu PrEP Excel Datasets###
################################################################################

DATA.DIR.PREP="../../data_raw/prep/aidsvu"

prep_files <- Sys.glob(paste0(DATA.DIR.PREP, '/*.xlsx'))
data.list.prep <- lapply(prep_files, function(x){
    skip=3
    list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})

################################################################################
###Create list- Total PrEP Use###
################################################################################
data.list.prep.total = lapply(data.list.prep, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Formatting all files will need#
    data$year = data$Year 
    data$outcome = "prep"
    #data= subset(data, data$`State Abbreviation` != "PR") #leaving Puerto Rico in this data#
    
    #Location conditional formatting
    if(grepl("state", filename)) {
        data$location = data$`State Abbreviation`
        
        #Create State Total
        data$count = as.numeric(data$`State PrEP Users`)
        data$count[data$count %in% c("-1")] = NA  #data suppressed#
        data$count[data$count %in% c("-2")] = NA  #data suppressed#
        data$count[data$count %in% c("-4")] = NA  #data not available at county level#
        data$count[data$count %in% c("-8")] = NA  #data undefined#
        data$count[data$count %in% c("-9")] = NA  #data unavailable#
    }
    
    if(grepl("county", filename)) {
        data$FIPS = as.numeric(data$`GEO ID`)
        data$location= str_pad(data$FIPS, width=5, side="left", pad="0")
        
        #Create County total
        data$count = as.numeric(data$`County PrEP Users`)
        data$count[data$count %in% c("-1")] = NA  #data suppressed#
        data$count[data$count %in% c("-2")] = NA  #data suppressed#
        data$count[data$count %in% c("-4")] = NA  #data not available at county level#
        data$count[data$count %in% c("-8")] = NA  #data undefined#
        data$count[data$count %in% c("-9")] = NA  #data unavailable#
    }
    
    data$value = as.numeric(data$count)
    data$year = as.character(data$year)
    
    data= as.data.frame(data)
    
    list(filename, data)
    
})

################################################################################
###Create list- PrEP Use by Sex###
################################################################################
data.list.prep.sex = lapply(data.list.prep, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Formatting all files will need#
    data$year = data$Year 
    data$outcome = "prep"
    
    #Location conditional formatting
    if(grepl("state", filename)) {
        data$location = data$`State Abbreviation`
    }
    if(grepl("county", filename)) {
        data$FIPS = as.numeric(data$`GEO ID`)
        data$location= str_pad(data$FIPS, width=5, side="left", pad="0")
    }
    data$male=data$`Male PrEP Users`
    data$female =data$`Female PrEP Users` 
    
    data <- data %>%
        pivot_longer(cols=c("male", "female"),
                     names_to = "sex",
                     values_to = "value")
    
    data$value = as.numeric(data$value)
    data$year = as.character(data$year)
    
    data$value[data$value %in% c("-1")] = NA  #data suppressed#
    data$value[data$value %in% c("-2")] = NA  #data suppressed#
    data$value[data$value %in% c("-4")] = NA  #data not available at county level#
    data$value[data$value %in% c("-8")] = NA  #data undefined#
    data$value[data$value %in% c("-9")] = NA  #data unavailable#
    
    data= as.data.frame(data)
    
    list(filename, data)
    
})

################################################################################
###Create list-PrEP Use by Age###
################################################################################

data.list.prep.age = lapply(data.list.prep, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Formatting all files will need#
    data$year = data$Year 
    data$outcome = "prep"
    
    #Location conditional formatting
    if(grepl("state", filename)) {
        data$location = data$`State Abbreviation`
    }
    
    if(grepl("county", filename)) {
        data$FIPS = as.numeric(data$`GEO ID`)
        data$location= str_pad(data$FIPS, width=5, side="left", pad="0")
    }
    
    data$`under 25 years`= data$`Age LE 24 PrEP Users`
    data$`25-34 years`= data$`Age 25-34 PrEP Users`
    data$`35-44 years` = data$`Age 35-44 PrEP Users`
    data$`45-54 years`= data$`Age 45-54 PrEP Users`
    data$`55+ years` = data$`Age 55+ PrEP Users`
    
    data <- data %>%
        pivot_longer(cols=c("under 25 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
                     names_to = "age",
                     values_to = "value")
    
    data$value = as.numeric(data$value)
    data$year = as.character(data$year)
    
    data$value[data$value %in% c("-1")] = NA  #data suppressed#
    data$value[data$value %in% c("-2")] = NA  #data suppressed#
    data$value[data$value %in% c("-4")] = NA  #data not available at county level#
    data$value[data$value %in% c("-8")] = NA  #data undefined#
    data$value[data$value %in% c("-9")] = NA  #data unavailable#
    
    data= as.data.frame(data)
    
    list(filename, data)
    
})
################################################################################
###Create list- PrEP Use by Race###
###Note race is not available at the county level###
################################################################################

data.list.prep.state = data.list.prep[11:20] #Subset to just have state level data#

data.list.prep.race = lapply(data.list.prep.state, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Formatting all files will need#
    data$year = data$Year 
    data$outcome = "prep"
    
    #Location conditional formatting
    if(grepl("state", filename)) {
        data$location = data$`State Abbreviation`
        
        data$black=data$`Black PrEP Users`
        data$hispanic =data$`Hispanic PrEP Users`
        data$white =data$`White PrEP Users` 
        
        data <- data %>%
            pivot_longer(cols=c("black", "hispanic", "white"),
                         names_to = "race",
                         values_to = "count")
        
        data$count[data$count %in% c("-1")] = NA  #data suppressed#
        data$count[data$count %in% c("-2")] = NA  #data suppressed#
        data$count[data$count %in% c("-4")] = NA  #data not available at county level#
        data$count[data$count %in% c("-8")] = NA  #data undefined#
        data$count[data$count %in% c("-9")] = NA  #data unavailable#
        
        data$value = as.numeric(data$count)
        data$year = as.character(data$year)
        
        data= as.data.frame(data)
    }
    
    list(filename, data)
    
})

################################################################################
###Put AIDS Vu Files into Data.Manager###
################################################################################

##Total PrEP Use State + County##

prep_total = lapply(data.list.prep.total, `[[`, 2) 
browser()
for (data in prep_total) {
    # error is in the array access line
    data.manager$put.long.form(
        data = data,
        outcome= "prep", 
        ontology.name = 'aidsvu', 
        source = 'aidsvu',
        dimension.values = list(),
        url = 'https://aidsvu.org/',
        details = 'AIDS Vu Reporting')
}
