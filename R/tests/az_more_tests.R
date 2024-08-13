
# source('R/SPECIFICATION_scales.R')
# source('R/HELPERS_misc_helpers.R')
# source('R/DATA_MANAGER_data_manager.R')
# source('R/ONTOLOGY_ontology.R')
# source('R/ONTOLOGY_ontology_mappings.R')
# source('R/HELPERS_array_helpers.R')
# source('R/HELPERS_dim_names_helpers.R')
# source('R/SPECIFICATION_model_specification.R') #has the outcome.metadata object definition
# Rcpp::sourceCpp('src/array_helpers.cpp')
# Rcpp::sourceCpp('src/ontology_mappings.cpp')

data.manager = create.data.manager('test', description='a data manager to test with')
data.manager$register.outcome('sales', 
                              metadata = create.outcome.metadata(scale = 'non.negative.number',
                                                                 display.name = 'Sales',
                                                                 axis.name = 'Sales (n)',
                                                                 units = 'drinks',
                                                                 description = "Annual Smoothie Sales"))

data.manager$register.outcome(
    'tipped',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Tipped',
        axis.name = 'Tipped',
        units = 'proportion of sales',
        description = "Proportion of Annual Smoothie Sales With Nonzero Tip"),
    denominator.outcome = 'sales')

data.manager$register.source('asa', full.name = "American Smoothie Association", short.name='ASA')
data.manager$register.source('ieg', full.name = "Institute of Educated Guessing", short.name='IEG')

data.manager$register.ontology(
    'ASA',
    ont=ontology(
        year=NULL,
        color=c('orange', 'blue', 'green'),
        size=c('small', 'large')
    )
)

# --- First put fills out the incomplete dimension ---
if (TRUE) {data1.dimnames = list(
    year=c('2008', '2009'),
    color=c('orange', 'blue', 'green'),
    size=c('small', 'large')
)

data1 = array(
    100000*(1:12),
    dim = sapply(data1.dimnames, length),
    dimnames=data1.dimnames
)

data.manager$put(
    data = data1,
    outcome = 'sales',
    source = 'asa',
    ontology.name = 'ASA',
    dimension.values = list(),
    url = 'www.smoothies4all.com',
    details = 'ASA reporting'
)
}

# --- Another put expands an incomplete dimension ---
if (T) {
    data2.dimnames = list(
        year=c('2010'),
        color=c('orange', 'blue', 'green'),
        size=c('small', 'large')
    )
    
    data2 = array(
        100000*(1:6),
        dim = sapply(data2.dimnames, length),
        dimnames=data2.dimnames
    )
    
    # Throws an error
    # Examining the data in the data manager afterwards shows year=2010 is now a
    # dimension value, but all data is NaN.
    # This probably means new values for an incomplete dimension cannot be added...
    # Though this doesn't seem best because it is quite likely new data will need
    # to be added for years/locations that the first put didn't have.
    data.manager$put(
        data=data2,
        outcome = 'sales',
        source = 'asa',
        ontology.name = 'ASA',
        dimension.values = list(year='2010'),
        url = 'www.smoothiekingdom.net',
        details = 'ASA reporting'
    )
}

# --- Another put uses dimension.values to add data to a subset of existing dimensions ---
if (F) {
    data3.dimnames = list(
        year=c('2008'),
        color=c('orange'),
        size=c('small', 'large')
    )
    
    data3 = array(
        1000000*(1:2),
        dim=sapply(data3.dimnames, length),
        dimnames=data3.dimnames
    )
    
    data.manager$put(
        data=data3,
        outcome='sales',
        source='asa',
        ontology.name='ASA',
        dimension.values = list(year='2008', color=c('orange')),
        url='www.americansmoothiefund.org',
        details = 'ASA reporting'
    )
}

# --- Another put adds data aggregated across a complete dimension, which is okay ---
if (F) {
    data4.dimnames = list(
        year=c('2009'),
        color=c('orange', 'blue')
    )
    
    data4 = array(
        1000000 + 100000*(1:2),
        dim = sapply(data4.dimnames, length),
        dimnames = data4.dimnames
    )
    
    data.manager$put(
        data=data4,
        outcome='sales',
        source='asa',
        ontology.name='ASA',
        dimension.values = list(),
        url='www.americansmoothiefund.org',
        details='ASA reporting'
    )
}

# --- This put adds data aggregated across an incomplete dimension, which should be illegal ---
if (FALSE) {
    data5.dimnames = list(
        color=c('orange', 'green', 'blue'),
        size=c('small', 'large')
    )
    
    data5 = array(
        7,
        dim=sapply(data5.dimnames, length),
        dimnames = data5.dimnames
    )
    
    data.manager$put(
        data=data5,
        outcome='sales',
        source='asa',
        ontology.name='ASA',
        dimension.values = list(),
        url='www.americansmoothiefund.org',
        details='ASA reporting'
    )
}

# --- This put adds data for a new source ---
if (T) {
    data6.dimnames = list(
        year=c('2008', '2009'),
        color=c('orange', 'green', 'blue'),
        size=c('small','large')
    )
    
    data6 = array(
        3100,
        dim=sapply(data6.dimnames, length),
        dimnames = data6.dimnames
    )
    
    data.manager$put(
        data=data6,
        outcome='sales',
        source='ieg',
        ontology.name='ASA',
        dimension.values = list(),
        url='www.ieg.org',
        details='IEG guess'
    )
}

# --- This put adds data using only a single value input ---
if (T) {
    data.manager$put(
        data=5000,
        outcome='sales',
        source='asa',
        ontology.name='ASA',
        dimension.values = list(year='2007', color=c('orange', 'green', 'blue')),
        url='www.americansmoothiefund.org',
        details='ASA reporting'
    )
}

# --- This put adds proportion data ---
if (T) {
    data7.dimnames = list(
        year=c('2008', '2009'),
        color=c('orange', 'green', 'blue'),
        size=c('small','large')
    )
    
    data7 = array(
        (1:12)*(1/12),
        dim=sapply(data7.dimnames, length),
        dimnames=data7.dimnames
    )
    
    data.manager$put(
        data7,
        outcome='tipped',
        source='asa',
        ontology.name='ASA',
        dimension.values = list(), 
        url='www.americansmoothiefund.org',
        details='ASA reporting'
    )
}

# --- This pull should return NULL because it doesn't specify incomplete dimension values ---
if (F) {
    pull1 = data.manager$pull(
        outcome='sales',
        keep.dimensions = c('color', 'size')
    )
}

# --- This pull should fail because target.ontology has a NULL dimension ---
if (F) {
    target.ontology = ontology(
        year=NULL,
        color=c('orange', 'blue', 'green'),
        size=c('small', 'large'),
        incomplete.dimensions = c('year')
    )
    pull2 = data.manager$pull(
        outcome='sales',
        target.ontology=target.ontology
    )
}

# --- This pull should return NULL because keep.dimensions will be inferred to not have year---
if (T) {
    target.ontology = ontology(
        year=c('2008', '2009', '2010'),
        color=c('orange', 'blue', 'other'),
        size=c('small', 'large'),
        incomplete.dimensions = c('year')
    )
    pull3 = data.manager$pull(
        outcome='sales',
        dimension.values = list(color='orange'),
        target.ontology=target.ontology,
        allow.mapping.from.target.ontology = T
    )
}

# --- This pull should succeed---
if (T) {
    target.ontology = ontology(
        year=c('2007'),
        color=c('orange', 'blue', 'other'),
        size=c('small', 'large'),
        incomplete.dimensions = c('year')
    )
    pull4 = data.manager$pull(
        outcome='sales',
        dimension.values = list(color='orange'),
        keep.dimensions = c('year', 'color'),
        target.ontology=target.ontology,
        allow.mapping.from.target.ontology = T
    )
}

# --- This pull should get data from two sources ---
if (T) {
    pull5 = data.manager$pull(
        outcome='sales',
        keep.dimensions=c('color', 'size'),
        dimension.values=list(year='2009', color=c('orange', 'green'))
    )
}


# data.manager$check()
