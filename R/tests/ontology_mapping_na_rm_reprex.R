# Issue with how ontology mappings handle NA when they are applied

# cdc ontology
ont1 = ontology(year=as.character(2010:2011),
                location=c('here', 'there'),
                race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
                incomplete.dimensions=c('year', 'location'))

# jheem ontology
ont2 = ontology(year=as.character(2010:2011),
                location=c('here', 'there'),
                race=c('black', 'hispanic', 'other'),
                incomplete.dimensions = c('year', 'location'))

# a basic mapping over race (if it has been registered)
mp = get.ontology.mapping(ont1, ont2)

# an array where one location has data for more years than another in the same array, a situation we often have
arr1 = array(rep(c(1, 1, NA, 1), 7), sapply(ont1, length), ont1)

# We will applying our mapping WITH SUBSETTING built in, asking only for the location with some missing data. This is how the pull function does it (where dimension values guide the subsetting)

# If we remove the NAs, zeroes are created that have numerous downstream consequences, because they are making it look like we have data when we don't. This is what we saw in suppression's "n-multipliers" recently.
mp$apply(arr1, list(year=ont2$year, location='there', race=ont2$race), fun = function(x){sum(x, na.rm=T)})

# Here, not removing NAs gets the behavior we expected
mp$apply(arr1, list(year=ont2$year, location='there', race=ont2$race), fun = function(x){sum(x, na.rm=F)})

# But here's a situation where we do want to remove NAs
arr2 = arr1
arr2['2011', 'there', 'American Indian/Alaska Native'] = NA

# If we don't, the NA is propagated and we'll be stuck with no estimate of the 'other' race count in the output. This is the case we saw in suppression's "obs-n" recently.
mp$apply(arr2, list(year=ont2$year, location='there', race=ont2$race), fun = function(x){sum(x, na.rm=F)})
