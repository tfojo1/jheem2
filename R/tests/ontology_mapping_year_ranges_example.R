# These ontologies represent what happens with the new migration data
ont1 = ontology(year='2011-2015', incomplete.dimensions = 'year')
ont2 = ontology(year=as.character(2011:2015), incomplete.dimensions = 'year')

# I expected both of these to work in this case, but one doesn't
get.mappings.to.align.ontologies(ont1, ont2, allow.non.overlapping.incomplete.dimensions = F) # an identity and a basic (correct)
get.mappings.to.align.ontologies(ont1, ont2, allow.non.overlapping.incomplete.dimensions = T) # two identities (incorrect)