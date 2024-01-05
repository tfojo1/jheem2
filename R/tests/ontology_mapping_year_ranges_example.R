# These ontologies represent what happens with the new migration data
ont1 = ontology(year='2011-2015', incomplete.dimensions = 'year')
ont2 = ontology(year=as.character(2011:2015), incomplete.dimensions = 'year')

# These mappings should map arrays to something in common, which I'd guess would be an ontology with just the year range
mm=get.mappings.to.align.ontologies(ont1, ont2, allow.non.overlapping.incomplete.dimensions = T)
arr1 = array(36, dim=c(year=1), ont1)
arr2 = array(1:5, dim=c(year=5), ont2)

# Instead, they just keep the arrays the same, meaning that I can't "align" them into one ontology
fromarr1=mm$mapping.from.1$apply(arr1)
fromarr2=mm$mapping.from.2$apply(arr2)

identical(dim(arr1), dim(fromarr1)) # TRUE as I expected
identical(dim(arr2), dim(fromarr2)) # TRUE but I expected FALSE; I thought dim(fromarr2) would now be "1"
