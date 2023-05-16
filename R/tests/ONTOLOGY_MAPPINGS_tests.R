
source('../jheem2/R/HELPERS_array_helpers.R')
source('../jheem2/R/HELPERS_dim_names_helpers.R')
source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
source('../jheem2/R/ONTOLOGY_ontology.R')
Rcpp::sourceCpp("../jheem2/src/ontology_mappings.cpp")

dim.names = list(age=c('age1','age2','age3'), race=c('black','hispanic','other'))

test.numeric.arr = array(1, dim=sapply(dim.names, length), dimnames=dim.names)
test.sequential.arr = array(1:prod(sapply(dim.names, length)), dim=sapply(dim.names, length), dimnames=dim.names)
test.sequential.char.arr = array(as.character(1:prod(sapply(dim.names, length))), dim=sapply(dim.names, length), dimnames=dim.names)

map1 = create.ontology.mapping(
    mappings = rbind(
        c("black", "black"),
        c("hispanic", "other"),
        c("other", "other")
    ),
    
    from.dimensions = 'race',
    to.dimensions = 'race'
)

map2 = create.ontology.mapping(
    mappings = rbind(
        c('age1', 'young'),
        c('age2', 'young'),
        c('age3', 'old')
    ),
    
    from.dimensions = 'age',
    to.dimensions = 'age'
)

map12 = combine.ontology.mappings(map1, map2)

cat("AGGREGATE NUMERIC TEST...")
aggregated.test = map1$apply(test.numeric.arr)
success = all(array.access(aggregated.test, race='black')==1) && all(array.access(aggregated.test, race='other')==2)
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")

cat("CONCATENATE CHAR TEST...")
concatenated.char = map1$apply(test.sequential.char.arr, fun=function(x){paste0(x, collapse=', ')})
sucess = all(array.access(concatenated.char, race='black') == array.access(test.sequential.char.arr, race='black')) &&
    all(array.access(concatenated.char, race='other') == paste0(array.access(test.sequential.char.arr, race='hispanic'), ', ', array.access(test.sequential.char.arr, race='other')))
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")


cat("AGGGREGATE COMBO MAPPING TEST...")
aggregated.combo = map12$apply(test.numeric.arr)
success = all(array.access(aggregated.combo, race='black', age='old')==1) &&
    all(array.access(aggregated.combo, race='black', age='young')==2) &&
    all(array.access(aggregated.combo, race='other', age='old')==2) &&
    all(array.access(aggregated.combo, race='other', age='young')==4)
if (success)
    cat("SUCCESS!\n")
if (!success)
    cat("******* FAILED *********\n")

if (1==2)
{
    # Speed Test
    N.ITER = 1000
    
    start.direct = Sys.time()
    for (i in 1:N.ITER)
        x = map1$apply(test.numeric.arr)
    end.direct = Sys.time()
    
    start.indirect = Sys.time()
    for (i in 1:N.ITER)
        x = map1$apply(test.numeric.arr, fun=function(x){sum(x)})
    end.indirect = Sys.time()
    
    start.ref = Sys.time()
    for (i in 1:N.ITER)
        x = cbind(test.numeric.arr[,1],
                  test.numeric.arr[,2]+test.numeric.arr[,3])
    end.ref = Sys.time()
    
    direct.vs.indirect = (as.numeric(end.direct)-as.numeric(start.direct)) / (as.numeric(end.indirect) - as.numeric(start.indirect))
    direct.vs.ref = (as.numeric(end.direct)-as.numeric(start.direct)) / (as.numeric(end.ref) - as.numeric(start.ref))
    print(paste0("direct / indirect: ", round(direct.vs.indirect,3)) )
    print(paste0("direct / ref: ", round(direct.vs.ref,3)) )
}