
#'Flexibly Subset an Array
#'
#'@description Function for subsetting an array when the number and order of dimensions is not known at the point of coding
#'
#'@param arr The array or matrix to be subsetted. Must have named dimensions and named dimnames set
#'@param dimension.values A named list containing the values of dimensions to subset for. The names of dimension.values should correspond to the names of dimnames. The elements of dimension.values should be character, integer, or logical vectors
#'@param flatten.length.one.dimensions If set to true, any dimensions in the return value that have length one are omitted (as with standard subsetting). If false, then the dimensions are retained in the array
#'
#'@examples
#'arr = array(1:24, dim=c(age=4, race=3, sex=2), 
#'                  dimnames=list(age=c('age1','age2','age3','age4'), 
#'                                race=c('black','hispanic','other'), 
#'                                sex=c('female','male')))
#'                                
#'array.access(arr, dimension.values=list(age=1:3, race='black'))
#'#is equivalent to
#'arr[1:3, 'black',]
#'
#'@export
array.access <- function(arr,
                         dimension.values,
                         flatten.length.one.dimensions=F)
{
    avoid.flattening = !flatten.length.one.dimensions
    if (avoid.flattening)
    {
        subset.values = lapply(names(dim(arr)), function(d){
            if (is.null(dimension.values[[d]]))
                dimnames(arr)[[d]]
            else if (is.character(dimension.values[[d]]))
                dimension.values[[d]]
            else
                dimnames(arr)[[d]][dimension.values[[d]]]
        })
    }
    else
    {
        subset.values = lapply(names(dim(arr)), function(d){
            if (is.null(dimension.values[[d]]))
                1:dim(arr)[d]
            else
                dimension.values[[d]]
        })
    }
    
    arr = do.call('[', args=c(list(arr), subset.values))
    
    if (avoid.flattening)
    {
        dim(arr) = sapply(subset.values, length)
        dimnames(arr) = subset.values
    }
    
    arr
}
