

##-------------------##
##-------------------##
##-- INTERPOLATING --##
##-------------------##
##-------------------##


# this is now implemented in a cpp function
# We leave the old code here in case we ever need it to help understand the cpp
#  or to compare against cpp output
OLD.interpolate <- function(values,
                        value.times,
                        desired.times)
{
    if (length(values) != length(value.times))
        stop("To interpolate, 'values' must have the same length as 'value.times'")
    
    if (is.list(values))
        fn = lapply
    else
    {
        fn = sapply
        values = as.list(values)   
    }
    
    n.times = length(value.times)
    fn(desired.times, function(time){
        index.before = (1:n.times)[value.times<=time]
        index.before = index.before[length(index.before)]
        index.after = (1:n.times)[value.times > time][1]
        
        if (length(index.before)==0)
            values[[index.after]]
        else if (is.na(index.after))
            values[[index.before]]
        else if (value.times[index.after] == time)
            values[[index.after]]
        else if (value.times[index.before] == time)
            values[[index.before]]
        else if (is.infinite(value.times[index.after]))
            values[[index.after]]
        else
        {
            (values[[index.before]] * (value.times[index.after] - time) +
                 values[[index.after]] * (time - value.times[index.before])) /
                (value.times[index.after] - value.times[index.before])
        }
    })
}

#'@export
interpolate <- function(values,
                        value.times,
                        desired.times)
{
    if (length(values) != length(value.times))
        stop("To interpolate, 'values' must have the same length as 'value.times'")
    
    if (length(values)==0)
        stop("To interpolate, 'values' cannot be empty")
    
    was.list = is.list(values)
    if (was.list)
    {
        len = length(values[[1]])
        if (any(vapply(values[-1], length, FUN.VALUE = integer(1)) != len))
            stop("To interpolate, all the elements of 'values' must have the same length")
    }
    else
    {
        values = as.list(values);
    }

    # This cpp function is in optimize_engine.cpp
    rv = do_interpolate(values = values,
                        value_times = value.times,
                        desired_times = desired.times)
    
    if (!was.list)
        rv = unlist(rv)
    
    rv
}

interpolate.array <- function(arr,
                              dimension = 'year',
                              desired.times = NULL)
{
    if (!is.numeric(arr) || !is.array(arr))
        stop("'arr' must be a numeric array")
    
    if (is.null(dimnames(arr)))
        stop("'arr' must have dimnames set")
    
    if ((!is.character(dimension) &&!is.numeric(dimension)) ||
        length(dimension)!=1 || is.na(dimension))
        stop("'dimension' must be a single, non-NA character or numeric value")
    
    if (is.character(dimension))
    {
        if (all(names(dim(arr))!=dimension))
            stop(paste0("The dimension '", dimension, "' is not present in 'arr'"))
        
        other.dimensions = setdiff(names(dim(arr)), dimension)
    }
    else if (is.numeric(dimension))
    {
        if (dimension < 1 || dimension > length(dim(arr)))
            stop(paste0("dimension (", dimension, ") must be between 1 and ", length(dim(arr))))
        
        other.dimensions = setdiff(1:length(dim(arr)), dimension)
    }
    
    times = suppressWarnings(as.numeric(dimnames(arr)[[dimension]]))
    if (any(is.na(times)))
        stop(paste0("dimnames(arr)[[", dimension, "]] cannot be interpreted as numbers"))
    
    if (is.null(desired.times))
        desired.times = times
    else if (!is.numeric(desired.times) || length(desired.times)==0 || any(is.na(desired.times)))
        stop("'desired.times' must be a numeric vector with no NA values")
    
    raw.interpolated = apply(arr, other.dimensions, function(values){
        if (all(is.na(values)))
            stop("Cannot interpolate array - at least one margin contains all NA values")
        interpolate(values = values[!is.na(values)], 
                    value.times = times[!is.na(values)],
                    desired.times = desired.times)
    })
    
    dim.names = dimnames(arr)[c(dimension, other.dimensions)]
    dim.names[[dimension]] = as.character(desired.times)
    dim(raw.interpolated) = sapply(dim.names, length)
    dimnames(raw.interpolated) = dim.names
    
    apply(raw.interpolated, names(dim(arr)), function(x){x})
}

##----------------------------##
##----------------------------##
##-- MISC STRING OPERATIONS --##
##----------------------------##
##----------------------------##



# str - a character vector
# contents - a single string value
# returns - a logical vector the length of contents
string.contains <- function(str, contents)
{
    sapply(str, function(one.str){
        if (nchar(contents)<=nchar(one.str))
        {
            any(sapply(1:(nchar(one.str)-nchar(contents)+1), function(i){
                substr(one.str, i, i+nchar(contents)-1)==contents
            }))
        }
        else
            F
    })
}

# str - a character vector
# contents - a single string value
# returns - a logical vector the length of contents
string.begins.with <- function(str, prefix)
{
    sapply(str, function(one.str){
        substr(one.str, 1, nchar(prefix)) == prefix
    })
}

# str - a character vector
# contents - a single string value
# returns - a logical vector the length of contents
string.ends.with <- function(str, postfix)
{
    sapply(str, function(one.str){
        substr(one.str, nchar(one.str)-nchar(postfix)+1, nchar(one.str)) == postfix
    })
}

NUMBERS.LETTERS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
NUMBERS.LETTERS.PERIOD = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ."
NUMBERS.LETTERS.DASH.PERIOD = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-."
NUMBERS.LETTERS.DASH.PERIOD.SPACE = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-. "
NUMBERS.LETTERS.DASH.PERIOD.COMMA.SPACE = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-., "
NUMBERS.LETTERS.DASH.PERIOD.UNDERSCORE = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-._"
NUMBERS.LETTERS.SPACE.DASH.PERIOD.UNDERSCORE = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-._ "

# str - a single character value
string.contains.invalid.characters <- function(str, valid.characters)
{
    valid.characters = unlist(strsplit(valid.characters, ''))
    sapply(strsplit(str, ''), function(chars){
        length(setdiff(chars, valid.characters))>0
    })
}

check.for.invalid.characters <- function(str,
                                         valid.characters,
                                         str.name,
                                         valid.characters.description = NULL,
                                         error.prefix = '')
{
    if (string.contains.invalid.characters(str, valid.characters = valid.characters))
    {
        invalid.characters = setdiff(strsplit(str, split='')[[1]],
                                     strsplit(valid.characters, split='')[[1]])
        
        if (is.single.value)
            stop(paste0(error.prefix,
                        str.name,
                        " ('", str, "') cannot contain ", 
                        collapse.with.or("'", invalid.characters, "'"),
                        ifelse(is.null(valid.characters.description), '', 
                               paste0(" - it can only contain ", valid.characters.description))
            ))
    }
}


##-------------------------------------##
##-------------------------------------##
##-- PRETTY PRINTING/TEXT FORMATTING --##
##-------------------------------------##
##-------------------------------------##

collapse.with.and <- function(..., separator=',')
{
    collapse.with.conjunction(..., conjunction='and', separator=separator)
}

collapse.with.or <- function(..., separator=',')
{
    collapse.with.conjunction(..., conjunction='or', separator=separator)
}


collapse.with.conjunction <- function(...,
                                      conjunction = 'and',
                                      separator = ',')
{
    to.collapse = paste0(...)
    
    if (length(to.collapse)==1)
        to.collapse
    else if (length(to.collapse)==2)
        paste0(to.collapse[1], " ", conjunction, " ", to.collapse[2])
    else
        paste0(paste0(to.collapse[-length(to.collapse)], collapse=paste0(separator, ' ')),
               separator, " ", conjunction, " ", to.collapse[length(to.collapse)])
}

toupper.first <- function(str)
{
    str = as.character(str)
    mask = !is.na(str) & nchar(str)>0
    str[mask] = paste0(toupper(substr(str[mask], 1, 1)),
                       substr(str[mask], 2, nchar(str[mask])))
    str
}

str.to.title <- function(str)
{
    split.str = strsplit(str, "[^a-zA-Z0-9\\-]", fixed=F)
    str = sapply(split.str, function(one.split){
        paste0(toupper.first(one.split), collapse=' ')
    })
    
    split.str = strsplit(str, "-", fixed=T)
    str = sapply(split.str, function(one.split){
        paste0(toupper.first(one.split), collapse='-')
    })
    
    str
}

get.ordinal <- function(nums)
{
    ORDINAL.SUFFIXES = c('th', #0
                         'st', #1
                         'nd', #2,
                         'rd', #3,
                         'th', #4
                         'th', #5
                         'th', #6
                         'th', #7
                         'th', #8
                         'th') #9
    
    last.digits = nums - (10 * floor(nums/10)) + 1
    rv = paste0(nums, ORDINAL.SUFFIXES[last.digits])
    
    last.two.digits = nums - (100 * floor(nums/100))
    rv[last.two.digits==11] = paste0(nums[last.two.digits==11], 'th')
    
    rv
}


##--------------##
##--------------##
##-- INDEX OF --##
##--------------##
##--------------##

first.index.of <- function(haystack, needle)
{
    (1:length(haystack))[haystack==needle][1]
}

# needles must be a vector, haystack a matrix
# returns all the row indices (if any) where needle appears in haystack
row.indices.of <- function(haystack, needle)
{
    mask = apply(haystack, 1, function(val){
        all( (is.na(val) & is.na(needle)) | 
                 (!is.na(val) & !is.na(needle) & val==needle))
    })
    
    (1:length(mask))[mask]
}

##-------------------------------##
##-------------------------------##
##-- FUNCTIONS and EXPRESSIONS --##
##-------------------------------##
##-------------------------------##

get.function.names.in.expr <- function(ex)
{
    setdiff(all.vars(ex, functions = T), all.vars(ex, functions = F))
}

get.function.argument.names <- function(fn, 
                                        exclude.arguments.with.default.values=F,
                                        exclude.dot.dot.dot=exclude.arguments.with.default.values)
{
    fn.args = formals(args(fn))
    arg.names = names(fn.args)
    
    if (exclude.arguments.with.default.values)
        rv = arg.names[as.logical(sapply(fn.args, function(val){!is.null(val) && val==''}))]
    else
        rv = arg.names
    
    if (exclude.dot.dot.dot)
        rv[rv!='...']
    else
        rv
}

##------------------##
##------------------##
##-- COMBINATIONS --##
##------------------##
##------------------##

# values is a list of non-empty vectors
# returns a matrix
# one column for each element in values
# one row for each combo of one value from each element of values
#'@export
get.every.combination <- function(values)
{
    n = length(values)
    n.val = sapply(values, length)
    n.before = c(1, cumprod(n.val[-n]))
    n.after = c(rev(cumprod(rev(n.val[-1]))),1)
    
    matrix(sapply(1:n, function(i){
        rep(rep(values[[i]], n.after[i]), each=n.before[i])
    }), ncol=n, dimnames = list(NULL, names(values)))
}

# x and y must be matrices with the same number of columns
# The result is a matrix (with the same columns)
#   which contains every row in x that is not in y
setdiff.rows <- function(x, y)
{
    has.match = sapply(1:nrow(x), function(i){
        any(sapply(1:nrow(y), function(j){
            
            all(is.na(x[i,]) & is.na(y[j,]) |
                    ( (!is.na(x[i,]) & !is.na(y[j,])) & (x[i,]==y[j,]) ))
            
        }))
    })
    
    x[!has.match,,drop=F]
}

# If needles is a vector, returns the index, i, 
#   of the first row for which all(haystack[i,] == needles)
# If needles is a matrix, returns a vector of row indices into haystack
#   one for each row in needle
# Returns only the first row index for each needle
row.index.of <- function(haystack, needles)
{
    if (is.null(dim(needles)))
        dim(needle) = c(1,length(needle))
        
    apply(needles, 1, function(one.needle){
        
        mask = apply(haystack, 1, function(val){
            all( (is.na(val) & is.na(one.needle)) | 
                 (!is.na(val) & !is.na(one.needle) & val==one.needle))
        })
        
        if (!any(mask))
            NA
        else
            (1:dim(haystack)[1])[mask][1]
    })
    
}

is.subset <- function(sub, super)
{
    length(setdiff(sub, super))==0
}

##-----------##
##-----------##
##-- OTHER --##
##-----------##
##-----------##

# if length(values)<n
# makes a vector of length n, where the first 1:length(values) elements == values
# and all subsequent elements are values[length(values)]
pad.with.last.value <- function(values, n)
{
    if (length(values)<n)
        c(values, rep(values[length(values)], n-length(values)))
    else
        values
}

r6.sets.equal <- function(set1, set2)
{
    if (length(set1) == length(set2))
    {
        for (obj1 in set1)
        {
            found.match = F
            for (i2 in 1:length(set2))
            {
                obj2 = set2[[i2]]
                if (obj1$equals(obj2))
                {
                    found.match = T
                    set2 = set2[-i2]
                    break
                }
            }
            
            if (!found.match)
                return (F)
        }
        
        T
    }
    else
        F
}