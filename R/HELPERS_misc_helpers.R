

##-------------------##
##-------------------##
##-- INTERPOLATING --##
##-------------------##
##-------------------##

interpolate <- function(values,
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
    paste0(nums, ORDINAL.SUFFIXES[last.digits])
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

##------------------##
##------------------##
##-- COMBINATIONS --##
##------------------##
##------------------##

# values is a list of non-empty vectors
# returns a matrix
# one column for each element in values
# one row for each combo of one value from each element of values
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