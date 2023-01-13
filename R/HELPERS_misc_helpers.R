
NUMBERS.LETTERS.DASH.PERIOD = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-."

string.contains.invalid.characters <- function(str, valid.characters)
{
    valid.characters = unlist(strsplit(valid.characters, ''))
    sapply(strsplit(str, ''), function(chars){
        length(setdiff(chars, valid.characters))>0
    })
}

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