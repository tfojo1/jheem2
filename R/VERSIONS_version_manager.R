

VERSION.MANAGER = new.env()
VERSION.MANAGER$x = numeric()

#'@export
add.test.to.version <- function(val)
{
    VERSION.MANAGER$x = c(VERSION.MANAGER$x, val)
}

#'@export
get.test.from.version <- function()
{
    VERSION.MANAGER$x
}