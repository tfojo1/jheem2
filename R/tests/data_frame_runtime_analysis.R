n = 500
xx = 1:n
yy = 2 * 1:n
df = as.data.frame(list(xx=xx, yy=yy))

microbenchmark::microbenchmark(
    {lapply(1:length(xx), function(x) {
        lapply(1:length(yy), function(y) {
            xx == yy
        })
    })},
    {lapply(1:length(xx), function(x) {
        lapply(1:length(yy), function(y) {
            df[[x, 'xx']] == df[[y, 'yy']]
        })
    })}
)

# for n = 100, median 14 milliseconds vs. 114 milliseconds
# for n = 500, median 0.9 seconds vs. 3.0 seconds