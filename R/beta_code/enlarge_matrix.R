# enlarge.matrix = function(mat, expansion.factor)
# {
#     m = nrow(mat)
#     n = ncol(mat)
#     M = Reduce(cbind,
#                lapply(1:m, function(i) {
#                    col = numeric(m * expansion.factor)
#                    col[(i - 1) * expansion.factor + (1:expansion.factor)] = 1
#                    col}))
#     N = Reduce(rbind,
#                lapply(1:n, function(j) {
#                    row = numeric(n * expansion.factor)
#                    row[(j - 1) * expansion.factor + (1:expansion.factor)] = 1
#                    row}))
#     M %*% mat %*% N
# }
# mat = matrix(1:15, nrow=5, ncol=3)
# enlarged.mat = enlarge.matrix(mat, 4)

ff = function(n.years, n.obs.locations, n.metalocations) {
    dd = array(0, dim = c(n.years, n.obs.locations, n.years, n.metalocations))
    for (y in 1:n.years) {
        dd[y,,y,] = 1
    }
    dim(dd) = c(n.obs.locations*n.years, n.metalocations*n.years)
    dd
}

ff(6, 4, 5)
