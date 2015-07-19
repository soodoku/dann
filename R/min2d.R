
min2d <-
function (mat) 
{
	# tester: mat <- matrix(rnorm(100,1,.2), ncol=5)
    i <- order(mat)[1]
    dd <- dim(mat)
    i1 <- trunc(i/dd[1]) + 1
    i2 <- i %% dd[1]
    c(i2, i1)
}