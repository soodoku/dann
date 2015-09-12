#' min2d
#'
#' @param mat matrix 
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' @export
#' @usage min2d()

min2d <-
function (mat=NULL) 
{
	# tester: mat <- matrix(rnorm(100,1,.2), ncol=5)
    i <-  order(mat)[1] # index for the smallest value in the matrix
    dd <- dim(mat)      # dim of the matrix
    i1 <- trunc(i/dd[1]) + 1 # should always be 1 unless it is a matrix with 1 row and then it would be 2
    i2 <- i %% dd[1]    #  
    c(i2, i1)           # 
}