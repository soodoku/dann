#' min2d
#'
#' @param mat matrix 
#' @return A vector
#' @examples \dontrun{
#' min2d(matrix(rnorm(100,1,.2), ncol=5))
#' }

min2d <-
function (mat=NULL) 
{
    i <-  order(mat)[1] # index for the smallest value in the matrix
    dd <- dim(mat)      # dims of the matrix
    i1 <- trunc(i/dd[1]) + 1 # should always be 1 unless it is a matrix with 1 row and then it would be 2
    i2 <- i %% dd[1]    #  
    c(i2, i1)           # 
}