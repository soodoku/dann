#' stand
#'
#' @param x
#' @param xx
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' @export
#' @usage # dann2()

stand <-
function (x, xx) 
{
    mm <- apply(x, 2, mean)
    dd <- sqrt(apply(x, 2, var))
    x  <- scale(x, mm, dd)
    xx <- scale(xx, mm, dd)
    return(list(x = x, xx = xx))
}
