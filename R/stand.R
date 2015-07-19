
stand <-
function (x, xx) 
{
    mm <- apply(x, 2, mean)
    dd <- sqrt(apply(x, 2, var))
    x <- scale(x, mm, dd)
    xx <- scale(xx, mm, dd)
    return(list(x = x, xx = xx))
}
