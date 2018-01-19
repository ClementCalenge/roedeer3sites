ptoxy <-
function(p)
{
    x <- p
    x1 <- (x[2] - x[1])/sqrt(2)
    y1 <- (2 * x[3] - x[2] - x[1])/sqrt(6)
    xy <- c(x1,y1)
    return(xy)
}
