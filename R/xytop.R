xytop <-
function(xy)
{
    x1 <- xy[1]
    y1 <- xy[2]
    p <- c(0,0,0)
    p[3] = (1 + y1*sqrt(6))/3
    p[1] =  p[3] -y1*sqrt(6)/2 - sqrt(2)*x1/2
    p[2] = 1-p[3]-p[1]
    return(p)
}
