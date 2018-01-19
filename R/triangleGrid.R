triangleImage <- function(pred, gr, names=c("A","B","C"),
                          cuts = c(-0.1, 0.05, 0.15, 0.5, 0.85, 0.95, 1.1),
                          col = c("lightgrey", "darkgrey","darkgrey","darkgrey",
                          "darkgrey","black"),
                          listlim = NULL,
                          lowerleft=c(1,0,0), lowerright=c(0,1,0), top=c(0,0,1))
{
    ## Transform to spatialPixelsDataFrame
    dd <- data.frame(pred)
    coordinates(dd) <- gr
    gridded(dd) <- TRUE

    ## keeps the grid within the limits
    if (!is.null(listlim)) {
        xlim <- listlim$xlim
        ylim <- listlim$ylim
        ppp <- gr[,1]>=xlim[1]&gr[,1]<=xlim[2]&gr[,2]>=ylim[1]&gr[,2]<=ylim[2]
    } else {
        ppp <- rep(TRUE, nrow(gr))
    }

    ## Elements copied from the function triangle.plot (package ade4)
    A <- ptoxy(lowerleft) # lower left corner
    B <- ptoxy(lowerright) # lower right corner
    C <- ptoxy(top) # top corner
    to <- rbind(A,B,C)

    ## Keeps the points within the limits
    ppp <- ppp&inout(as.matrix(gr), to)

    ## Prepare the plot
    par(mar = c(0.1,0.1,0.1,0.1))

    ## The limits used for the probability that the log-ratio is greater than 0
    se <- cuts
    ## ...as well as the colors
    cole <- col

    ## Plot the image
    plot(to[,1], to[,2], asp=1, ty="n", xlim = c(-0.8,0.4),
         ylim=c(-0.5, 0.55), axes=FALSE)
    image(dd[ppp,], add=TRUE, col=cole, breaks=se)
    ## and the limits
    segments(A[1], A[2], B[1], B[2], lwd = 2)
    segments(C[1], C[2], B[1], B[2], lwd = 2)
    segments(A[1], A[2], C[1], C[2], lwd = 2)
    mini <- c(0.3,0,0)
    maxi <- c(1,0.7,0.7)
    text(C[1], C[2], labels = paste(mini[1]), pos = 2)
    text(C[1], C[2], labels = paste(maxi[3]), pos = 4)
    text((A + C)[1]/2, (A + C)[2]/2, labels = names[1], cex = 1.5,
         pos = 2)
    text(A[1], A[2], labels = paste(maxi[1]), pos = 2)
    text(A[1], A[2], labels = paste(mini[2]), pos = 1)
    text((A + B)[1]/2, (A + B)[2]/2, labels = names[2], cex = 1.5,
         pos = 1)
    text(B[1], B[2], labels = paste(maxi[2]), pos = 1)
    text(B[1], B[2], labels = paste(mini[3]), pos = 4)
    text((B + C)[1]/2, (B + C)[2]/2, labels = names[3], cex = 1.5,
         pos = 4)
    box()
    return(invisible(dd[ppp,]))
}


triangleGrid <- function(ngrid=150)
{
    ## Min and max coordinates in the ecological triangle
    xmin <- -0.75
    xmax <- 0.75
    ymin <- -0.45
    ymax <- 0.85

    ## Grid of value on the simplex
    gr <- expand.grid(seq(xmin, xmax, length=ngrid), seq(ymin, ymax, length=ngrid))

    ## convert to the proportions of the home range covered by the three habitat types
    pp <- t(apply(gr,1,xytop))

    ## As all points generated on the grid are not necessarily within the triangle
    ## we kee only the points located within these limits.
    ## These are points for which the corresponding values are comprised between 0 and 1
    ## and for which the sum is equal to 1.
    tr <- apply(pp, 1, function(x) (all(x>0)&all(x<1)&abs(sum(x)-1)<0.000000001))

    ## And we keep these points, as well in the three-dimensional space defined by
    ## the three proportions...
    pp <- pp[tr,]
    ## ... as in the ecological triangle
    gr <- gr[tr,]

    ## Results
    return(list(triangle=gr, proportions=pp))

}
