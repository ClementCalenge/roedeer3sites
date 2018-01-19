marginalityTriangle <-
function(availMatrix, useMatrix,
                                names=colnames(availMatrix), main="")
{

    ##  Use and available probas on the triangle
    predu <- t(apply(useMatrix,1,ptoxy))
    predd <- t(apply(availMatrix,1,ptoxy))

    ## Elements copied from the function triangle.plot in the package ade4
    A <- ptoxy(c(1,0,0))
    B <- ptoxy(c(0,1,0))
    C <- ptoxy(c(0,0,1))
    to <- rbind(A,B,C)

    ## Représentation graphique
    par(mar = c(0.1,0.1,0.1,0.1))
    se <- c(0, 0.2, 0.4, 0.5, 0.6, 0.8, 1)
    plot(to[,1], to[,2], asp=1, ty="n", xlim = c(-0.8,0.8), axes=FALSE, main="")

    segments(A[1], A[2], B[1], B[2], lwd = 2, col="lightgrey")
    segments(C[1], C[2], B[1], B[2], lwd = 2, col="lightgrey")
    segments(A[1], A[2], C[1], C[2], lwd = 2, col="lightgrey")
    mini <- c(0,0,0)
    maxi <- c(1,1,1)
    text(C[1], C[2], labels = paste(mini[1]), pos = 2, cex=2)
    text(C[1], C[2], labels = paste(maxi[3]), pos = 4, cex=2)
    text((A + C)[1]/2, (A + C)[2]/2, labels = names[1], cex = 2,
         pos = 2)
    text(A[1], A[2], labels = paste(maxi[1]), pos = 2, cex=2)
    text(A[1], A[2], labels = paste(mini[2]), pos = 1, cex=2)
    text((A + B)[1]/2, (A + B)[2]/2, labels = names[2], cex = 2,
         pos = 1)
    text(B[1], B[2], labels = paste(maxi[2]), pos = 1, cex=2)
    text(B[1], B[2], labels = paste(mini[3]), pos = 4, cex=2)
    text((B + C)[1]/2, (B + C)[2]/2, labels = names[3], cex = 2,
             pos = 4)
    text(-0.7, 0.9, main, cex=3, font=2)
    box()

    arrows(predd[,1], predd[,2], predu[,1], predu[,2], length=0.15, angle=15,
           lwd=2, col=rgb(0,0,0,0.4))

    return(invisible(NULL))
}
