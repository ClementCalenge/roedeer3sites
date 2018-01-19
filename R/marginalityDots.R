marginalityDots <-
function(avail, useMatrix, main="")
{

    ##  On convertit les probas d'utilisations en coordonnées sur le simplex
    predu <- t(apply(useMatrix,1,ptoxy))
    predd <- ptoxy(avail)

    ## Des éléments pompés à la fonction triangle.plot
    ## nécessaire pour la mise en forme du triangle
    A <- ptoxy(c(1,0,0))
    B <- ptoxy(c(0,1,0))
    C <- ptoxy(c(0,0,1))
    to <- rbind(A,B,C)

    ## Représentation graphique
    par(mar = c(0.1,0.1,0.1,0.1))
    se <- c(0, 0.2, 0.4, 0.5, 0.6, 0.8, 1)
    plot(to[,1], to[,2], asp=1, ty="n", ylim = c(-0.6,1.1), axes=FALSE, main="")

    segments(A[1], A[2], B[1], B[2], lwd = 2, col="lightgrey")
    segments(C[1], C[2], B[1], B[2], lwd = 2, col="lightgrey")
    segments(A[1], A[2], C[1], C[2], lwd = 2, col="lightgrey")
    mini <- c(0,0,0)
    maxi <- c(1,1,1)
    box()
    points(predu[,1], predu[,2], pch=16, col=rgb(0.1,0.1,0.1,0.01), cex=0.5)
    points(predd[1], predd[2], pch=21, bg="yellow", col="black", cex=2)
    points(mean(predu[,1]), mean(predu[,2]), pch=21, bg="yellow", col="black", cex=2)
    points(mean(predu[,1]), mean(predu[,2]), pch=3, cex=2)
    text(0.75,0.6, main)

    return(invisible(NULL))
}
