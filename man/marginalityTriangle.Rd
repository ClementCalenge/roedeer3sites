\name{marginalityTriangle}
\alias{marginalityTriangle}
\alias{ptoxy}
\alias{xytop}
\title{
  Plot Marginality Vectors in the Ecological Triangle
}
\description{
  The function marginalityTriangle plots a set of marginality vectors in
  the ecological triangle (used for the figure 3 of the paper).

  The function \code{ptoxy} transforms a vector of length 3 containing
  the proportion of 3 habitat types into a vector of two Cartesian
  coordinates on the triangle.  Conversely, the function \code{xytop}
  transforms a vector of length 2 containing a set of two Cartesian
  coordinates on the triangle into a vector of length 3 containing the
  proportions of 3 habitat types (see examples).
  
}
\usage{

marginalityTriangle(availMatrix, useMatrix,
                    names = colnames(availMatrix),
                    main = "")

ptoxy(p)

xytop(xy)

}
\arguments{
  \item{availMatrix}{
    A data.frame containing the proportion of the home range of animals
    (rows) covered by the three habitat types (columns).
  }
  \item{useMatrix}{
    A data.frame containing the proportion of the time of animals
    (rows) spent in the three habitat types (columns).
  }
  \item{names}{
    The names of the three habitat types.
  }
  \item{main}{
    a character string to be used as a title for the plot.
  }
  \item{p}{
    a vector of length 3 containing a set of 3 proportions summing to 1.
  }
  \item{xy}{
    a vector of length 2 containing a set of 2 Cartesian coordinates on
    the Ecological Triangle.
  }
}
\author{
  William Gaudry, Clement Calenge, Sonia Said, Jean-Michel Gaillard
}
\examples{

## Just to illustrate the use of the function marginalityTriangle
data(HS3sites)

## We remove the information concerning the meadows
## (negligible, see the paper)
HS3sites$locs$meadows <- NULL
HS3sites$hr$meadows <- NULL

## Note that we found clearer to define the scrubs as
## the lower side of the triangle on figure 3 of the paper, so that we
## reordered the columns.  We do the same in this example:
locs <- HS3sites$locs[,c(3,1,2)]
hr <- HS3sites$hr[,c(3,1,2)]

## calculation of the proportion of use
locs <- t(apply(locs,1,function(x) x/sum(x)))

\dontrun{
## preparation of the plot
par(mfrow=c(2,2), cex=0.7)

## Chize (do not pay attention to the warnings)
marginalityTriangle(hr[HS3sites$site==1,], locs[HS3sites$site==1,],
                    main="(A)")
## LPP
marginalityTriangle(hr[HS3sites$site==2,], locs[HS3sites$site==2,],
                    main="(B)")

## TF
marginalityTriangle(hr[HS3sites$site==3,], locs[HS3sites$site==3,],
                    main="(C)")
}

## All the animals pooled:
marginalityTriangle(hr, locs)

## Just to illustrate ptoxy, place a point
## corresponding to the proportions
## CWS = 0.7, scrub=0.2, poleStage=0.1
pt <- ptoxy(c(0.7,0.2,0.1))
points(pt[1],pt[2], col="blue", cex=2, lwd=4)

## note that xytop does the converse operation
xytop(pt)

}
\keyword{hplot}
