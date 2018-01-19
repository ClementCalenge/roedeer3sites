\name{triangleImage}
\alias{triangleImage}
\alias{triangleGrid}
\title{
  Comparison of the Habitat Use by the Roe Deer Between two Sites
}
\description{
  The functions presented on this help page have been used to compare
  the habitat use between two sites (figure 5 of the paper).  The
  function \code{triangleImage} can be used to display an image in the
  ecological triangle.  The function \code{triangleGrid} can be used to
  generate such an image.  We describe in the Examples section how we
  used these two functions to generate the figure 5 of the paper.
}
\usage{

triangleImage(pred, gr, names=c("A","B","C"), 
              cuts = c(-0.1, 0.05, 0.15, 0.5, 0.85, 0.95, 1.1),
              col = c("lightgrey", "darkgrey","darkgrey","darkgrey",
                      "darkgrey","black"),
              listlim = NULL,
              lowerleft=c(1,0,0), lowerright=c(0,1,0), top=c(0,0,1))

triangleGrid(ngrid=150)

}
\arguments{
  \item{pred}{ a vector of N values to be plotted }
  \item{gr}{ a N x 2 matrix containing the pairs of Cartesian
    coordinates on the ecological triangle, corresponding to the N
    values stored in \code{pred}.}
  \item{names}{
    The names of the three habitat types.
  }
  \item{cuts}{
    a numeric vector containing the B+1 breaks used to define the colors.
  }
  \item{col}{
    a character vector containing the names of the B colors
    corresponding to the B classes defined by \code{cuts}.
  }  
  \item{listlim}{
    Optionally, a list with two elements named \code{xlim} and
    \code{ylim} describing the x and y limits of the image to be plotted
    on the triangle.
  }
  \item{lowerleft}{
    A vector of length 3 describing the habitat proportions
    corresponding to the lower left corner of the triangle.
  }
  \item{lowerright}{
    A vector of length 3 describing the habitat proportions
    corresponding to the lower right corner of the triangle.
  }
  \item{top}{
    A vector of length 3 describing the habitat proportions
    corresponding to the top corner of the triangle.
  }
  \item{ngrid}{
    An integer value controling the resolution of the grid generated on
    the ecological triangle.
  }
}
\value{
  The function \code{triangleGrid} returns a list with two elements
  named \code{triangle} and \code{proportions} containing respectively a
  matrix containing the pairs of Cartesian coordinates of the grid in
  the triangle and a matrix containing the corresponding vectors of
  three habitat proportions.

  The function \code{triangleImage} returns invisibly an object of class
  \code{spatialPixelsDataFrame} containing the gridded values within the
  limits specified by \code{listlim}, \code{lowerleft},
  \code{lowerright} and \code{top}.
}
\author{
  William Gaudry, Clement Calenge, Sonia Said, Jean-Michel Gaillard
}
\examples{

## In these examples, we describe how we built the figure 5.

######################################################
##
## 1. Load the data and fit the model

## We load the data used for the fit:
data(HS3sites)

## We remove the information concerning the meadows
## (negligible, see the paper)
HS3sites$locs$meadows <- NULL
HS3sites$hr$meadows <- NULL

## Calculates the total number of relocations
HS3sites$N <- apply(HS3sites$locs,1,sum)

## stores the number of animals
HS3sites$J <- nrow(HS3sites$locs)

## keeps the home-range information in another object
availhr <- HS3sites$hr

## For a better mixing, we scale the covariates
HS3sites$hr <- scale(HS3sites$hr)
mo <- attr(HS3sites$hr,"scaled:center")
sc <- attr(HS3sites$hr,"scaled:scale")

## We have described in the example section of the help
## page of the dataset coefficientsModel2 how to fit the model.
## We just load the dataset:
data(coefficientsModel2)

\dontrun{
####################################################
##
## 2. We generate a grid of values in the ecological triangle 

tr <- triangleGrid()
pp <- tr$proportions
gr <- tr$triangle


####################################################
##
## 3. We predict, for each possible habitat availability stored in tr
##    and gr, the posterior distribution of the log-ratios between
##    habitat use in a site and in another

## Remember that we centered and scaled the availability proportions
## to improve mixing of the MCMC. We center and scale the proportions
## for prediction. Also, pp is designed for representation purposes
## (with scrubs as the horisontal axis), whereas d is for calculation:
d <- pp[,c(2,3,1)]
d <- t(apply(d,1,function(x) (x-mo)/sc))

## We bind the 3 chains
coef2 <- do.call(rbind, coefficientsModel2)

## We work with 1000 random vectors of parameters sampled from the
## posterior.
set.seed(98)
sampledvectors <- sample(1:nrow(cor2), 1000)

## For each parameter vector, we predict the mean of log-ratios between
## pairs of sites for all possible available proportions.
## WARNING!!!! Very slow loop!
lpr <- lapply(1:1000, function(samp) {

    ## progress bar
    cat(round(100*samp/1000),"\r")

    ## Prediction of the use in site 1
    i <- sampledvectors[samp]
    site <- rep(1,nrow(d))
    logp1 <- coef2[i,paste("a0f[",site,"]",sep="")] +
        coef2[i,paste("aff[",site,"]",sep="")]*d[,1] +
            coef2[i,paste("afp[",site,"]",sep="")]*d[,2]
    logp2 <- coef2[i,paste("a0p[",site,"]",sep="")] +
        coef2[i,paste("apf[",site,"]",sep="")]*d[,1] +
            coef2[i,paste("app[",site,"]",sep="")]*d[,2]
    p1 <- exp(logp1)/(1+exp(logp1)+exp(logp2))
    p2 <- exp(logp2)/(1+exp(logp1)+exp(logp2))
    p3 <- 1/(1+exp(logp1)+exp(logp2))
    p11 <- p1
    p21 <- p2
    p31 <- p3

    ## Prediction of the use in site 2
    site <- rep(2,nrow(d))
    logp1 <- coef2[i,paste("a0f[",site,"]",sep="")] +
        coef2[i,paste("aff[",site,"]",sep="")]*d[,1] +
            coef2[i,paste("afp[",site,"]",sep="")]*d[,2]
    logp2 <- coef2[i,paste("a0p[",site,"]",sep="")] +
        coef2[i,paste("apf[",site,"]",sep="")]*d[,1] +
    coef2[i,paste("app[",site,"]",sep="")]*d[,2]
    p1 <- exp(logp1)/(1+exp(logp1)+exp(logp2))
    p2 <- exp(logp2)/(1+exp(logp1)+exp(logp2))
    p3 <- 1/(1+exp(logp1)+exp(logp2))
    p12 <- p1
    p22 <- p2
    p32 <- p3

    ## Prediction of the use in site 3
    site <- rep(3,nrow(d))
    logp1 <- coef2[i,paste("a0f[",site,"]",sep="")] +
        coef2[i,paste("aff[",site,"]",sep="")]*d[,1] +
            coef2[i,paste("afp[",site,"]",sep="")]*d[,2]
    logp2 <- coef2[i,paste("a0p[",site,"]",sep="")] +
        coef2[i,paste("apf[",site,"]",sep="")]*d[,1] +
            coef2[i,paste("app[",site,"]",sep="")]*d[,2]
    p1 <- exp(logp1)/(1+exp(logp1)+exp(logp2))
    p2 <- exp(logp2)/(1+exp(logp1)+exp(logp2))
    p3 <- 1/(1+exp(logp1)+exp(logp2))
    p13 <- p1
    p23 <- p2
    p33 <- p3

    ## return the matrix of the predicted mean log-ratios.
    return(cbind(log(p11/p12), log(p21/p22), log(p31/p32),
                 log(p12/p13), log(p22/p23), log(p32/p33),
                 log(p13/p11), log(p23/p21), log(p33/p31)))
})


####################################################
##
## 4. We predict, for each possible habitat availability stored in tr
##    and gr, the probability that the log-ratios between
##    habitat use in a site and in another is greater than 0


gg <- do.call("cbind", lapply(1:ncol(lpr[[1]]), function(i) {
    aa <- do.call("cbind",lapply(1:length(lpr), function(k) {
        lpr[[k]][,i]
    }))
    apply(aa,1,function(x) mean(x>0))
}))



####################################################
##
## 5. We plot the results using triangleImage


## Several elements required for the plot
namesa <- c("Chizé","LPP","TF")
nameshab <- c("Scrubs","pole","CWS")
labels1 <- apply(expand.grid(nameshab,
                             namesa)[,2:1],1,
                 function(x) paste(x, collapse="-"))


## The range of available conditions available in the two sites of a
## pair
availhr <- availhr[,c(3,1,2)] ## we reordered the columns, as we found
## clearer to present the scrubs as the horizontal side
di <- availhr[HS3sites$site==1,]
predd <- t(apply(di,1,ptoxy))
poly1 <- list(xlim=range(predd[,1]), ylim=range(predd[,2]))
di <- availhr[HS3sites$site==2,]
predd <- t(apply(di,1,ptoxy))
poly2 <- list(xlim=range(predd[,1]), ylim=range(predd[,2]))
poly2$xlim <- poly1$xlim
di <- availhr[HS3sites$site==3,]
predd <- t(apply(di,1,ptoxy))
poly3 <- list(xlim=range(predd[,1]), ylim=range(predd[,2]))
poly3$xlim <- poly1$xlim


## preparation of the image layout
mat <- rbind(c(1,2,2,2,2,3,3,3,3,4,4,4,4),
             c(5,6,6,6,6,7,7,7,7,8,8,8,8),
             c(5,6,6,6,6,7,7,7,7,8,8,8,8),
             c(5,6,6,6,6,7,7,7,7,8,8,8,8),
             c(5,6,6,6,6,7,7,7,7,8,8,8,8),
             c(9,10,10,10,10,11,11,11,11,12,12,12,12),
             c(9,10,10,10,10,11,11,11,11,12,12,12,12),
             c(9,10,10,10,10,11,11,11,11,12,12,12,12),
             c(9,10,10,10,10,11,11,11,11,12,12,12,12),
             c(13,14,14,14,14,15,15,15,15,16,16,16,16),
             c(13,14,14,14,14,15,15,15,15,16,16,16,16),
             c(13,14,14,14,14,15,15,15,15,16,16,16,16),
             c(13,14,14,14,14,15,15,15,15,16,16,16,16))
lay <- layout(mat)


## Top row of graphs = titles
par(mar = c(0.1,0.1,0.1,0.1))
plot(0,0, asp=1, ty="n", xlim = c(0,1), ylim = c(0,1), axes=FALSE)
par(mar = c(0.1,0.1,0.1,0.1))
plot(0,0, asp=1, ty="n", xlim = c(0,1), ylim = c(0,1), axes=FALSE)
text(0.5,0.5,"Scrubs", cex=1.5, font=2)
box()
par(mar = c(0.1,0.1,0.1,0.1))
plot(0,0, asp=1, ty="n", xlim = c(0,1), ylim = c(0,1), axes=FALSE)
text(0.5,0.5,"Pole Stage", cex=1.5, font=2)
box()
par(mar = c(0.1,0.1,0.1,0.1))
plot(0,0, asp=1, ty="n", xlim = c(0,1), ylim = c(0,1), axes=FALSE)
text(0.5,0.5,"CWS", cex=1.5, font=2)
box()

## Second row of graphs
par(mar = c(0.1,0.1,0.1,0.1))
plot(0,0, asp=1, ty="n", xlim = c(0,1), ylim = c(0,1), axes=FALSE)
text(0.5,0.5,"Chizé/LPP", cex=1.5, font=2, srt=90)
box()
triangleImage(gg[,1], gr, listlim=poly1, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))
triangleImage(gg[,2], gr, listlim=poly1, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))
triangleImage(gg[,3], gr, listlim=poly1, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))

## Third row of graphs
par(mar = c(0.1,0.1,0.1,0.1))
plot(0,0, asp=1, ty="n", xlim = c(0,1), ylim = c(0,1), axes=FALSE)
text(0.5,0.5,"LPP/TF", cex=1.5, font=2, srt=90)
box()
triangleImage(gg[,4], gr, listlim=poly3, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))
triangleImage(gg[,5], gr, listlim=poly3, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))
triangleImage(gg[,6], gr, listlim=poly3, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))

## Last row of graphs
par(mar = c(0.1,0.1,0.1,0.1))
plot(0,0, asp=1, ty="n", xlim = c(0,1), ylim = c(0,1), axes=FALSE)
text(0.5,0.5,"TF/Chizé", cex=1.5, font=2, srt=90)
box()
triangleImage(gg[,7], gr, listlim=poly1, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))
triangleImage(gg[,8], gr, listlim=poly1, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))
triangleImage(gg[,9], gr, listlim=poly1, names=c("CWS","scrubs","pole"),
              lowerleft=c(1,0,0), lowerright=c(0.3,0.7,0), top=c(0.3,0,0.7))

}

}
\keyword{hplot}
