\name{marginalityDots}
\alias{marginalityDots}
\title{
  Plot the posterior distribution of the mean marginality vectors
}
\description{
  This function allows to plot the posterior distribution of the
  marginality vector of a given animal (used for the figure 4 of the
  paper).  We describe in the example section how we calculated the
  posterior distribution of the mean marginality vectors from the
  posterior distribution of the coefficients of the model f2.  
}
\usage{
marginalityDots(avail, useMatrix, main = "")
}
\arguments{
  \item{avail}{
    A vector of length 3 containing the proportion of the home range of
    the animal covered by the three habitat types (columns).
  }
  \item{useMatrix}{
    A data.frame containing the proportion of the time of animals
    spent in the three habitat types, predicted by.
  }
  \item{main}{
    a character string to be used as a title for the plot.
  }
}
\author{
  William Gaudry, Clement Calenge, Sonia Said, Jean-Michel Gaillard
}
\examples{

## We describe here how we built the figure 4 of our paper

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

## We have described in the example section of the help
## page of the dataset coefficientsModel2 how to fit the model.
## We just load the dataset:
data(coefficientsModel2)




####################################################
##
## 2. For every possible observed home range, we calculate
##    the posterior distribution of the probability of use

## bind the 3 MCMC chains
cor2 <- do.call(rbind, coefficientsModel2)

\dontrun{

## For every vector of coefficients simulated by the MCMC
## we calculate the mean probability of use predicted for
## each animal and each habitat type.
## Warning: VERY SLOW!!!!

r <- lapply(1:nrow(cor2), function(x) {

        ## progress bar
        cat(round(100*x/nrow(cor2)),"%\r")

        ## the vector of coefficients simulated at step x
        coef <- cor2[x,]

        ## Calculation of the multinomial logit of the probability of use
        site <- HS3sites$site
        logp1 <- coef[paste("a0f[",site,"]",sep="")] +
            coef[paste("aff[",site,"]",sep="")]*HS3sites$hr[,1] +
                coef[paste("afp[",site,"]",sep="")]*HS3sites$hr[,2]
        logp2 <- coef[paste("a0p[",site,"]",sep="")]+
            coef[paste("apf[",site,"]",sep="")]*HS3sites$hr[,1] +
                coef[paste("app[",site,"]",sep="")]*HS3sites$hr[,2]

        ## back-transform to probabilities
        p1 <- exp(logp1)/(1+exp(logp1)+exp(logp2))
        p2 <- exp(logp2)/(1+exp(logp1)+exp(logp2))
        p3 <- 1/(1+exp(logp1)+exp(logp2))

        ## Results
        return(cbind(p1,p2,p3))
})



####################################################
##
## 3. We select randomly 8 "representative" roe deer
##    in each site (see the paper for an explanation.

## For reproducibility
set.seed(801)

## for each site
la <- lapply(1:3, function(i) {

    ## transforms, for each animal, the available
    ## proportion into Cartesian coordinates of the animal
    ## in the ecological triangle (see ?ptoxy for further detail on this
    ## operation)
    predd <- t(apply(availhr[HS3sites$site==i,],1,ptoxy))


    ## Calculate the distance matrix between animals based on these
    ## coordinates, hierarchical clustering on this distance matrix
    ## using the Ward's method, and cut the tree into 8 classes
    si <- cutree(hclust(dist(predd), meth="ward.D"), 8)

    ## sample 1 animal in each group:
    sapply(split(si,si), function(x) names(x)[sample.int(length(x),1)])
})


####################################################
##
## 4. And finally, we plotted the posterior distribution of the
##    marginality for each one of these animals


x11(width=4, height=8)
par(mfrow = c(8,3))

## for each one of the 8 deer
tmp <- sapply(1:8, function(i) {

    ## for each site
    sapply(1:3, function(s) {

        ## Builds a data.frame containing the MCMC simulations (rows) of
        ## the probability of use of each habitat (columns) by the
        ## animal i of the site s, based on the list built at step 2.
        u <- do.call(rbind,lapply(r, function(x) {
                 x[which(row.names(HS3sites$hr)==la[[s]][i]),]
        })) 

        ## gets the vector of available proportions for the animal i of
        ## the site s
        di <- unlist(availhr[which(row.names(HS3sites$hr)==la[[s]][i]),])

        ## plots the posterior distribution.
        ## Note that we found clearer to define the scrubs as
        ## the lower side of the triangle on figure 3 of the paper, so that we
        ## reordered the columns.  We do the same in this example:
        marginalityDots(di[c(3,1,2)], u[,c(3,1,2)],
                        main=la[[s]][i])
    })
})


}

}
\keyword{hplot}
