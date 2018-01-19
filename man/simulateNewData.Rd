\name{simulateNewData}
\alias{simulateNewData}
\title{
  Simulate New Datasets from MCMC iterations
}
\description{
  
  This function allows, for each MCMC iteration, to simulate a
  hypothetical replication of the dataset using the model; in other
  words, for each simulated vector of parameters, it simulates a number
  of relocations in each habitat type for each monitored animal.  Such
  simulated values can be used to compare the observed number of
  relocations for each roe deer with the statistical distribution of
  simulated numbers of relocations.  THIS FUNCTION CAN ONLY BE USED WITH
  THE RESULTS OF THE FIT OF MODEL f2 !!!!! i.e. using the dataset
  \code{coefficientsModel2}.

}
\usage{
simulateNewData(coefMatrix, availMatrix, Nlocs, site)
}
\arguments{
  \item{coefMatrix}{
    the matrix containing the simulated vectors of parameters,
    concatenated by rows (i.e. each row is a simulated vector of
    parameter).
  }
  \item{availMatrix}{
    A matrix containing the proportion of the 3 habitat types (columns)
    for each monitored animal (row)
  }
  \item{Nlocs}{
    A vector containing the number of relocations for each animal.
  }
  \item{site}{
    A vector containing the site corresponding to each animal.
  }
}
\value{
  The result is a list with B elements (B being the number of MCMC
  iterations, i.e. the number of rows in \code{coefMatrix}), each
  element being a matrix with 3 columns (the 3 habitat types) and N rows
  (the N animals), containing the simulated number of relocations.
}
\author{
    William Gaudry, Clement Calenge, Sonia Said, Jean-Michel Gaillard
}
\examples{

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


######################################################
##
## 2. Simulate new datasets


## binds the three MCMC chains by row
cor2 <- do.call(rbind, coefficientsModel2)

\dontrun{

## Simulate new datasets: warning! very slow!!
nd <- simulateNewData(cor2, HS3sites$hr, HS3sites$N, HS3sites$site)

## For every number of observed relocations
## identify whether it falls in the 95% credible interval
## WARNING: can also be very slow!
deds <- matrix(0, nrow=nrow(HS3sites$hr), ncol=3)
for (i in 1:nrow(deds)) {
    for (j in 1:ncol(deds)) {

        ## vector of simulated number of relocations
        vec <- sapply(nd, function(x) x[i,j])

        ## The 95% credible interval
        qu <- quantile(vec, c(0.025,0.975))

        ## does the observed number fall in the CI?
        deds[i,j] <- as.numeric((HS3sites$locs[i,j]>=qu[1]&HS3sites$locs[i,j]<=qu[2]))
    }
}

## Finally, deds contains 1 if the corresponding number of relocations
## is in the CI. The proportion of CI containing the true value is:
sum(deds)/length(as.vector(deds))



}


}
\keyword{datagen}
