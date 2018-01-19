\name{roedeer3sites-package}
\alias{roedeer3sites-package}
\alias{roedeer3sites}
\docType{package}
\title{
  Habitat Selection by the Roe Deer in three French Study Sites
}
\description{
  
  Companion package to the article by Gaudry et al. on habitat selection
  by the roe deer in three Study sites.

}
\details{
\tabular{ll}{
Package: \tab roedeer3sites\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-20\cr
License: \tab GPL (>= 2)\cr
}

This package contains all the code used to analyse the GPS data
collected on the roe deer at Chize, la Petite Pierre and
Trois-Fontaines. The code is stored in the example section of the help
pages of the objects contained in this package:

\code{HS3Sites} is the dataset analyzed in the paper (i.e. habitat
use and availability)

\code{coefficientsModel1} is a dataset containing the vectors of
parameters sampled in the posterior distribution for the model f1 (same
habitat selection in all sites).  The R and JAGS code used to fit the
model is presented in the example section of the help page of this
dataset (requires the R package rjags).

\code{coefficientsModel2} is a dataset containing the vectors of
parameters sampled in the posterior disttribution for the model f2
(habitat selection different in the three sites). The R and JAGS code
used to fit the model is presented in the example section of the help
page of this dataset.  Note that this section also contains the code
used to fit the model to the dataset after removal of three
individuals suspected to have a strong influence on the parameters, as
well as the comparison between the posterior distribution fit to the
full dataset.

\code{simulateNewData} is the function that was used to check the
goodness of fit of our model by simulating one dataset for each sampled
vector of parameters. The example section of this help page contains the
R code used for this check.

\code{marginalityTriangle} is the function used to plot the
observed marginality vectors on the ecological triangle. The example
section of this help page contains the R code used to draw figure 3 of
the paper.

\code{marginalityDots} is the function used to plot the posterior
distribution of the marginality vectors in the ecological triangle.  The
example section of this help page contains the R code used to draw
figure 4 of the paper.

\code{triangleImage} is the function used to plot an image on the
ecological triangle. The example section of this help page contains the
R code used to draw figure 5 of the paper.



}
\author{
  William Gaudry, Clement Calenge, Sonia Said, Jean-Michel Gaillard

Maintainer: Clement Calenge <clement.calenge@oncfs.gouv.fr>
}
\keyword{ package }
\examples{

## See the help page of the functions

}
