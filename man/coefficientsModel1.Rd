\name{coefficientsModel1}
\alias{coefficientsModel1}
\docType{data}
\title{
  Model of Identical Habitat use by the Roe Deer in 3 Sites.
}
\description{
  This help page describes how we fitted the model f1, supposing the
  same habitat use by the roe deer in Chize, la Petite Pierre and
  Trois-Fontaines.  The R and JAGS code used to fit the model is
  included in the Examples section of this help page, and the dataset
  \code{coefficientModel1} is an object of class \code{mcmc.list}
  (package rjags) containing the sampled values of the coefficients.
}
\usage{data("coefficientsModel1")}
\format{
  This object is an object of class \code{mcmc.list} containing the
  values of the coefficients sampled for three chains.
}
\details{

  The fitted model was the following (see paper) for the multinomial
  logit of the probability of use of the scrubs:
  
  \deqn{log(P(scrubs)/P(CWS)) = a0f + apf * PoleStageInHomeRange +
    aff * ScrubsInHomeRange + eps1}
  
  Where \code{eps1} is a normal overdispersion residual, and for the
  probability of use of the pole stage:

  \deqn{log(P(pole stage)/P(CWS)) = a0p + app * PoleStageInHomeRange +
    apf * ScrubsInHomeRange}

  Where \code{eps2} is a normal overdispersion residual.
  
}
\source{
  Sonia Said, Centre national d'etude et de recherche appliquee 
  "Cervides-Sangliers", Office national de la chasse et de la faune
  sauvage, Birieux, Ain, France.
}
\examples{

## We load the data used for the fit:
data(HS3sites)

## We remove the information concerning the meadows
## (negligible, see the paper)
HS3sites$locs$meadows <- NULL
HS3sites$hr$meadows <- NULL

## The site is not required for this model
HS3sites$site <- NULL

## Calculates the total number of relocations
HS3sites$N <- apply(HS3sites$locs,1,sum)

## stores the number of animals
HS3sites$J <- nrow(HS3sites$locs)

## For a better mixing, we scale the covariates
HS3sites$hr <- scale(HS3sites$hr)


\dontrun{

## We define the following starting values
init <- list(
  list(a0f=-10,apf=0,aff=0, afp=0,app=0),
  list(a0f=-10,apf=1,aff=1, afp=-1,app=-1),
  list(a0f=-10,apf=-1,aff=1, afp=1,app=-1))


## We write the JAGS model in a file named model1.jags
## in the working directory

cat("model {

  a0f ~ dnorm(0,0.001)
  a0p ~ dnorm(0,0.001)
  aff ~ dnorm(0,0.001)
  afp ~ dnorm(0,0.001)
  apf ~ dnorm(0,0.001)
  app ~ dnorm(0,0.001)
  sig ~ dunif(0,100)

  for (j in 1:J) {

    eps1[j]~dnorm(0,sig)
    eps2[j]~dnorm(0,sig)
    eps3[j]~dnorm(0,sig)

    ep[j,1] <- a0f + aff*hr[j,1] + apf*hr[j,2] + eps1[j]
    ep[j,2] <- a0p + afp*hr[j,1] + app*hr[j,2] + eps2[j]
    p[j,1] <- exp(ep[j,1])/(1+exp(ep[j,1])+exp(ep[j,2]))
    p[j,2] <- exp(ep[j,2])/(1+exp(ep[j,1])+exp(ep[j,2]))
    p[j,3] <- 1/(1+exp(ep[j,1])+exp(ep[j,2]))

    locs[j,]~dmulti(p[j,], N[j])

  }

}
", file = "model1.jags")


## initialization
mo1 <- jags.model("model1.jags", n.adapt=20000, n.chain=3, data=HS3sites, inits=init)

## We draw 500 000 realization of this model (takes a long time!!!)
coefficientsModel1 <- coda.samples(mo1,
                                   variable.names=c("a0f", "apf", "aff", "afp", "a0p",
                                                    "app",  "sig"),
                    n.iter=500000, thin=50)

}

## To avoid waiting a long time for the fit, we have stored the results
## in the dataset coefficientsModel1
data(coefficientsModel1)


}
\keyword{datasets}
