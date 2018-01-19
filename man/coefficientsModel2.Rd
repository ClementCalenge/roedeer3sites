\name{coefficientsModel2}
\alias{coefficientsModel2}
\docType{data}
\title{
  Model of Habitat use by the Roe Deer Different in the 3 Sites.
}
\description{
  This help page describes how we fitted the model f2, supposing a
  differente habitat use by the roe deer in Chize, la Petite Pierre and
  Trois-Fontaines.  The R and JAGS code used to fit the model is
  included in the Examples section of this help page, and the dataset
  \code{coefficientModel2} is an object of class \code{mcmc.list}
  (package rjags) containing the sampled values of the coefficients.
}
\usage{
data("coefficientsModel2")
}
\format{
  This object is an object of class \code{mcmc.list} containing the
  values of the coefficients sampled for three chains.
}
\details{

  The fitted model was the following (see paper) for the multinomial
  logit of the probability of use of the scrubs:
  
  \deqn{log(P(scrubs)/P(CWS)) = a0f[site] + apf[site] * PoleStageInHomeRange +
    aff[site] * ScrubsInHomeRange + eps1}
  
  Where \code{eps1} is a normal overdispersion residual, and for the
  probability of use of the pole stage: 

  \deqn{log(P(pole stage)/P(CWS)) = a0p[site] + app[site] * PoleStageInHomeRange +
    apf[site] * ScrubsInHomeRange + eps2}

  Where \code{eps2} is a normal overdispersion residual, and \code{site}
  is an integer value taking the value 1 (Chize), 2 (La Petite Pierre)
  or 3 (Trois-Fontaines).
}
\source{
  Sonia Said, Centre national d'etude et de recherche appliquee 
  "Cervides-Sangliers", Office national de la chasse et de la faune
  sauvage, Birieux, Ain, France.
}
\examples{

#########################################
##
## 1. Model fit

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

## For a better mixing, we scale the covariates
HS3sites$hr <- scale(HS3sites$hr)


\dontrun{
## starting values
init <- list(
  list(a0f=rep(-10,3),apf=rep(0,3),aff=rep(0,3), afp=rep(0,3),app=rep(0,3)),
  list(a0f=rep(-10,3),apf=rep(1,3),aff=rep(1,3), afp=rep(-1,3),app=rep(-1,3)),
  list(a0f=rep(-10,3),apf=rep(-1,3),aff=rep(1,3), afp=rep(1,3),app=rep(-1,3)))

## We store the JAGS code for the model in the file model2.jags
cat("model {

  for (s in 1:3) {
    a0f[s] ~ dnorm(0,0.001)
    a0p[s] ~ dnorm(0,0.001)
    aff[s] ~ dnorm(0,0.001)
    afp[s] ~ dnorm(0,0.001)
    apf[s] ~ dnorm(0,0.001)
    app[s] ~ dnorm(0,0.001)
 }
  sig ~ dunif(0,100)

  for (j in 1:J) {

    eps1[j]~dnorm(0,sig)
    eps2[j]~dnorm(0,sig)
    eps3[j]~dnorm(0,sig)

    ep[j,1] <- a0f[site[j]] + aff[site[j]]*hr[j,1] + apf[site[j]]*hr[j,2] + eps1[j]
    ep[j,2] <- a0p[site[j]] + afp[site[j]]*hr[j,1] + app[site[j]]*hr[j,2] + eps2[j]
    p[j,1] <- exp(ep[j,1])/(1+exp(ep[j,1])+exp(ep[j,2]))
    p[j,2] <- exp(ep[j,2])/(1+exp(ep[j,1])+exp(ep[j,2]))
    p[j,3] <- 1/(1+exp(ep[j,1])+exp(ep[j,2]))

    locs[j,]~dmulti(p[j,],N[j])

  }

}
", file = "model2.jags")



################################################################
##
## IF YOU WANT TO CHECK HOW THE FIT WORKS, COPY AND PASTE THE
## FOLLOWING CODE, BUT NOTE THAT IT CAN TAKE SEVERAL HOURS TO EXECUTE!!!
## IF YOU DO NOT WANT TO WAIT, THE RESULTING SAMPLED COEFFICIENTS ARE
## STORED IN THE DATASET coefficientsModel2
## THAT IS LOADED FURTHER BELOW.

## initialization and burn-in (very slow)
mo2 <- jags.model("model2.jags", n.adapt=20000, n.chain=3,
                  data=HS3sites, inits=init)

## Sample 2000000 coefficients from the posterior (warning! very slow)
coefficientsModel2 <- coda.samples(mo2,
                                   variable.names=c("a0f", "apf", "aff", "afp", "a0p",
                                                    "app", "sig"),
                                   n.iter=2000000, thin=50)

}

## TO AVOID WAITING A LONG TIME FOR THE FIT, WE HAVE STORED THE RESULTS
## IN THE DATASET coefficientsModel2. LOAD THESE RESULTS HERE:
data(coefficientsModel2)


##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##
##
##
##  As we note in the paper, there were three animals characterized by a
##  very particular composition of their home range: the individual X15
##  in Chize was characterized by a very large availability of scrubs,
##  the individual X24 in LPP was characterized by a very low
##  availability of scrubs, and the individual X64 in TF was
##  characterized by both an availability of scrubs nearly equal to zero
##  and a large availability of pole stage.
##
##
##  2. We fitted the model f2
##     after removal of these three individuals


## Removal of the three individuals
dj <- HS3sites
dj$locs <- dj$locs[!row.names(dj$hr)\%in\%c("X15_ch","X24_lpp","X64_tf"),]
dj$N <- dj$N[!row.names(dj$hr)\%in\%c("X15_ch","X24_lpp","X64_tf")]
dj$site <- dj$site[!row.names(dj$hr)\%in\%c("X15_ch","X24_lpp","X64_tf")]
dj$hr <- dj$hr[!row.names(dj$hr)\%in\%c("X15_ch","X24_lpp","X64_tf"),]
dj$J <- nrow(dj$locs)

\dontrun{

## Fit the model
## WARNING!!!!!!!!!!!!!!!!!!!!!!
## VERY SLOW!!!!!!!!!!!!!!!!!!!
mo2a <- jags.model("model2.jags", n.adapt=20000, n.chain=3,
                  data=dj, inits=init)

## We draw a small sample, just to compare the distributions
## WARNING!!!!!!!!!!!!!!!!!!!!!!
## VERY SLOW!!!!!!!!!!!!!!!!!!!
coefficientsModel2Without3Deer <- coda.samples(mo2a,
                                               variable.names=c("a0f", "apf", "aff", "afp", "a0p",
                                                                "app", "sig"),
                                               n.iter=20000, thin=1)

#########################################
##
## 3. Plot the posterior distributions of the coefficients for the full dataset

## Note that because we have scaled the variables prior to the fit (to
## improve mixing), we have to back-transform the coefficients.

## centering and scale of the explanatory variables
sc <- attr(HS3sites$hr,"scaled:scale")
ce <- attr(HS3sites$hr,"scaled:center")

## We bind the three chains together
cor2 <- do.call(rbind,coefficientsModel2)

## prepare the plot
par(mfrow = c(3,2))

#### Plot of the different coefficients:
## a0f
a0f <- cor2[,c("a0f[1]","a0f[2]","a0f[3]")] -
       cor2[,c("aff[1]","aff[2]","aff[3]")]*ce[1]/sc[1] -
       cor2[,c("apf[1]","apf[2]","apf[3]")]*ce[2]/sc[2]
colnames(a0f) <- c("Chize", "LPP","TF")
boxplot(a0f, xlab="Site", ylab="a0f", col="grey",
        main="a0f", ylim=c(-15,3))
abline(h=0)

## a0p
a0p <- cor2[,c("a0p[1]","a0p[2]","a0p[3]")] -
       cor2[,c("afp[1]","afp[2]","afp[3]")]*ce[1]/sc[1] -
       cor2[,c("app[1]","app[2]","app[3]")]*ce[2]/sc[2]
colnames(a0p) <- c("Chize", "LPP","TF")
boxplot(a0p, xlab="Site", ylab="a0p", col="grey",
        main="a0p", ylim=c(-15,3))
abline(h=0)

## aff
aff <- cor2[,c("aff[1]","aff[2]","aff[3]")]
aff <- aff/sc[1]
colnames(aff) <- c("Chize", "LPP","TF")
boxplot(aff, xlab="Site", ylab="aff", col="grey",
        main="aff")
abline(h=0)

## afp
afp <- cor2[,c("afp[1]","afp[2]","afp[3]")]
afp <- afp/sc[1]
colnames(afp) <- c("Chize", "LPP","TF")
boxplot(afp, xlab="Site", ylab="afp", col="grey",
        main="afp")
abline(h=0)

## apf
apf <- cor2[,c("apf[1]","apf[2]","apf[3]")]
apf <- apf/sc[2]
colnames(apf) <- c("Chize", "LPP","TF")
boxplot(apf, xlab="Site", ylab="apf", col="grey",
        main="apf")
abline(h=0)

## app
app <- cor2[,c("app[1]","app[2]","app[3]")]
app <- app/sc[2]
colnames(app) <- c("Chize", "LPP","TF")
boxplot(app, xlab="Site", ylab="app", col="grey",
        main="app")
abline(h=0)




#########################################
##
## 4. Plot the posterior distributions of the coefficients for the
##    reduced dataset

## We bind the three chains together
cor2 <- do.call(rbind,coefficientsModel2Without3Deer)

## prepare a new plot for comparison
x11()
par(mfrow = c(3,2))

#### Plot of the different coefficients:
## a0f
a0f <- cor2[,c("a0f[1]","a0f[2]","a0f[3]")] -
       cor2[,c("aff[1]","aff[2]","aff[3]")]*ce[1]/sc[1] -
       cor2[,c("apf[1]","apf[2]","apf[3]")]*ce[2]/sc[2]
colnames(a0f) <- c("Chize", "LPP","TF")
boxplot(a0f, xlab="Site", ylab="a0f", col="grey",
        main="a0f", ylim=c(-15,3))
abline(h=0)

## a0p
a0p <- cor2[,c("a0p[1]","a0p[2]","a0p[3]")] -
       cor2[,c("afp[1]","afp[2]","afp[3]")]*ce[1]/sc[1] -
       cor2[,c("app[1]","app[2]","app[3]")]*ce[2]/sc[2]
colnames(a0p) <- c("Chize", "LPP","TF")
boxplot(a0p, xlab="Site", ylab="a0p", col="grey",
        main="a0p", ylim=c(-15,3))
abline(h=0)

## aff
aff <- cor2[,c("aff[1]","aff[2]","aff[3]")]
aff <- aff/sc[1]
colnames(aff) <- c("Chize", "LPP","TF")
boxplot(aff, xlab="Site", ylab="aff", col="grey",
        main="aff")
abline(h=0)

## afp
afp <- cor2[,c("afp[1]","afp[2]","afp[3]")]
afp <- afp/sc[1]
colnames(afp) <- c("Chize", "LPP","TF")
boxplot(afp, xlab="Site", ylab="afp", col="grey",
        main="afp")
abline(h=0)

## apf
apf <- cor2[,c("apf[1]","apf[2]","apf[3]")]
apf <- apf/sc[2]
colnames(apf) <- c("Chize", "LPP","TF")
boxplot(apf, xlab="Site", ylab="apf", col="grey",
        main="apf")
abline(h=0)

## app
app <- cor2[,c("app[1]","app[2]","app[3]")]
app <- app/sc[2]
colnames(app) <- c("Chize", "LPP","TF")
boxplot(app, xlab="Site", ylab="app", col="grey",
        main="app")
abline(h=0)


#################################
##                             ##
## The two plots are similar.  ##
##                             ##
#################################



}


}
\keyword{datasets}
