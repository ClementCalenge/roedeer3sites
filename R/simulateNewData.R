simulateNewData <-
function(coefMatrix, availMatrix, Nlocs, site)
{
    ## for every simulated coefficient
    licor2 <- lapply(1:nrow(coefMatrix), function(i) {

        ## progress bar
        cat(round(100*i/nrow(coefMatrix)),"\r")

        ## on récupère le vecteur de coefficients généré à l'itération i
        coef <- coefMatrix[i,]

        ## overdispersion residuals
        epsilon1 <- rnorm(nrow(availMatrix), 0, sqrt(1/coef["sig"]))
        epsilon2 <- rnorm(nrow(availMatrix), 0, sqrt(1/coef["sig"]))

        ## Estimated probability for each habitat type:
        ## multinomial logit:
        logp1 <- coef[paste("a0f[",site,"]",sep="")] +
            coef[paste("aff[",site,"]",sep="")]*availMatrix[,1] +
                coef[paste("apf[",site,"]",sep="")]*availMatrix[,2] + epsilon1

        logp2 <- coef[paste("a0p[",site,"]",sep="")] +
            coef[paste("afp[",site,"]",sep="")]*availMatrix[,1] +
                coef[paste("app[",site,"]",sep="")]*availMatrix[,2] + epsilon2

        ## corresponding probabilities
        p1 <- exp(logp1)/(1+exp(logp1)+exp(logp2))
        p2 <- exp(logp2)/(1+exp(logp1)+exp(logp2))
        p3 <- 1/(1+exp(logp1)+exp(logp2))

        ## We simulate a number of relocations in each habitat type by drawing
        ## in a multinomial distribution for each animal
        n <- t(do.call(cbind,
                       lapply(1:length(p1), function(r) {
                           rmultinom(1, Nlocs[r], c(p1[r],p2[r],p3[r]))
                       })))

        ## result matrix
        return(n)
    })
    return(licor2)
}
