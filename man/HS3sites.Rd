\name{HS3sites}
\alias{HS3sites}
\docType{data}
\title{
  Data Used to Model Habitat Use by the Roe Deer in Three French Study Sites
}
\description{
  This dataset is a list containing the habitat use and availability by
  62 roe deer monitored in three study sites.
}
\usage{data("HS3sites")}
\format{
  This dataset is a list with three components: (i) a data.frame named
  \code{locs} containing the number of GPS relocations of the animal in
  row found in the habitat type in column, (ii) a data.frame named
  \code{hr} containing the proportion of the home range of the animal in
  row covered by the habitat type in column, (iii) an integer vector
  named \code{site} containing the index of the site in which the
  animals in the data.frames \code{locs} and \code{hr} are monitored
  (1=Chize, 2=la Petite Pierre, 3=Trois-Fontaines). 
}
\source{
  Sonia Said, Centre national d'etude et de recherche appliquee 
  "Cervides-Sangliers", Office national de la chasse et de la faune
  sauvage, Birieux, Ain, France.
}
\examples{
data(HS3sites)
str(HS3sites)

## see the help pages of the datasets coefficientsModel1 and
## coefficientsModel2 for the modeling of this dataset

}
\keyword{datasets}
