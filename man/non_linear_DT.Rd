\name{non_linear_DT}
\alias{non_linear_DT}
\title{non_linear_DT}
\usage{
  non_linear_DT(x, y, Error, yl, xl, yyl, xxl, Fyx, Ival,
    dir, show = FALSE)
}
\arguments{
  \item{x}{numeric vectors.}

  \item{y}{numeric vectors.}

  \item{Error}{numeric vectors.}

  \item{yl}{numeric vectors with two values.}

  \item{xl}{numeric vectors with two values.}

  \item{yyl}{numeric vectors with two values.}

  \item{xyl}{numeric vectors with two values.}

  \item{Fyx}{a expression; default is 1.}

  \item{Ival}{a list.}

  \item{dir}{a path; default is 'D:/'.}

  \item{show}{logical.}
}
\value{
  character a string; default is 'finished!'.
}
\description{
  A special curve.
}
\details{
  Here more details
}
\examples{
# Example usage of 'non_linear_DT'

###############################################################
## Non Linear Modelling #######################################

library(MASS)
library(plotrix)
dir<-"C:/Users/Pablo/Documents/CIP/Marc Sporleder/Abril 2015"

# dataset of the mortality

x<-c(10.13,14.89,10.1,15.1,20.4,20.4,25.4,25.4,27.8,10,10,10,15.4,13.2,17,17,17,20,20,20,24,24,28)
y<-c(0.030140063,0.065729989,0.030064807,0.07621477,0.121007833,0.123823267,0.181627026,0.185092422,0.192492132,0.041961615,0.044020493,0.042663992,0.072331179,0.068030442,0.088575499,0.090419077,0.098401424,0.137325673,0.114383322,0.103695143,0.149165329,0.157630751,0.257046057)
#yl<-c(0,max(y)+0.01) # se puede editar
y<-log(1/y)
yl<-c(0,0.3) # se puede editar
xl<-c(0,max(x)+5) # se puede editar
yyl<-0.05 # se puede editar
xxl<-5 # se puede editar
Error<-c(0.001838716,0.005752903,0.003142897,0.008049071,0.011666794,0.012147017,0.019794179,0.020029991,0.017113088,0.003042291,0.003866312,0.003400574,0.005510225,0.005401576,0.00826834,0.010117433,0.013288156,0.014397771,0.017462622,0.00783597,0.014747385,0.012056713,0.023403715)

Fyx=as.formula(paste("y ~ log(1/(b*(x-Tb))^2)")) ## ecuacion
Ival=list(b=-0.0151 ,Tb=-2.8397)

non_linear_DT(x,y,Error,yl,xl,yyl,xxl,Fyx,Ival,dir)
}
\author{
  Pablo Carhuapoma Ramos
}

