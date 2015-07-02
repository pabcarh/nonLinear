\name{non_linear_DT}
\alias{non_linear_DT}
\title{non_linear_DT}
\usage{
  non_linear_DT(x, y, Error = NULL, yl, xl, yyl, xxl, ylab,
    xlab = expression(bold(paste("temperature (", degree, "C)"))),
    Fyx, Ival, SEmodel = NULL, DFF = NULL, dir,
    show = FALSE, variable = "Other", modelling = FALSE,
    yypos = NULL, yylab = NULL, width = 12, height = 10,
    cex.lab = 1, Cex.Axis = 1, TITLE = NULL, Symbol = NULL,
    FeatSymbol = NULL, Text = NULL, FeatText = NULL,
    Limit1 = NULL, Limit2 = NULL)
}
\arguments{
  \item{x}{numeric vector.}

  \item{y}{numeric vector.}

  \item{Error}{numeric vector; default is NULL.}

  \item{yl}{numeric vector with two values.}

  \item{xl}{numeric vector with two values.}

  \item{yyl}{numeric vector with two values.}

  \item{xyl}{numeric vector with two values.}

  \item{Fyx}{a expression; default is 1.}

  \item{Ival}{a list; default is NULL.}

  \item{SEmodel}{numeric value; default is NULL.}

  \item{dff}{numeric value.}

  \item{dir}{a path.}

  \item{show}{logical; default is FALSE.}

  \item{ylab}{character a string}

  \item{xlab}{character a string; default is 'temperature
  (°C)'.}

  \item{variable}{character a string; default is 'Other'
  and you can choise 'DT' and "Fec"}

  \item{modelling}{logical; default is FALSE.}

  \item{yypos}{numeric vector; default is NULL.}

  \item{yylab}{numeric vector; default is NULL.}

  \item{width}{the width of the device; default is 12.}

  \item{height}{the height of the device; default is 10.}

  \item{cex.lab}{the height of the device; default is 1.}

  \item{TITLE}{character a string; default is NULL}
}
\value{
  character a string; default is 'finished!'.
}
\description{
  A special curve using specific parameters and also its
  modelling
}
\details{
  If no estimated values of the parameters of the model
  must enable "modeling = TRUE", then include the initial
  values
}
\examples{
###############################################################
## Non Linear Modelling for the Insect Biology ################
###############################################################
###############################################################

library(MASS)
library(plotrix)
library(nonLinear)
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

############################################################################################################
# DEVELOPMENT RATE #########################################################################################

##########################################
# Egg
x<-c(10.13,14.89,10.1,15.1,20.4,20.4,25.4,25.4,27.8,10,10,10,15.4,13.2,17,17,17,20,20,20,24,24,28)
y<-c(0.030140063,0.065729989,0.030064807,0.07621477,0.121007833,0.123823267,0.181627026,0.185092422,0.192492132,0.041961615,0.044020493,0.042663992,0.072331179,0.068030442,0.088575499,0.090419077,0.098401424,0.137325673,0.114383322,0.103695143,0.149165329,0.157630751,0.257046057)
y<-log(1/y) # only for development rate and senescense using transformation log(1/y)

# Plot arguments
yl<-c(0,0.3) # edit to DT
xl<-c(0,30) # edit to DT
yyl<-0.05 # edit to fecundity
xxl<-5 # edit to DT
ylab<-"development rate (1/day)" # edit

# Egg
Error<-c(0.001838716,0.005752903,0.003142897,0.008049071,0.011666794,0.012147017,0.019794179,0.020029991,0.017113088,0.003042291,0.003866312,0.003400574,0.005510225,0.005401576,0.00826834,0.010117433,0.013288156,0.014397771,0.017462622,0.00783597,0.014747385,0.012056713,0.023403715)

# Equation and parameters
Fyx=as.formula(paste("y ~ log(1/(b*(x-Tb))^2)")) ## ecuacion log(1/Ecuacion)  Egg
Ival=list(b=-0.0151 ,Tb=-2.8397) # Egg

non_linear_DT(x,y,Error,yl,xl,yyl,xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              SEmodel=0.120078689,dir=dir,variable="DT",height=8,cex.lab=1.8,Cex.Axis=1.2,
              Symbol="A",FeatSymbol=c(-0.22,-0.05,2.5),Text="Eggs",FeatText=c(0.1,0,2),Limit2=29.7) # development rate of "Egg"


#########################################
# Larvas
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10,10,15,20,20,25,25,28,28,10,10,10,10,15,15,17,20,20,20,20,24,28)
y<-c(0.009,0.009,0.021,0.035,0.034,0.045,0.047,0.046,0.045,0.010,0.011,0.011,0.011,0.019,0.019,0.027,0.037,0.034,0.035,0.034,0.043,0.050)
y<-log(1/y) # only for development rate and senescense using transformation log(1/y)

# Plot arguments
yl<-c(0,0.06) # edit to DT
xl<-c(0,30) # edit to DT
yyl<-0.01 # edit to fecundity
xxl<-5 # edit to DT
ylab<-"development rate (1/day)" # edit

# Larve
Error<-c(0.00040,0.00057,0.00112,0.00232,0.00215,0.00412,0.00369,0.00436,0.00342,0.00072,0.00066,0.00074,0.00067,0.00100,0.00109,0.00132,0.00194,0.00180,0.00176,0.00190,0.00234,0.00314)

# Equation and parameters
Fyx=as.formula(paste("y ~ log(1/(rm*exp(-0.5*((x-Topt)/Troh)^2)))")) ## ecuacion log(1/Ecuacion)  Larve
Ival=list(rm=0.04763 ,Topt=28.58079, Troh=10.49975) # Larve

non_linear_DT(x,y,Error,yl,xl,yyl,xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              SEmodel=0.061596277,dir=dir,variable="DT",height=8,cex.lab=1.8,
              Symbol="B",FeatSymbol=c(-0.22,-0.05,2.5),Text="Larvas",FeatText=c(0.1,0,2)) # development rate of "Egg"

#########################################
# Pupas
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10,15,20,25,28,10,15,17,20,24,25,28)
y<-c(0.013,0.039,0.060,0.089,0.098,0.016,0.028,0.041,0.052,0.075,0.077,0.091)
y<-log(1/y) # only for development rate and senescense using transformation log(1/y)

# Plot arguments
yl<-c(0,0.14) # edit to DT
xl<-c(0,30) # edit to DT
yyl<-0.02 # edit to fecundity
xxl<-5 # edit to DT
ylab<-"development rate (1/day)" # edit

# Pupae
Error<-c(0.00034,0.00157,0.00355,0.00476,0.00583,0.00057,0.00104,0.00172,0.00178,0.00365,0.00535,0.00466)

# Equation and parameters
Fyx=as.formula(paste("y ~ log(1/(rm*exp(-0.5*((x-Topt)/Troh)^2)))")) ## ecuacion log(1/Ecuacion)  Pupae
Ival=list(rm=0.09893 ,Topt=31.80226, Troh=11.10037) # Pupae

non_linear_DT(x,y,Error,yl,xl,yyl,xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              SEmodel=0.10942986,dir=dir,variable="DT",height=8,cex.lab=1.8,
              Symbol="C",FeatSymbol=c(-0.22,-0.05,2.5),Text="Pupas",FeatText=c(0.1,0,2)) # development rate of "Egg"


############################################################################################################
# SENESCENCE ###############################################################################################

##########################################
# Female
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10,17,17,20,20,24,28,15.1,20.4,25.4,25.4,27.8,27.8,10,15)
y<-c(0.026,0.047,0.045,0.058,0.053,0.052,0.073,0.043,0.043,0.102,0.089,0.127,0.139,0.029,0.036)

y<-log(1/y) # only for development rate and senescense using transformation log(1/y)

yl<-c(0,0.3) # edit to DT
xl<-c(0,35) # edit to DT
yyl<-0.05 # edit to fecundity
xxl<-5 # edit to DT
ylab<-"senescence (1/day)" # edit

# Female
Error<-c(0.00085,0.00590,0.00499,0.00822,0.00618,0.00430,0.00779,0.00197,0.00266,0.01194,0.00756,0.01698,0.01157,0.00211,0.00359)

Fyx=as.formula(paste("y ~ log(b1+b2*x)")) ##  Female
Ival=list(b1=47.6594009280213, b2=-1.40813862718278) # Female

non_linear_DT(x,y,Error,yl,xl,yyl,xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              SEmodel=0.23632013752966,DFF=28,dir=dir,variable="DT",height=8,cex.lab=1.8,
              Symbol="A",FeatSymbol=c(-0.22,-0.05,2.5),Text="Female",FeatText=c(0.1,0,2),Limit2=c(31.5,32.4,30)) # development rate of "Egg"

##########################################
# Male
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10,17,17,20,20,24,28,15.1,20.4,25.4,25.4,27.8,27.8,10,15)
y<-c(0.028,0.048,0.048,0.058,0.056,0.057,0.081,0.040,0.044,0.114,0.110,0.108,0.250,0.031,0.040)

y<-log(1/y) # only for development rate and senescense using transformation log(1/y)

yl<-c(0,0.3) # edit to DT
xl<-c(0,35) # edit to DT
yyl<-0.05 # edit to fecundity
xxl<-5 # edit to DT
ylab<-"senescence (1/day)" # edit

# Male
Error<-c(0.00128,0.00615,0.00537,0.00772,0.00653,0.00492,0.00853,0.00182,0.00318,0.01254,0.00907,0.01556,0.02226,0.00225,0.00401)

Fyx=as.formula(paste("y ~ log(b1+b2*x)-0.0441")) ## ecuacion log(1/Ecuacion)  Male
Ival=list(b1=47.6594009280213, b2=-1.40813862718278) # Male

non_linear_DT(x,y,Error,yl,xl,yyl,xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              SEmodel=0.23632013752966,DFF=28,dir=dir,variable="DT",height=8,cex.lab=1.8,
              Symbol="B",FeatSymbol=c(-0.22,-0.05,2.5),Text="Male",FeatText=c(0.1,0,2),Limit2=c(31.5,32.4,30)) # development rate of "Egg"



############################################################################################################
# MORTALITY #########################################################################################

##########################################
# Egg
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10.1,14.9,10.1,15.1,20.4,20.4,25.4,25.4,27.8,10,10,10,15.4,13.2,17,17,17,20,20,20,24,24,28)
y<-c(0.041,0.034,0.400,0.195,0.117,0.066,0.114,0.101,0.255,0.194,0.129,0.183,0.171,0.111,0.118,0.142,0.134,0.189,0.180,0.157,0.272,0.290,0.375)

yl<-c(0,1) # edit
xl<-c(0,35) # edit
yyl<-0.1 # edit
xxl<-5 # edit
ylab<-"mortality" # edit

# Egg
Error<-c(0.002066,0.001718,0.015492,0.008748,0.005671,0.003409,0.005749,0.005040,0.007934,0.005018,0.005818,0.006279,0.004278,0.003583,0.004860,0.007963,0.009935,0.008210,0.012591,0.003760,0.008388,0.006030,0.008404)

# non linear equation
Fyx=as.formula(paste("y ~ exp(log(rmin)+cc*((Topt-x)^2))")) ## ecuacion log(1/Ecuacion)  Egg
Ival=list(rmin=0.131404, cc=0.0070, Topt=17.093862) # Egg

non_linear_DT(x,y,Error=Error,yl=yl,xl=xl,yyl=yyl,xxl=xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              dir=dir,variable="Other",,height=8,cex.lab=1.8,
              Symbol="A",FeatSymbol=c(-0.22,-0.05,2.5),Text="Eggs",FeatText=c(0.05,0,2),Limit1=c(1,1,1),Limit2=c(33,33,33))

#########################################
# Larve
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10.1,10.1,15.1,20.4,20.4,25.4,25.4,27.8,27.8,10,10,10,10,15,15,17,20,20,20,20,24,28)
y<-c(0.590,0.605,0.318,0.681,0.554,0.855,0.633,0.817,0.620,0.385,0.539,0.522,0.594,0.310,0.258,0.229,0.246,0.256,0.309,0.337,0.573,0.587)

yl<-c(0,1) # edit
xl<-c(0,35) # edit
yyl<-0.1 # edit
xxl<-5 # edit
ylab<-"mortality" # edit

# Larve
Error<-c(0.01889,0.01901,0.01311,0.02111,0.02004,0.01722,0.02267,0.02098,0.01911,0.02523,0.02085,0.02645,0.02238,0.01342,0.01481,0.00710,0.00955,0.00943,0.01096,0.01583,0.01248,0.01778)

Fyx=as.formula(paste("y ~ 1-1/(1+exp(log(rmin)+cc*((1/(sqrt(Topt))-1/(sqrt(x)))^2)))")) ## ecuacion log(1/Ecuacion)  Larve
Ival=list(rmin=0.380422018178331, cc=383.273821136479, Topt=14.6033534594175) # Larve

non_linear_DT(x,y,Error=Error,yl=yl,xl=xl,yyl=yyl,xxl=xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              dir=dir,variable="Other",,height=8,cex.lab=1.8,
              Symbol="B",FeatSymbol=c(-0.22,-0.05,2.5),Text="Larvas",FeatText=c(0.05,0,2),Limit1=c(0,9,0),Limit2=c(35,28,35))

#########################################
# Pupae
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10.10,15.10,20.40,25.40,27.80,10.00,15.00,17.00,20.00,24.00,25.00,28.00)
y<-c(0.323,0.155,0.424,0.314,0.615,0.274,0.163,0.124,0.212,0.230,0.280,0.400)

yl<-c(0,1) # edit
xl<-c(0,35) # edit
yyl<-0.1 # edit
xxl<-5 # edit
ylab<-"mortality" # edit

# Pupae
Error<-c(0.01327,0.00712,0.02506,0.02200,0.02538,0.01127,0.00835,0.00855,0.00762,0.01489,0.02376,0.02043)


# non linear equation
Fyx=as.formula(paste("y ~ exp(log(rmin)+cc*((sqrt(Topt)-sqrt(x))^2))")) ## ecuacion log(1/Ecuacion)  Pupae
Ival=list(rmin=0.18505025674978 ,cc=0.616669022415545,Topt=16.2539949567149) # Pupae

non_linear_DT(x,y,Error=Error,yl=yl,xl=xl,yyl=yyl,xxl=xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,
              dir=dir,variable="Other",,height=8,cex.lab=1.8,
              Symbol="C",FeatSymbol=c(-0.22,-0.05,2.5),Text="Pupas",FeatText=c(0.05,0,2),Limit1=c(6,6.5,6),Limit2=c(32,31,32))


############################################################################################################
# OVIPOSITION TIME #########################################################################################

dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10,15,20,25,28,10,15,17,17,20,20,24,28)
y<-c(0.067,0.167,0.091,0.151,0.214,0.069,0.066,0.095,0.077,0.094,0.133,0.102,0.123)

y<-log(1/y)

yl<-c(0,0.3) # edit to DT
xl<-c(0,35) # edit to DT
yyl<-0.05 # edit to fecundity
xxl<-5 # edit to DT
ylab<-"median oviposition rate" # edit

# Oviposition time
Error<-c(0.00267,0.00727,0.00404,0.00693,0.02094,0.00300,0.00299,0.00439,0.00368,0.00452,0.00593,0.00483,0.00646)

# non linear equation
Fyx=as.formula(paste("y ~ log(b1+b2*x)")) ##  Oviposition time
Ival=list(b1=17.7518847738705, b2=-0.409568939949337) # Oviposition time

non_linear_DT(x,y,Error=Error,yl=yl,xl=xl,yyl=yyl,xxl=xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,SEmodel=0.23632013752966,DFF=28,
              dir=dir,variable="DT",height=8,cex.lab=1.8,
              Symbol="A",FeatSymbol=c(-0.22,-0.05,2.5),Text="Female",FeatText=c(0.05,0,2),Limit2=c(34,34,30))

############################################################################################################
# Fecundity ################################################################################################
dir<-"D:/_BK-D/Pablo/cip/Marc/Abril 2015"

x<-c(10.0,10.1,15.0,15.1,17.0,17.0,20.0,20.0,20.4,24.0,25.4,27.8,28.0)
y<-c(85.2,45.9,102.5,236.7,150.3,81.9,112.9,149.1,187.8,44.9,94.7,9.9,39.8)

y<-log(y) # only for fecundity transformation log(y)

xl<-c(0,35)
yl<-c(0,7) # edit to fecundity
xxl<-5
yyl<-0.5
yypos<-log(10^(0:3)) # edit to fecundity
yylab<-10^(0:3) # edit to fecundity
ylab<-"fecundity / female"

# non linear equation
Fyx=as.formula(paste("y ~ rmax+cc*(Top-x)^2")) ## ecuacion log(Ecuacion) Fecundity
Ival=list(rmax=4.99887937569638, cc=-0.0166398011904257, Top=17.2944263203708) # Fecundity

#non_linear_DT(x,y,yl=yl,xl=xl,yyl=yyl,xxl=xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,dir=dir,variable="Other",modelling=TRUE) # Fecundity option 4
#non_linear_DT(x,y,yl=yl,xl=xl,yyl=yyl,xxl=xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,SEmodel=0.534779270479086,dir=dir,variable="Other",yypos=yypos,yylab=yylab) # Fecundity option 5
non_linear_DT(x,y,yl=yl,xl=xl,yyl=yyl,xxl=xxl,ylab=ylab,Fyx=Fyx,Ival=Ival,SEmodel=0.534779270479086,
              dir=dir,variable="Other",height=8,cex.lab=1.5,
              Symbol="B",FeatSymbol=c(-0.22,-0.05,2.5),Text="Female",FeatText=c(0.05,0,2),Limit1=c(1,1,2.5),Limit2=c(34,34,32))
}
\author{
  Pablo Carhuapoma Ramos
}

