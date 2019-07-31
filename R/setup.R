             
# Load libraries
 library(MASS)
 library(mvtnorm)
 library(fields)
 library(ggplot2)
 library(rethinking)  
 library(sfsmisc)
 library(ash)
 library(reshape)
 library(maptools)
 library(gridExtra)
 library(scales)
 library(msm)
 library(maps)
 library(grid)
 library(xtable)

# Define functions
 dist2<-function(a,b){return(sqrt( (b[2]-a[2])^2   + (b[1]-a[1])^2 ))}  
 
 dist<-function(x2a,x1a,y2a,y1a){return(sqrt((x2a-x1a)^2 + (y2a-y1a)^2))}  
 
 rad2deg <- function(rad) {(rad * 180) / (pi)}
 
 deg2rad <- function(deg) {(deg * pi) / (180)}
 
 ang.dif <- function(x,y) {min((2 * pi) - abs(x - y), abs(x - y))}
 
 lp_dist <-function(a,b,c){ 
                           # Return distance between a point and line segment 
                           t <- -(((a[1]-c[1])*(b[1]-a[1]) + (a[2]-c[2])*(b[2]-a[2])) / ((b[1]-a[1])^2 + (b[2] - a[2])^2 ))
 
                           if(t >0 & t <1){    
                            numer <- abs(  (b[2]-a[2])*c[1] -(b[1]-a[1])*c[2] + b[1]*a[2] - b[2]*a[1])
                            denom <- sqrt( (b[2]-a[2])^2   + (b[1]-a[1])^2 ) 
                            return(numer/denom)  
                             }
        
                           else{
                            d1 <- dist2(a,c)
                            d2 <- dist2(b,c)   
                            return( min(d1,d2))                              
                             }                                                
                           }

ScaleBeta <- function(X){
 a<-0
 b<-pi
 y<-X
 Samp<-50

 y2 <- (y-a)/(b-a)
 y3<-(y2*(Samp - 1) + 0.5)/Samp
 y3}

ang<-function(x2,x1,y2,y1){
  Theta<-seq(0,2*pi,length.out=100)
  DB<-cbind(cos(Theta),sin(Theta))
   r <- dist(x2,x1,y2,y1)
   x0<-(x2-x1)/r
   y0<-(y2-y1)/r

   (atan2(y0,x0) + pi )
   }

################################################################# Set parameters
Thresh <- 50         # Threshold of visual range
Lags<-10             # Lags at which there are effects of encounters on movement, kinda hardcoded---careful with changes
Steps<-150           # Length of simulation
Reps <- 30           # Number of replications

# Adaptive model parameters
Phi0A <- 3.2
Psi0A <- 0
PhiA  <- c(-0.76, -0.65, -0.58, -0.43, -0.29, -0.15, -0.03,0,0,0)
PsiA <- c(0.5, 0.3, 0.25, 0.2, 0.17, 0.15, 0.13, 0.11, 0.09, 0.08)
OmegaA <- 0.4
EtaA <- 2

 # Levy parameters
Phi0L <- 2.8
Psi0L <- 0
PhiL  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PsiL <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
OmegaL <- 1
EtaL <- 2

 # Brownian parameters
Phi0B <- 25
Psi0B <- 0
PhiB  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PsiB <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
OmegaB <- 15
EtaB <- 2

