
grackleize <- function(StoreX,StoreY) {                                          
############################################################ Prepare Data
 Trip <- dim(StoreX)[2]  
 MaxTicks<-dim(StoreX)[1]
    
 Distance <-matrix(NA,ncol=max(Trip),nrow=MaxTicks) 
 Ang <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 AngDiff <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)

 N<-c()
 for(j in 1:max(Trip)){
 X <- StoreX[,j]
 Y <- StoreY[,j]
 N[j] <- length(X)
  
 Dist<-c()  
 ang1<-c()
 angdif<-c()
  
 for(i in 2:N[j]){
 Dist[i]<- dist(X[i],X[i-1],Y[i],Y[i-1]);
 ang1[i]<- ang(X[i],X[i-1],Y[i],Y[i-1])
 if(i>3){
 angdif[i]<- ang.dif(ang1[i],ang1[i-1])
         }}    
         
 Distance[1:N[j],j]<-Dist
 Ang[1:N[j],j]<-ang1       
 AngDiff[1:N[j],j]<-angdif
 }

 for(i in 1:MaxTicks){
 for(j in 1:max(Trip)){
  Distance[i,j] <- ifelse(Distance[i,j]==0,
                          runif(1,0,min(Distance[which(Distance != 0)],na.rm=TRUE)),
                          Distance[i,j])
      }}
      

 Distance[is.na(Distance)] <- 999999

 EmptyAngle <-  fitdistr( ScaleBeta(c(na.omit(c(AngDiff)))),start=list(shape1=1,shape2=1),"beta")$estimate

 for(i in 1:MaxTicks){
 for(j in 1:max(Trip)){
  AngDiff[i,j] <- ifelse(is.nan(AngDiff[i,j]), rbeta(1,EmptyAngle[1], EmptyAngle[2])*pi,AngDiff[i,j])
      }}
      
 AngDiff <- ScaleBeta(AngDiff)
 AngDiff[is.na(AngDiff)] <- 999999

 AngDiff <- AngDiff[c(-1,-2,-3),]
 Distance <- Distance[c(-1,-2,-3),]
 N <- N-3
 MaxTicks<-MaxTicks-3


########################################################## Extract data for Stan    
model_dat=list(
Distance=Distance, 
AngDiff=AngDiff,
N=N,
MaxTrip=max(Trip),
MaxTicks=MaxTicks)

################################################################# Fit Stan Model
model_code_1 <- '
data{
int MaxTrip;
int MaxTicks;
int N[MaxTrip];

real Distance[MaxTicks,MaxTrip];
real AngDiff[MaxTicks,MaxTrip];
}

parameters {
 real MuD_M;
 real MuA_M;

 real<lower=0> DispD_M;
 real<lower=0> DispA_M;

 real MuD_D;
 real MuA_D;

 real<lower=0> DispD_D;
 real<lower=0> DispA_D;

 vector[MaxTrip] AlphaDist_Raw;
 vector[MaxTrip] SDDist_Raw; 

 vector[MaxTrip] AlphaAngle_Raw;
 vector[MaxTrip] DAngle_Raw; 
}

transformed parameters{
 vector[MaxTrip] AlphaDist;
 vector[MaxTrip] SDDist; 

 vector[MaxTrip] AlphaAngle;
 vector[MaxTrip] DAngle; 

 AlphaDist = MuD_M + DispD_M*AlphaDist_Raw;
 SDDist = MuD_D + DispD_D*SDDist_Raw;

 AlphaAngle = MuA_M + DispA_M*AlphaAngle_Raw;
 DAngle = MuA_D + DispA_D*DAngle_Raw;  

}

model{
  MuD_M~normal(0,5); 
  MuA_M~normal(0,5); 

  DispD_M~cauchy(0,1); 
  DispA_M~cauchy(0,1); 

  MuD_D~normal(0,5); 
  MuA_D~normal(0,5); 

  DispD_D~cauchy(0,1); 
  DispA_D~cauchy(0,1); 

  AlphaDist_Raw~normal(0,1); 
  SDDist_Raw~normal(0,1); 

  AlphaAngle_Raw~normal(0,1); 
  DAngle_Raw~normal(0,1); 

for(j in 1:MaxTrip){
{

 vector[N[j]] PredAngle;
 vector[N[j]] PredDist;
 vector[N[j]] Dist;
 vector[N[j]] AngleDifference;
  
 for(i in 1:N[j]){
  PredDist[i] = AlphaDist[j]; 
  } 
        
 for(i in 1:N[j]){
  PredAngle[i] = AlphaAngle[j]; 
  }       
              
 for(i in 1:N[j]){            
  Dist[i] = Distance[i,j];              
  AngleDifference[i] = AngDiff[i,j];
  }

 Dist ~ lognormal(PredDist,exp(SDDist[j]));
 AngleDifference ~ beta(inv_logit(PredAngle)*exp(DAngle[j]), (1-inv_logit(PredAngle))*exp(DAngle[j]));
 }}    

}

'

 m1 <- stan( model_code=model_code_1, data=model_dat,refresh=1,chains=1, control = list(adapt_delta = 0.9, max_treedepth = 15))

 return(m1)
}
