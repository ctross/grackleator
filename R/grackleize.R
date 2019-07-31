
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
M1<<-m1
############################################################## Plot main results
# Make caterpillar plot

 m1a<-rstan::extract(m1,pars="AlphaAngle")
 sample_eff<-apply(m1a$AlphaAngle,2,quantile,probs=c(0.05,0.5,0.95))
 df_angle<-data.frame(Trip=c(1:max(Trip)),Group="Heading Change",Group2="Mean",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 m1d<-rstan::extract(m1,pars="AlphaDist")
 sample_eff<-apply(m1d$AlphaDist,2,quantile,probs=c(0.05,0.5,0.95))
 df_dist<-data.frame(Trip=c(1:max(Trip)),Group="Step-Size",Group2="Mean",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 df_all1<-rbind(df_angle,df_dist)

  m1a<-rstan::extract(m1,pars="DAngle")
 sample_eff<-apply(exp(m1a$DAngle),2,quantile,probs=c(0.05,0.5,0.95))
 df_angle<-data.frame(Trip=c(1:max(Trip)),Group="Heading Change",Group2="Dispersion",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 m1d<-rstan::extract(m1,pars="SDDist")
 sample_eff<-apply(exp(m1d$SDDist),2,quantile,probs=c(0.05,0.5,0.95))
 df_dist<-data.frame(Trip=c(1:max(Trip)),Group="Step-Size",Group2="Dispersion",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 df_all2<-rbind(df_angle,df_dist)

 df_all<-rbind(df_all1,df_all2)
windows()
 ggplot(df_all,aes(x=Trip,y=Median))+geom_point()+
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(Group2~Group,scales="free")+
 labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
 scale_x_continuous(breaks= pretty_breaks())


MuD_M<-data.frame(Group="Step-Size",Group2="Mean",Group3="Mean",Value=rstan::extract(m1,pars="MuD_M")$MuD_M) # Mean Step-Size over Days
DispD_M<-data.frame(Group="Step-Size",Group2="Mean",Group3="Dispersion",Value=rstan::extract(m1,pars="DispD_M")$DispD_M) # Dispersion in Mean Step-Size over days

MuD_D <-data.frame(Group="Step-Size",Group2="Dispersion",Group3="Mean",Value=rstan::extract(m1,pars="MuD_D")$MuD_D)#  Mean Dispersion in Step-Size 
DispD_D <-data.frame(Group="Step-Size",Group2="Dispersion",Group3="Dispersion",Value=rstan::extract(m1,pars="DispD_D")$DispD_D)# Dispersion in Dispersion in Step-Size 

MuA_M <-data.frame(Group="Heading Change",Group2="Mean",Group3="Mean",Value=rstan::extract(m1,pars="MuA_M")$MuA_M)# Mean Angle Change over Days
DispA_M <-data.frame(Group="Heading Change",Group2="Mean",Group3="Dispersion",Value=rstan::extract(m1,pars="DispA_M")$DispA_M)# Dispersion in Mean Angle Change over days

MuA_D <-data.frame(Group="Heading Change",Group2="Dispersion",Group3="Mean",Value=rstan::extract(m1,pars="MuA_D")$MuA_D)#  Mean Dispersion in Angle Change 
DispA_D <-data.frame(Group="Heading Change",Group2="Dispersion",Group3="Dispersion",Value=rstan::extract(m1,pars="DispA_D")$DispA_D)# Dispersion in Dispersion in Angle Change

df_allx<-rbind(MuD_M,DispD_M,MuD_D,DispD_D,MuA_M,DispA_M,MuA_D,DispA_D)
windows()
 ggplot(df_allx, aes(x=Value)) + geom_density(aes(group=Group3, colour=Group3, fill=Group3), alpha=0.3)   + facet_wrap(Group2~Group,scales="free")+
 labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) 
}
