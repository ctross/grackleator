
grackleate <- function(AlphaDist=2.8,AlphaAngle=0,BetaDist=rep(0,10),BetaAngle=rep(0,10),SDDist=1,DAngle=2,Thresh=50, Lags=10, steps=150, seed=123456, Reps=30,N_Patches=13){

################################################################################# Set up Patches of Food                                                
X_Mushrooms<-vector("list",N_Patches)                                           # Location of prey
Y_Mushrooms<-vector("list",N_Patches)                                           # 

set.seed(seed)                                                                  # Reset Seed

N_PerPatch<-rpois(N_Patches, 200)                                               # Create some prey
for(i in 1:N_Patches){
X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100
Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100
}

X_Mushrooms_per_step<-vector("list",steps)                                      # Location of prey
Y_Mushrooms_per_step<-vector("list",steps)                                      #

for(i in 1:steps){
 X_Mushrooms_per_step[[i]]<-X_Mushrooms
 Y_Mushrooms_per_step[[i]]<-Y_Mushrooms
}                                                                 
                                                                                     
################################################################################ Start Model
Store.X <- matrix(NA,ncol=Reps,nrow=steps-1)
Store.Y <- matrix(NA,ncol=Reps,nrow=steps-1)

for(q in 1:Reps){                                                               # Run simulation several times
set.seed(seed)                                                                  # Reset seed

N_PerPatch<-rpois(N_Patches, 200)                                               # Choose number of items per patch

for(i in 1:N_Patches){
X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100  # Make some patches of prey
Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100  #
}

X_Mushrooms_per_step<-vector("list",steps)                                      # Location of prey
Y_Mushrooms_per_step<-vector("list",steps)                                      #

for(i in 1:steps){
 X_Mushrooms_per_step[[i]]<-X_Mushrooms
 Y_Mushrooms_per_step[[i]]<-Y_Mushrooms
}

loc.x<-c()                                                                      # Location of forager
loc.y<-c()                                                                      #

speed<-c()                                                                      # Speed or step size
heading<-c()                                                                    # Absolute heading
d.heading<-c()                                                                  # Heading change

Hits<-c()                                                                       # Binary vector of encounters

loc.x[1:Lags]<-rep(0,Lags)                                                      # Intialize vectors
loc.y[1:Lags]<-rep(0,Lags)                                                      #
heading[1:Lags]<-rep(0,Lags)                                                    #
speed[1:Lags]<-rep(0,Lags)                                                      #
Hits[1:Lags]<-rep(0,Lags)                                                       #

plot(loc.x,loc.y,typ="l",ylim=c(-3500,3500),xlim=c(-3500,3500))                 # Plot prey items
 for(i in 1:N_Patches){                                                         #
  points(X_Mushrooms[[i]],Y_Mushrooms[[i]], col="red",pch=".")                  #
   }                                                                            #

set.seed(q*103)

################################################################################ Now model forager movement

for(s in (Lags+1): (steps-1)){                                                  
 X_Mushrooms <- X_Mushrooms_per_step[[s]]
 Y_Mushrooms <- Y_Mushrooms_per_step[[s]]

 PredDist <- AlphaDist;                                                         # First calculate mean prediction conditional on encounters
for(k in 1:Lags){                                                               #    
 PredDist <- PredDist + BetaDist[k]*ifelse(Hits[s-k]>0,1,0);                                  #
       }                                                                        #

 R<- exp(rnorm(1,PredDist,SDDist))                                              # Then simulate a step distance. 
    
 PredAngle <- AlphaAngle;                                                       # Again calculate mean prediction conditional on encounters
for(k in 1:Lags){                                                               #    
 PredAngle <- PredAngle + BetaAngle[k]*ifelse(Hits[s-k]>0,1,0);                              #
        }                                                                       #
                
 Theta<- rbeta(1,inv_logit(PredAngle)*DAngle,                                   # And then simulate a directional change
          (1-inv_logit(PredAngle))*DAngle)*180*ifelse(runif(1,0,1)>0.5,1,-1)    # The 180 shifts from the unit to the maximum absolute distance
                                                                                # The ifelse just chooses a random direction
                                                                             
 heading[s]<-(heading[s-1]+Theta)%%360                                          # Store new heading. Note that the %% is the mod operation to wrap around if needed
 d.heading[s] <- abs(Theta)/180                                                 # Also store just the delta heading
 speed[s] <- R                                                                  # And the speed slash step size
   
 ynew <- R * sin(deg2rad(heading[s]))                                           # Now convert polar to Cartesian, to get the offset for a new x and y pair
 xnew <- R * cos(deg2rad(heading[s]))                                           #
   
 loc.x[s]<-loc.x[s-1]+xnew                                                      # Make the new x and y pair
 loc.y[s]<-loc.y[s-1]+ynew                                                      #

############################################################################### Now check for an encounter 
 Scrap2<-c()
 for(i in 1:N_Patches){                                                         # For each patch
  Scrap<-rep(NA,length(X_Mushrooms[[i]]))
 for(j in 1:length(X_Mushrooms[[i]])){                                          # For each mushroom in patch
 Scrap[j]<- dist(loc.x[s],X_Mushrooms[[i]][j],loc.y[s],Y_Mushrooms[[i]][j]);    # Calculate the distance from the forager to the mushroom
 if(Scrap[j]<Thresh){                                                           # If the forager is closer than the visual radius slash encounter threshold
 points(X_Mushrooms[[i]][j],Y_Mushrooms[[i]][j], col="blue",pch=20)             # Plot a hit

       for(allgone in s: (steps-1)){
       X_Mushrooms_per_step[[allgone]][[i]][j]<-99999                           # If this run is for destructive foraging
       X_Mushrooms_per_step[[allgone]][[i]][j]<-99999                           # disappear food for all future timesteps
      }
 }}
 
 Scrap2[i]<- ifelse(sum(ifelse(Scrap<Thresh,1,0))>0,sum(ifelse(Scrap<Thresh,1,0)),0)      # Check for hits, also can replace sum(ifelse(Scrap<Thresh,1,0)) with 1
 }
 
 Hits[s]<-ifelse(sum(Scrap2)==0,0,sum(Scrap2))                                            # If there is a hit, then set encounters to 1
  
 lines(loc.x,loc.y,ylim=c(-2500,2500),xlim=c(-2500,2500))                                 # Plot updates to the foragers path
}
 Store.X[,q] <- loc.x
 Store.Y[,q] <- loc.y
 }


 return(list(X=Store.X[(Lags+1):length(loc.x),],Y=Store.Y[(Lags+1):length(loc.y),]))
 }
