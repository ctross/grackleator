gracklebinner <- function(tracks,nbin=c(15,15),ab_override=NA){

Trips <- dim(tracks$X)[2]
GrackBins <- matrix(NA,nrow=Trips,ncol=nbin[1]*nbin[2])
bins<-bin2(cbind(c(z$X),c(z$Y)),nbin=nbin)

if(is.na(ab_override)){
for( i in 1:Trips){
GrackBins[i,] <- c(bin2(cbind(z$X[,i],z$Y[,i]),nbin=nbin,ab=bins$ab)$nc)
 }
 } else{
for( i in 1:Trips){
GrackBins[i,] <- c(bin2(cbind(z$X[,i],z$Y[,i]),nbin=nbin,ab=ab_override)$nc)
 }
 }

return(GrackBins)
}

GrackBins<- gracklebinner(z,ab_override=matrix(c(-3000,-3000,3000,3000),nrow=2,ncol=2))

model_dat_2=list(
Trips=30,
Bins=15*15,
GrackBins=GrackBins)

model_code_2 <- '
data{
	int Trips;
	int Bins;
	int GrackBins [Trips,Bins];
}

parameters{
 simplex[Bins] A;
 real<lower=0,upper=1> B;
}

model{
 vector[Bins] C;
 A ~ dirichlet(rep_vector(1,Bins));
 B ~ beta(1,1);

 for( i in 2:Trips){
 	C = to_vector(GrackBins[i-1])/sum(GrackBins[i-1]);
	GrackBins[i] ~ multinomial( A*(1-B) + B*C  );
    }
}
'
 m2 <- stan( model_code=model_code_2, data=model_dat_2,refresh=1,chains=1, control = list(adapt_delta = 0.9, max_treedepth = 13))

A <- get_posterior_mean(m2,pars="A")
B <- 0.9

 qq<-get_posterior_mean(m2,pars="A")
image(matrix(qq, nrow=15,ncol=15))


GrackBins2 <- GrackBins
A <- get_posterior_mean(m2,pars="A")
A <- A/sum(A)
B <- 0.8

for(i in 2:30)
GrackBins2[i,] <- rmultinom(1,sum(GrackBins2[i-1,]),A*(1-B) + B*(GrackBins2[i-1,]/sum(GrackBins2[i-1,])))


 image(matrix(GrackBins2[1,], nrow=15,ncol=15))


matrix(c(-3000,-3000,3000,3000),nrow=2,ncol=2)

model_dat_2=list(
Trips=30,
Bins=15*15,
GrackBins=GrackBins2)
 m2 <- stan( model_code=model_code_2, data=model_dat_2,refresh=1,chains=1, control = list(adapt_delta = 0.9, max_treedepth = 13))





