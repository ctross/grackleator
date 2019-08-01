grackleation <- function(GrackBins){
model_dat_2=list(
Trips=dim(GrackBins)[1],
Bins=dim(GrackBins)[2],
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

 return(m2)
}
