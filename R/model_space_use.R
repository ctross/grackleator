model_space_use = function(GrackBins){
 model_dat_2 = list(
  Trips=dim(GrackBins)[1],
  Bins=dim(GrackBins)[2],
  GrackBins=GrackBins)

model_code_2 = '
data{
	int Trips;
	int Bins;
	array[Trips,Bins] int GrackBins;
}

parameters{
 simplex[Bins] A;
 real<lower=0, upper=1> B;
}

model{
 vector[Bins] C;
 A ~ dirichlet(rep_vector(1,Bins));
 B ~ beta(1,1);

 for(i in 2:Trips){
 	C = to_vector(GrackBins[i-1])/sum(GrackBins[i-1]);
	GrackBins[i] ~ multinomial( A*(1-B) + B*C  );
    }
}
'

 model_2 = cmdstanr::cmdstan_model(write_stan_file(model_code_2))

 fit_2 = model_2$sample(
        data = model_dat_2,
        chains = 1,
        parallel_chains = 1,
        refresh = 1,
        iter_warmup = 1000,
        iter_sampling = 1500,
        max_treedepth = 14,
        adapt_delta = 0.92
        )


 m2 = brms:::read_csv_as_stanfit(fit_2$output_files())
 #m1 = rstan::read_stan_csv(fit_1$output_files()) 
 return(m2)
}
