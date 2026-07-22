grackleator
========
This is an R package for modeling bird movement.

Install by running on R:
```{r}
library(devtools)
install_github("Ctross/grackleator")
```

Some quick examples:

First, lets look at movement dynamics:

```{r}
# Load library and attach data
library(grackleator)
library(rstan)
library(cmdstanr)

# Simulate tracks from 1 grackle over 30 trips
z = grackleate(AlphaDist=3.8, AlphaAngle=0, SDDist=1.5, DAngle=2)

# Analyze results of trips for step-size and angle change
m1 = model_movement(z$X, z$Y)
 
# Plot results over trips
extract_movement_parameters(m1)

# Plot bird-specific parameters
extract_summary_parameters(m1)
```

And now habitat selection:
```{r}
# Load library and attach data
library(grackleator)  

# Simulate tracks from 1 grackle over 30 trips
z = grackleate(AlphaDist=3.8, AlphaAngle=0, SDDist=1.5, DAngle=2)

# Bin data on 2D grid
grackle_bins = bin_movement_tracks(matrix_to_list(z), ab_override=matrix(c(-3000,-3000,3000,3000), nrow=2, ncol=2))

# Model location data
m2 = model_space_use(grackle_bins)
 
# Plot environmental hotspots
image(matrix(colSums(grackle_bins), nrow=15, ncol=15)) # Overall
image(matrix(grackle_bins[1,], nrow=15, ncol=15)) # Day 1
image(matrix(grackle_bins[2,], nrow=15, ncol=15)) # Day 2
image(matrix(grackle_bins[3,], nrow=15, ncol=15)) # Day 3
image(matrix(get_posterior_mean(m2,pars="A"), nrow=15, ncol=15)) # Overall

# Plot bird-specific parameters
extract_autocorrelation_parameter(m2)

# Now create data with temporal correlations
grackle_bins2 = grackle_bins
A = get_posterior_mean(m2, pars="A")
A = A/sum(A)
B = 0.8

for(i in 2:30)
grackle_bins2[i,] = rmultinom(1,sum(grackle_bins2[i-1,]),A*(1-B) + B*(grackle_bins2[i-1,]/sum(grackle_bins2[i-1,])))

# Analyze the new data
m3 = model_space_use(grackle_bins2)

# Plot environmental hotspots
image(matrix(colSums(grackle_bins2), nrow=15, ncol=15)) # Overall
image(matrix(grackle_bins2[1,], nrow=15, ncol=15)) # Day 1
image(matrix(grackle_bins2[2,], nrow=15, ncol=15)) # Day 2
image(matrix(grackle_bins2[3,], nrow=15, ncol=15)) # Day 3

image(matrix(get_posterior_mean(m3, pars="A"), nrow=15, ncol=15)) # Post

# Plot bird-specific parameters
extract_autocorrelation_parameter(m3)

```
