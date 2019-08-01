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

# Simulate tracks from 1 grackle over 30 trips
z <- grackleate(AlphaDist=3.8,AlphaAngle=0,SDDist=1.5,DAngle=2)

# Analyze results of trips for step-size and angle change
m1 <- grackleize(z$X,z$Y)
 
# Plot results over trips
grackletrip(m1)

# Plot bird-specific parameters
gracklepars(m1)
```

And now habitat selection:
```{r}
# Load library and attach data
library(grackleator)  

# Simulate tracks from 1 grackle over 30 trips
z <- grackleate(AlphaDist=3.8,AlphaAngle=0,SDDist=1.5,DAngle=2)

# Bin data on 2D grid
GrackBins<- gracklebinner(z,ab_override=matrix(c(-3000,-3000,3000,3000),nrow=2,ncol=2))

# Model location data
m2 <- gracklenomial(GrackBins)
 
# Plot environmental hotspots
image(matrix(get_posterior_mean(m2,pars="A"), nrow=15,ncol=15))

# Plot bird-specific parameters
grackleations(m2)

# Now create data with temporal correlations
GrackBins2 <- GrackBins
A <- get_posterior_mean(m2,pars="A")
A <- A/sum(A)
B <- 0.8

for(i in 2:30)
GrackBins2[i,] <- rmultinom(1,sum(GrackBins2[i-1,]),A*(1-B) + B*(GrackBins2[i-1,]/sum(GrackBins2[i-1,])))

m3 <- gracklenomial(GrackBins2)
grackleations(m3)

```
