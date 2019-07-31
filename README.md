grackleator
========
This is an R package for modeling bird movement.

Install by running on R:
```{r}
library(devtools)
install_github("Ctross/grackleator")
```

Some quick examples:

```{r}
# Load library and attach data
library(grackleator)  

# Simulate tracks from 1 grackle over 30 trips
z <- grackleate(AlphaDist=3.8,AlphaAngle=0,SDDist=1.5,DAngle=2)

# Analyze results of trips for step-size and angle change
 m <- grackleize(z$X,z$Y)
 
# Plot results over trips
grackletrip(m)

# Plot bird-specific parameters
gracklepars(m)
```
