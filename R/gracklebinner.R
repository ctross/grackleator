gracklebinner <- function(tracks,nbin=c(15,15)){

Trips <- dim(tracks$X)[2]
GrackBins <- matrix(NA,nrow=Trips,ncol=nbin[1]*nbin[2])
bins<-bin2(cbind(c(z$X),c(z$Y)),nbin=nbin)

for( i in 1:Trips){
GrackBins[i,] <- c(bin2(cbind(z$X[,i],z$Y[,i]),nbin=nbin,ab=bins$ab)$nc)
 }

return(GrackBins)
}
