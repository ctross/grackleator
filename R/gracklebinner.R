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
