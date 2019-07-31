gracklepars <- function(m1){
MuD_M<-data.frame(Group="Step-Size",Group2="Mean",Group3="Mean",Value=rstan::extract(m1,pars="MuD_M")$MuD_M) # Mean Step-Size over Days
DispD_M<-data.frame(Group="Step-Size",Group2="Mean",Group3="Dispersion",Value=rstan::extract(m1,pars="DispD_M")$DispD_M) # Dispersion in Mean Step-Size over days

MuD_D <-data.frame(Group="Step-Size",Group2="Dispersion",Group3="Mean",Value=rstan::extract(m1,pars="MuD_D")$MuD_D)#  Mean Dispersion in Step-Size 
DispD_D <-data.frame(Group="Step-Size",Group2="Dispersion",Group3="Dispersion",Value=rstan::extract(m1,pars="DispD_D")$DispD_D)# Dispersion in Dispersion in Step-Size 

MuA_M <-data.frame(Group="Heading Change",Group2="Mean",Group3="Mean",Value=rstan::extract(m1,pars="MuA_M")$MuA_M)# Mean Angle Change over Days
DispA_M <-data.frame(Group="Heading Change",Group2="Mean",Group3="Dispersion",Value=rstan::extract(m1,pars="DispA_M")$DispA_M)# Dispersion in Mean Angle Change over days

MuA_D <-data.frame(Group="Heading Change",Group2="Dispersion",Group3="Mean",Value=rstan::extract(m1,pars="MuA_D")$MuA_D)#  Mean Dispersion in Angle Change 
DispA_D <-data.frame(Group="Heading Change",Group2="Dispersion",Group3="Dispersion",Value=rstan::extract(m1,pars="DispA_D")$DispA_D)# Dispersion in Dispersion in Angle Change

df_allx<-rbind(MuD_M,DispD_M,MuD_D,DispD_D,MuA_M,DispA_M,MuA_D,DispA_D)

 ggplot(df_allx, aes(x=Value)) + geom_density(aes(group=Group3, colour=Group3, fill=Group3), alpha=0.3)   + facet_wrap(Group2~Group,scales="free")+
 labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) 
}
