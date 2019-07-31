grackletrip <- function(m1){
 m1a<-rstan::extract(m1,pars="AlphaAngle")
 sample_eff<-apply(m1a$AlphaAngle,2,quantile,probs=c(0.05,0.5,0.95))
 df_angle<-data.frame(Trip=c(1:dim(m1a$AlphaAngle)[2]),Group="Heading Change",Group2="Mean",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 m1d<-rstan::extract(m1,pars="AlphaDist")
 sample_eff<-apply(m1d$AlphaDist,2,quantile,probs=c(0.05,0.5,0.95))
 df_dist<-data.frame(Trip=c(1:dim(m1d$AlphaDist)[2]),Group="Step-Size",Group2="Mean",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 df_all1<-rbind(df_angle,df_dist)

  m1a<-rstan::extract(m1,pars="DAngle")
 sample_eff<-apply(exp(m1a$DAngle),2,quantile,probs=c(0.05,0.5,0.95))
 df_angle<-data.frame(Trip=c(1:dim(m1a$DAngle)[2]),Group="Heading Change",Group2="Dispersion",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 m1d<-rstan::extract(m1,pars="SDDist")
 sample_eff<-apply(exp(m1d$SDDist),2,quantile,probs=c(0.05,0.5,0.95))
 df_dist<-data.frame(Trip=c(1:dim(m1d$SDDist)[2]),Group="Step-Size",Group2="Dispersion",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
 df_all2<-rbind(df_angle,df_dist)

 df_all<-rbind(df_all1,df_all2)

 ggplot(df_all,aes(x=Trip,y=Median))+geom_point()+
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(Group2~Group,scales="free")+
 labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
 scale_x_continuous(breaks= pretty_breaks())
}
