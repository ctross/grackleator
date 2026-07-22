extract_autocorrelation_parameter = function(m2){
df2 = data.frame(Parameter="B", Value=rstan::extract(m2,pars="B")$B) # attraction score in (0,1) to 1-lag locations


 ggplot(df2, aes(x=Value)) + geom_density(aes(fill=Parameter), alpha=0.3) +
 theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) 
}

